# CLAUDE.md - AI Assistant Guide for gitlib Development

This project is a multi-package Haskell library providing a high-level, backend-agnostic interface for Git operations. It uses tagless-final style with multiple backends (libgit2, command-line, pure Haskell).

## Development Commands

### Building
```bash
# Build all packages (from project root)
cabal build all

# Build specific package
cabal build gitlib
cabal build gitlib-libgit2
cabal build gitlib-cmdline
cabal build gitlib-test

# Using Nix
nix develop  # Enter development shell with all dependencies
nix build    # Build default package (git-monitor)
```

### Testing
```bash
# Run all tests
cabal test all

# Test specific backend
cabal test gitlib-libgit2
cabal test gitlib-cmdline

# Run with verbose output
cabal test all -v2

# Run specific test suite (example)
cabal test gitlib-test --test-option="--match=/Smoke/"

# Test a single spec
cabal test gitlib-test --test-option="--match=/Repository/can be created/"
```

### Development Workflow
```bash
# Enter REPL for exploration
cabal repl gitlib
cabal repl gitlib-libgit2

# Check for type errors without full rebuild
cabal build --ghc-options="-fno-code -fwrite-interface"

# Generate documentation
cabal haddock all

# Clean build artifacts
cabal clean

# Update dependencies
cabal update
cabal outdated  # Check for outdated dependencies
```

## High-Level Architecture

### Core Abstraction: Tagless-Final MonadGit

```haskell
-- The heart of the architecture
class (Applicative m, Monad m, MonadThrow m,
       IsOid (Oid r), Show (Oid r), Eq (Oid r), Ord (Oid r))
      => MonadGit r m | m -> r where
    type Oid r :: Type           -- Associated type family
    data Tree r :: Type          -- Data family for trees
    data Options r :: Type       -- Backend-specific options

    -- Operations defined as class methods
    lookupCommit :: CommitOid r -> m (Commit r)
    createTree :: TreeT r m () -> m (Tree r)
    -- ... more operations
```

**Key Insight**: The `r` parameter identifies the backend, `m` is the monad stack. Functional dependency `m -> r` ensures type inference works.

## Key Architecture Patterns

### 1. Type Families vs Data Families

```haskell
type Oid r :: Type      -- Type family: abstract, no pattern matching
data Tree r :: Type     -- Data family: concrete, allows pattern matching
```

**Rule**: Use data families when you need to pattern match on constructors (like `LgTree`), type families for abstract types.

### 2. Tagged Types for Safety

```haskell
type BlobOid r   = Tagged r (Oid r)           -- Tagged with backend
type TreeOid r   = Tagged (Tree r) (Oid r)    -- Tagged with object type
```

**Purpose**: Prevents mixing OIDs from different backends or object types at compile time.

### 3. ForeignPtr Resource Management

```haskell
data OidPtr = OidPtr
    { getOid    :: ForeignPtr C'git_oid  -- Automatic cleanup
    , getOidLen :: Int                    -- Length tracking
    }
```

**CRITICAL**: Always use `ForeignPtr` for C resources, never raw `Ptr`. Attach finalizers immediately:

```haskell
ptr <- c'git_repository_open ...
fptr <- newForeignPtr ptr c'git_repository_free  -- Attach finalizer
```

### 4. Conduit Streaming Pattern

```haskell
sourceTreeEntries :: Bool -> Tree r -> ConduitT i (TreeFilePath, TreeEntry r) m ()
```

**Rule**: ALL collection operations must return conduits for lazy evaluation and constant memory usage.

### 5. Exception Dual System

```haskell
data LgRepo = LgRepo
    { ...
    , repoExcTrap :: IORef (Maybe Git.GitException)  -- C callback exceptions
    }
```

**Warning**: C callbacks can't throw Haskell exceptions. Check `repoExcTrap` after C operations!

## Adding New Git Operations

To add a new Git operation (e.g., stash support), follow this workflow:

1. **Define types** in `/Users/johnw/src/gitlib/gitlib/Git/Types.hs`
2. **Extend MonadGit class** with the new operation
3. **Implement in libgit2 backend** first (primary backend)
4. **Add tests** to `/Users/johnw/src/gitlib/gitlib-test/Git/Smoke.hs`
5. **Implement in other backends** or mark as TODO

Example workflow for adding a new operation:

```haskell
-- 1. In Git/Types.hs - define types
data Stash r = Stash { stashMessage :: Text, stashCommit :: CommitOid r }
type StashOid r = Tagged (Stash r) (Oid r)

-- 2. In MonadGit class - add operation
class MonadGit r m where
    saveStash :: Text -> m (StashOid r)
    listStashes :: ConduitT i (StashOid r, Text) m ()  -- Collections must stream!

-- 3. In libgit2 backend - implement
instance MonadGit LgRepo m where
    saveStash msg = do
        repo <- getRepository
        withForeignPtr (repoObj repo) $ \repoPtr -> do
            -- Call C function with proper error handling
            checkResult "git_stash_save" $ c'git_stash_save ...

-- 4. In gitlib-test - add tests
stashSpec :: MonadGit r m => RepositoryFactory m IO r -> Spec
stashSpec factory = describe "Stash" $ do
    it "saves stash" $ withNewRepository factory $ do
        -- Test implementation
```

## Backend-Specific Guidance

### libgit2 Backend (Primary)

**Key Files**:
- `/Users/johnw/src/gitlib/gitlib-libgit2/Git/Libgit2.hs` - Main implementation
- `/Users/johnw/src/gitlib/gitlib-libgit2/Git/Libgit2/Types.hs` - Type definitions

**Critical Patterns**:

1. **Resource Acquisition**:
```haskell
withRepository :: MonadUnliftIO m => FilePath -> (LgRepo -> m a) -> m a
withRepository path action = bracket
    (openRepository path)
    closeRepository
    action
```

2. **Error Checking**:
```haskell
checkResult :: (MonadIO m, MonadExcept m) => Text -> IO CInt -> m ()
checkResult name action = do
    result <- liftIO action
    when (result < 0) $ lgThrow (BackendError name)
```

3. **Exception Trap for Callbacks**:
```haskell
-- Before calling C function with callback
liftIO $ writeIORef (repoExcTrap repo) Nothing

-- In callback (runs in C context)
catch callbackAction $ \e ->
    writeIORef excTrap (Just e)

-- After C function returns
mexc <- liftIO $ readIORef (repoExcTrap repo)
maybe (return ()) throwM mexc
```

### Command-Line Backend

**Key Pattern**: Process invocation with output parsing

```haskell
git :: [String] -> m ByteString
git args = do
    (exitCode, stdout, stderr) <- readProcessWithExitCode "git" args ""
    case exitCode of
        ExitSuccess -> return stdout
        ExitFailure n -> throwM $ GitError $
            "git " <> unwords args <> " failed: " <> stderr
```

**Warning**: This backend is less efficient and less complete. Use primarily for testing.

## Testing Strategy

The test suite (`gitlib-test`) validates backend compliance using polymorphic tests that work with any `MonadGit` implementation:

```haskell
-- From gitlib-test/Git/Smoke.hs
smokeTestSpec :: (MonadGit r m, MonadUnliftIO m, MonadThrow m, MonadFail m,
                  MonadGit s n, MonadUnliftIO n, MonadThrow n)
              => RepositoryFactory m IO r
              -> RepositoryFactory n m s
              -> Spec
```

This polymorphic approach ensures all backends implement the same semantics.

## Memory Management Patterns

### ForeignPtr Pattern for C Resources

All C resources MUST be wrapped in ForeignPtr with finalizers:

```haskell
-- Correct pattern
ptr <- c'git_repository_open ...
fptr <- newForeignPtr ptr c'git_repository_free  -- Attach finalizer immediately
```

### Streaming Pattern for Collections

All collection operations return conduits to maintain constant memory usage:

```haskell
sourceTreeEntries :: Tree r -> ConduitT i (TreeFilePath, TreeEntry r) m ()
sourceCommits :: ConduitT i (Commit r) m ()
```

### Async Pattern

```haskell
-- STM queue pattern for producer-consumer
gatherFrom' :: Int -> (TBQueue o -> m ()) -> ConduitT i o m ()
gatherFrom' queueSize producer = do
    queue <- liftIO $ newTBQueueIO queueSize
    worker <- lift $ async (producer queue)

    -- Consumer loop
    let loop = do
            items <- liftIO $ atomically $
                flushTBQueue queue
            if null items
                then do
                    done <- lift $ poll worker
                    case done of
                        Nothing -> loop
                        Just (Left e) -> throwM e
                        Just (Right _) -> return ()
                else mapM_ yield items >> loop
    loop
```

## Critical Implementation Details

### Exception Trap for C Callbacks

C callbacks cannot throw Haskell exceptions directly. The library uses an IORef trap pattern:

```haskell
-- From LgRepo type
data LgRepo = LgRepo
    { repoObj     :: ForeignPtr C'git_repository
    , repoExcTrap :: IORef (Maybe Git.GitException)  -- Stores exceptions from callbacks
    }

-- Pattern for C operations with callbacks
result <- c'git_operation_with_callback ptr callback
mexc <- liftIO $ readIORef (repoExcTrap repo)
maybe (return ()) throwM mexc  -- Re-throw if callback had exception
```

### STM Queue Pattern for Async Operations

The library uses STM queues for producer-consumer patterns:

```haskell
gatherFrom' :: Int -> (TBQueue o -> m ()) -> ConduitT i o m ()
gatherFrom' queueSize producer = do
    queue <- liftIO $ newTBQueueIO queueSize
    worker <- lift $ async (producer queue)
    -- Consumer loop with exception propagation
    consumeQueue worker queue
```


## Common Code Patterns

### Pattern 1: Adding MonadGit Operation

```haskell
-- Step 1: Types.hs
class MonadGit r m where
    myOperation :: TreeOid r -> m (Maybe (Blob r m))

    default myOperation :: (MonadTrans t, MonadGit r m1, t m1 ~ m)
                        => TreeOid r -> m (Maybe (Blob r m))
    myOperation = lift . myOperation

-- Step 2: Libgit2.hs
instance MonadGit LgRepo m where
    myOperation treeOid = do
        repo <- getRepository
        result <- liftIO $ withForeignPtr (repoObj repo) $ \ptr -> do
            -- C implementation
            ...
        case result of
            0 -> return Nothing
            _ -> Just <$> constructBlob result
```

### Pattern 2: Conduit Source

```haskell
sourceGitObjects :: MonadGit r m => ConduitT i (Object r) m ()
sourceGitObjects = do
    repo <- lift getRepository

    -- Use gatherFrom' for async production
    gatherFrom' 100 $ \queue -> do
        -- Producer: fills queue asynchronously
        forM_ allObjectOids $ \oid -> do
            obj <- lookupObject oid
            atomically $ writeTBQueue queue obj
        atomically $ closeTBQueue queue
```

### Pattern 3: Tree Modification

```haskell
modifyTree :: MonadGit r m => TreeOid r -> m (TreeOid r)
modifyTree baseTree = buildTree $ do
    -- Start from existing tree
    fromTree baseTree

    -- Make modifications
    putBlob "new-file.txt" "content"
    dropEntry "old-file.txt"

    -- Recursive modification
    modifySubtree "src" $ do
        putBlob "main.hs" "new content"
```

## Advanced Topics

### Custom Object Database Backend

```haskell
data CustomBackend = CustomBackend
    { backendRead  :: Oid r -> IO (Maybe ByteString)
    , backendWrite :: ByteString -> IO (Oid r)
    }

installCustomBackend :: CustomBackend -> LgRepo -> m ()
installCustomBackend backend repo = do
    odbPtr <- liftIO $ c'git_repository_odb (repoObj repo)
    backendPtr <- createOdbBackend backend
    liftIO $ c'git_odb_add_backend odbPtr backendPtr 1
```

### Implementing Worktree Support

```haskell
-- Requires extending MonadGit
class MonadGit r m where
    -- ... existing methods ...

    addWorktree :: FilePath -> Text -> m (Worktree r)
    listWorktrees :: ConduitT i (Worktree r) m ()
    pruneWorktrees :: m ()
```

### Performance Profiling

```haskell
-- Time individual operations
timeOperation :: MonadIO m => Text -> m a -> m a
timeOperation name action = do
    start <- liftIO getCurrentTime
    result <- action
    end <- liftIO getCurrentTime
    liftIO $ putStrLn $ name <> ": " <> show (diffUTCTime end start)
    return result

-- Memory profiling points
{-# SCC "tree_building" #-}
buildLargeTree :: MonadGit r m => m (Tree r)
```


