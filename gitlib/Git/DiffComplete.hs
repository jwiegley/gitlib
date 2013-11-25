module Git.Diff where

-- import           Control.Applicative
-- import           Control.Monad
-- import           Control.Monad.Trans.Class
import           Data.ByteString as B
-- import           Data.Conduit
-- import qualified Data.Conduit.List as CList
-- import           Data.HashSet (HashSet)
-- import qualified Data.HashSet as HashSet
-- import           Data.Tagged
-- import           Data.Text as T
-- import           Data.Text.Encoding as T
import           Git.Types

data DiffOptionFlag
    = DONormal                  -- ^ Normal diff, the default
    | DOReverse                 -- ^ Reverse the sides of the diff
    | DOForceText               -- ^ Treat all files as text, disabling binary
                                --   attributes & detection
    | DOIgnoreWhitespace        -- ^ Ignore all whitespace
    | DOIgnoreWhitespaceChange  -- ^ Ignore changes in amount of whitespace
    | DOIgnoreWhitespaceEol     -- ^ Ignore whitespace at end of line
    | DOIgnoreSubmodules        -- ^ Exclude submodules from the diff completely
    | DOPatience                -- ^ Use the "patience diff" algorithm
                                --   (currently unimplemented)
    | DOIncludeIgnored          -- ^ Include ignored files in the diff list
    | DOIncludeUntracked        -- ^ Include untracked files in the diff list
    | DOIncludeUnmodified       -- ^ Include unmodified files in the diff list
    | DORecurseUntrackedDirs    -- ^ Even with DOIncludeUntracked, an entire
                                --   untracked directory will be marked with
                                --   only a single entry in the diff list;
                                --   this flag adds all files under the
                                --   directory as UNTRACKED entries, too.
    | DODisablePathspecMatch    -- ^ If the pathspec is set in the diff
                                --   options, this flags means to apply it as
                                --   an exact match instead of as an fnmatch
                                --   pattern.
    | DODeltasAreIcase          -- ^ Use case insensitive filename comparisons
    | DOIncludeUntrackedContent -- ^ When generating patch text, include the
                                --   content of untracked files
    | DOSkipBinaryCheck         -- ^ Disable updating of the binary flag in
                                --   delta records.  This is useful when
                                --   iterating over a diff if you don't need
                                --   hunk and data callbacks and want to avoid
                                --   having to load file completely.
    | DOIncludeTypechange       -- ^ Normally, a type change between files
                                --   will be converted into a DELETED record
                                --   for the old and an ADDED record for the
                                --   new; this options enabled the generation
                                --   of TYPECHANGE delta records.
    | DOIncludeTypechangeTrees  -- ^ Even with DOIncludeTypechange, tree
                                --   changes still generally show as a DELETED
                                --   blob.  This flag tries to correctly label
                                --   blob->tree transitions as TYPECHANGE
                                --   records with newFile's mode set to tree.
                                --   Note: the tree SHA will not be available.
    | DOIgnoreFilemode          -- ^ Ignore file mode changes
    | DORecurseIgnoredDirs      -- ^ Even with DOIncludeIgnored, an entire
                                --   ignored directory will be marked with
                                --   only a single entry in the diff list;
                                --   this flag adds all files under the
                                --   directory as IGNORED entries, too.

data DiffFlag = DFBinary        -- ^ File(s) treated as binary data
              | DFNotBinary     -- ^ File(s) treated as text data
              | DFFlagValidOid  -- ^ Oid value is known correct

data DiffDeltaKind
    = DDKUnmodified             -- ^ No changes
    | DDKAdded                  -- ^ Entry does not exist in old version
    | DDKDeleted                -- ^ Entry does not exist in new version
    | DDKModified               -- ^ Entry content changed between old and new
    | DDKRenamed                -- ^ Entry was renamed between old and new
    | DDKCopied                 -- ^ Entry was copied from another old entry
    | DDKIgnored                -- ^ Entry is ignored item in workdir
    | DDKUntracked              -- ^ Entry is untracked item in workdir
    | DDKTypeChange             -- ^ Type of entry changed between old and new

data DiffFile m = DiffFile
    { diffFileOid   :: Oid m
    , diffFilePath  :: FilePath
    , diffFileSize  :: Int
    , diffFileFlags :: Int
    , diffFileMode  :: Int
    }

data DiffDelta m = DiffDelta
    { diffDeltaOldFile    :: DiffFile m
    , diffDeltaNewFile    :: DiffFile m
    , diffDeltaStatus     :: DiffDeltaKind
    , diffDeltaSimilarity :: Int
    , diffDeltaFlags      :: Int
    }

data DiffList (m :: * -> *)
data DiffPatch (m :: * -> *)

newtype DiffNotifyCallback m
    = DiffNotifyCallback (DiffList m -> DiffDelta m -> String -> m ())

data DiffOptions m = DiffOptions
    { diffOptionsVersion        :: Int
    , diffOptionsFlags          :: [DiffOptionFlag]
    , diffOptionsContextLines   :: Int
    , diffOptionsInterHunkLines :: Int
    , diffOptionsOldPrefix      :: String
    , diffOptionsNewPrefix      :: String
    , diffOptionsPathspec       :: [String]
    , diffOptionsMaxSize        :: Int
    , diffOptionsNotifyCallback :: DiffNotifyCallback m
    }

newtype DiffFileCallback m = DiffFileCallback (DiffDelta m -> Float -> m ())

data DiffRange = DiffRange
    { diffRangeOldStart :: Int
    , diffRangeOldLines :: Int
    , diffRangeNewStart :: Int
    , diffRangeNewLines :: Int
    }

newtype DiffHunkCallback m
    = DiffHunkCallback (DiffDelta m -> DiffDelta m -> String -> Int -> m ())

newtype DiffDataCallback m
    = DiffDataCallback (DiffDelta m -> DiffRange -> Char -> String -> Int
                        -> m ())

data DiffFind
    = DFRenames                 -- ^ Look for renames? (--find-renames)
    | DFRenamesFromRewrites     -- ^ Consider old side of modified for
                                --   renames? (--break-rewrites=N)
    | DFCopies                  -- ^ Look for copies? (a la --find-copies)
    | DFCopiesFromUnmodified    -- ^ Consider unmodified as copy sources?
                                --   (--find-copies-harder)
    | DFAndBreakRewrites        -- ^ Split large rewrites into delete/add
                                --   pairs (--break-rewrites=/M)
    | DFAll                     -- ^ Turn on all finding features
    | DFIgnoreLeadingWhitespace -- ^ Measure similarity ignoring leading
                                --   whitespace (default)
    | DFIgnoreWhitespace        -- ^ Measure similarity ignoring all whitespace
    | DFDontIgnoreWhitespace    -- ^ Measure similarity including all data

data DiffFindOptions = DiffFindOptions
    { diffFindOptionsVersion                    :: Int
    , diffFindOptionsFlags                      :: Int
    , diffFindOptionsRenameThreshold            :: Int
    , diffFindOptionsRenameFromRewriteThreshold :: Int
    , diffFindOptionsCopyThreshold              :: Int
    , diffFindOptionsBreakRewriteThreshold      :: Int
    , diffFindOptionsTargetLimit                :: Int
    }

diffBlobToBuffer :: Blob m       -- ^ Old blob
                 -> ByteString   -- ^ Buffer
                 -> DiffOptions m
                 -> DiffFileCallback m
                 -> DiffHunkCallback m
                 -> DiffDataCallback m
                 -> m ()
diffBlobToBuffer = undefined

diffBlobs :: Blob m              -- ^ Old blob
          -> Blob m              -- ^ New blob
          -> DiffOptions m       -- ^ Buffer
          -> DiffFileCallback m
          -> DiffHunkCallback m
          -> DiffDataCallback m
          -> m ()
diffBlobs = undefined

diffFindSimilar :: DiffList m -> DiffFindOptions -> m (DiffList m)
diffFindSimilar = undefined

diffForeach :: DiffList m
            -> DiffFileCallback m
            -> DiffHunkCallback m
            -> DiffDataCallback m
            -> m ()
diffForeach = undefined

diffGetPatch :: DiffDelta m -> DiffList m -> Int -> m (DiffPatch m)
diffGetPatch = undefined

-- diffIndexToWorkdir :: DiffList m -> Index m -> DiffOptions m -> m ()
-- diffIndexToWorkdir = undefined

diffMerge :: DiffList m          -- ^ Onto
          -> DiffList m          -- ^ From
          -> m (DiffList m)
diffMerge = undefined

diffNumDeltas :: DiffList m -> m Int
diffNumDeltas = undefined

diffNumDeltasOfType :: DiffList m -> DiffDeltaKind -> m Int
diffNumDeltasOfType = undefined

diffPatchDelta :: DiffPatch m -> m (DiffDelta m)
diffPatchDelta = undefined

data DiffPatchHunk = DiffPatchHunk
    { diffHunkRange       :: DiffRange m
    , diffHunkHeader      :: String
    , diffHunkHeaderLen   :: Int
    , diffHunkLinesInHunk :: Int
    }

diffPatchGetHunk :: DiffPatch m -> Int -> m (DiffPatchHunk m)
diffPatchGetHunk = undefined

diffPatchGetLineInHunk :: ()
diffPatchGetLineInHunk = undefined

diffPatchLineStats :: ()
diffPatchLineStats = undefined

diffPatchNumHunks :: ()
diffPatchNumHunks = undefined

diffPatchNumLinesInHunk :: ()
diffPatchNumLinesInHunk = undefined

diffPatchPrint :: ()
diffPatchPrint = undefined

diffPatchToStr :: ()
diffPatchToStr = undefined

diffPrintCompact :: ()
diffPrintCompact = undefined

diffPrintPatch :: ()
diffPrintPatch = undefined

diffStatusChar :: ()
diffStatusChar = undefined

diffTreeToTree :: ()
diffTreeToTree = undefined

diffTreeToWorkdir :: ()
diffTreeToWorkdir = undefined

-- diffTreeToindex :: ()
-- diffTreeToindex = undefined
