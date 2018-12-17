A type-safe interface to manipulating the objects in a Git repository.  The
actual work is done using [hlibgit2].  The main purpose of this library is
allow for a more functional style of interacting with Git objects.

[hlibgit2]: https://github.com/jwiegley/gitlib/tree/master/hlibgit2

Basic Work-flow
--------------

Unfortunately, [libgit2] is rather spotty on its documentation of how
to do typical things, like creating new commits containing actual
changes.  With `gitlib`, it's easier, since we can take a high-level
approach. Nevertheless, a tutorial is instructive.

[libgit2]: https://libgit2.github.com

### First steps

First things first&nbsp;&mdash; import the library

```haskell
{-# Language OverloadedStrings #-}

import Git
import Git.Libgit2
import Control.Monad.IO.Class
import Data.Tagged
import Data.Time
import qualified Data.Text as T
```

Now, we need to grab a reference to the repository itself. (This is
not a Git ref, but a plain old memory reference, which `libgit2` uses to
keep track of things.)

```haskell
main = do
    let repoOpts = RepositoryOptions { repoPath = "."
                                     , repoWorkingDir = Nothing
                                     , repoIsBare = False
                                     , repoAutoCreate = False
                                     }
    withRepository' lgFactory repoOpts $ do
```

You can see above that `withRepository'` is a versatile action, which
can handle a working directory separate from the `.git` directory, or a
bare repository; it can even automatically create a missing repository,
if you need that.

Note that we're using the `OverloadedStrings` language extension for
convenience's sake, so that we can enter the repository path via a
`String`.

### Git Technical Overview

Now, let's make a commit.
We want to add a new file to the repo (`README`), and then commit our
changes.  To make a commit, we'll need the following things:

1. A list of parents (e.g., more than one for a merge)
2. The updated tree
3. The author and committor [Signatures], i.e. name/email/time
4. The commit message

For the first one, we'll use `HEAD`. For the author/committor
signatures, we can easily invent one; for the commit message, again,
we'll invent it.

### Building the Tree

That leaves the new tree, which we'll have to build based on our current
tree at `HEAD` and our new file. But `git` doesn't know about files: it
knows about `blobs`, so we'll have to register our new file with `git`
first, to turn it into a [blob] object. Rather than modifying the
working directory directly (i.e. to save the file), we'll use an
in-memory `blob`, to keep things simple.

[blob]: https://github.com/jwiegley/gitlib/tree/master/gitlib/Git/Types.hs#L168
[Signatures]: https://github.com/jwiegley/gitlib/tree/master/gitlib/Git/Types.hs#L241

```haskell
        let contents :: T.Text
            contents = "Welcome to my shiny new project"
        blobID <- createBlobUtf8 contents
```

Notice the layers here. We start off with a `Text` string,
an abstract object which is not tied to any particular encoding.
(The fact that it maintains an internal encoding is irrelevant: as
long as it can re-encode its contents faithfully, it doesn't matter).

Then, we use the only [sane] [encoding] in the
known universe: `UTF-8` to create a blob and obtain a blob ID.

[sane]: http://utf8everywhere.org
[encoding]: http://htmlpurifier.org/docs/enduser-utf8.html#whyutf8

Finally, we're ready to make our awesome commit! Here it goes:

```haskell
        maybeObjID <- resolveReference "HEAD"
        case maybeObjID of
            Just commitID -> do
                headCommit <- lookupCommit (Tagged commitID)
                mutatedTreeId <- mutateTreeOid (commitTree headCommit) (putBlob "README" blobID)
                now <- liftIO getZonedTime
                let sig = Signature { signatureName = "Nobody"
                                    , signatureEmail = "nobody@example.com"
                                    , signatureWhen = now
                                    }
                newCommit <- createCommit [commitOid headCommit] mutatedTreeId sig sig "Commit message\n" Nothing
                updateReference "refs/heads/master" (RefObj (untag (commitOid newCommit)))
                pure ()
            _ ->
                liftIO (print "Couldn't resolve HEAD")
```

Note that it's a separate step to update our master branch with the
new commit.


Example Projects
-----------
https://github.com/nomeata/gipeda


MonadGit
--------
TODO: Explain the interface a bit, the reason for the phantom variable,
and how to construct an instance.

TreeBuilder
-----------
TODO: Explain how much cooler the State monad is (esp.
`queryTreeBuilder`, from a high-level perspective) than the
[TreeBuilder API] provided by `libgit2`.

[API]: http://libgit2.github.com/docs/guides/101-samples/#trees_treebuilder
