A type-safe interface to manipulating the objects in a Git repository.  The
actual work is done using `hlibgit2`.  The main purpose of this library is
allow for a more functional style of interacting with Git objects.

Basic Work-flow
--------------
TODO: Make the example actually work.

Unfortunately, [libgit2] is rather spotty on its documentation of how to
do typical things, like creating new commits containing actual changes.
With `gitlib`, it's even easier, since we can take a high-level
approach. Nevertheless, a tutorial is instructive.

[libgit2]: https://libgit2.github.com

### First steps

First things first&nbsp;&mdash; import the library

    ```haskell
    {-# Language OverloadedStrings #-}

    import Git
    import Data.ByteString (ByteString)
    import Data.Maybe
    import Data.Text.Encoding (encodeUtf8)
    import qualified Data.Text as T
    ```

Now, we need to grab a reference to the repository itself. (This is
not a Git ref, but a plain old memory reference, which `libgit2` uses to
keep track of things.)

```haskell
    main = do
        let repoOpts = RepositoryOptions { repoPath = "."
                                         , repoWorkingDir = Nothnig
                                         , reoIsPare = False
                                         , repoAutoCreate = False
                                         }
        repo <- openRepository repoOpts False
```

You can see above that `openRepository` is a versatile action, which
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
[Signatures]: https://github.com/jwiegley/gitlib/tree/master/gitlib/Git/Types.hs#L253

    ```haskell
        let contents :: T.Text
            contents = "Welcome to my shiney new project"

            bytes :: ByteString
            bytes = encodeUtf8 contents

            buffer :: BlobContents
            buffer = BlobString bytes
    ```

    Notice the layers here. First, we start off with a `Text` string,
    an abstract object which is not tied to any particular encoding.
    (The fact that it maintains an internal encoding is irrelevant: as
    long as it can re-encode its contents faithfully, it doesn't matter).

    Then, we have a `ByteString`, holding the raw bitstream of the
    would-be file. For this, we use the only [sane] [encoding] in the
    known universe: `UTF-8`. Finally, we wrap up our `ByteString` in a
    `BlobContents`, which has several constructors with various
    perfomance characteristics.

[sane]: http://utf8everywhere.org
[encoding]: http://htmlpurifier.org/docs/enduser-utf8.html#whyutf8

    Now, we register our new blob in the repository, and add in into the
    new `tree`:

    ```haskell
        blobID <- createBlob buffer
        putEntry "README" BlobEntry { blobEntryOid = blobID
                                    , blobEntryKind = PlainBlob
                                    }
    ```

    Since this is the only file we're changing in the `tree`, we go
    ahead and write it out (i.e. regisiter it in the repository)

    ```haskell
        (_, tree) <- writeTrueeBuilder =<< getBuilder
    ```

    Finally, we're ready to make our awesome commit! We'll go ahead and
    Here it goes:

    ```haskell
        now <- getCurrentTime
        let sig = Signature { signatureName = "Nobody"
                            , signatureEmail = "nobody@example.com"
                            , signatureWhen = now
                            }
            commitMessage :: T.Text
            commitMessage = "Added new README\n\nIt's about time!"

        mRef <- lookupReference "HEAD"
        let ref = fromMaybe (error "Invalid ref: HEAD") mRef

        mCid <- referenceToOid ref
        let head = fromMaybe (error "Something bad happened") mCid

        commit <- createCommit [head] tree sig sig commitMessage
    ```

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
