{-| This module provides a brief introductory tutorial in the \"Introduction\"
    section followed by a lengthy discussion of the library's design and idioms.
-}

module Git.Tutorial
       (
         -- * Introduction
         -- $intro

         -- * Repositories
         -- $repositories

         -- * References
         -- $references

         -- * Commits
         -- $commits
       ) where

{- $intro

   The @gitlib@ library provides high-level types for working with the
   @libgit2@ C library (<http://libgit2.github.com>).  The intention is to
   make @libgit2@ easier and more type-safe, while using laziness to avoid
   unnecessary work.
-}

{- $repositories

   Every use of @gitlib@ must begin with a 'Git.Libgit2.Repository' object.  At
   the moment each 'Repository' must be associated with a local directory,
   even if the Git objects are kept elsewhere via a custom backend (see
   <https://github.com/libgit2/libgit2/issues/1213>).

   If no 'Repository' exists yet, use 'Git.Libgit2.Repository.createRepository';
   if one does exist, use 'Git.Libgit2.Repository.openRepository'; or, you can
   use 'Git.Libgit2.Repository.openOrCreateRepository'.  For example:

> repo <- openOrCreateRepository path False -- False here means "not bare"
> ... make use of the repository ...

   Note that the 'path' variable here is of type 'Filesystem.Path.FilePath',
   since @gitlib@ almost never uses the 'String' type.
-}

{- $references

   If you are working with an existing repository, probably the first thing
   you'll want to do is resolve a reference so that you can lookup a commit:

> repo   <- openOrCreateRepository path False
> ref    <- resolveRef repo "HEAD"
> commit <- maybe (return Nothing) (lookupCommit repo) ref

   'resolveRef' works for both symbolic and specific refs.  Further, this
   pattern is rather common, so there is a shortcut called
   'Git.Libgit2.Commit.lookupRefCommit'.  Or, if you have a SHA string, you can
   use 'Git.Libgit2.Commit.lookupCommit' with 'Git.Libgit2.Oid.parseOid'.

> repo          <- openOrCreateRepository path False
> commitFromRef <- lookupRefCommit repo "HEAD"             :: Maybe Commit
> commitFromSHA <- lookupCommit repo (parseOid "f7acdbed") :: Maybe Commit

-}

{- $commits

   If you don't have a commit object, the recommend way to create one is by
   creating a 'Git.Libgit2.Common.Signature' and using it to modify the return
   value from 'Git.Libgit2.Commit.create'.  This requires a 'Repository' object:

> now <- getCurrentTime
> let sig = Signature {
>               signatureName  = "John Smith"
>             , signatureEmail = "johnsmith@nowhere.org"
>             , signatureWhen  = now }
>     c   = (createCommit repo) {
>             , commitAuthor    = sig
>             , commitCommitter = sig }
>             

   Load a 'Git.Libgit2.Commit.Commit', and thereafter its history through its
      parents, or load a 'Git.Libgit2.Tree.Tree' or 'Git.Libgit2.Blob.Blob' from
      its contents.

   3. Construct a new commit
-}