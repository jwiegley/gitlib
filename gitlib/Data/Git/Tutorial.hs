{-| This module provides a brief introductory tutorial in the \"Introduction\"
    section followed by a lengthy discussion of the library's design and idioms.
-}

module Data.Git.Tutorial
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

   Every use of @gitlib@ must begin with a 'Data.Git.Repository' object.  At
   the moment each 'Repository' must be associated with a local directory,
   even if the Git objects are kept elsewhere via a custom backend (see
   <https://github.com/libgit2/libgit2/issues/1213>).

   If no 'Repository' exists yet, use 'Data.Git.Repository.createRepository';
   if one does exist, use 'Data.Git.Repository.openRepository'; or, you can
   use 'Data.Git.Repository.openOrCreateRepository'.  For example:

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

   This pattern is rather common, so there is a shortcut called
   'Data.Git.Commit.lookupRefCommit'.  Or, if you have a SHA string, you can
   use 'Data.Git.Commit.lookupCommit' with 'Data.Git.Oid.parseOid'.

> repo          <- openOrCreateRepository path False
> commitFromRef <- lookupRefCommit repo "HEAD"             :: Maybe Commit
> commitFromSHA <- lookupCommit repo (parseOid "f7acdbed") :: Maybe Commit

   1. Read a reference to obtain a commit id, such as @HEAD@ or
      @refs\/heads\/master@.

   2. Load a 'Data.Git.Commit.Commit', and thereafter its history through its
      parents, or load a 'Data.Git.Tree.Tree' or 'Data.Git.Blob.Blob' from
      its contents.

   3. Construct a new commit
-}