{- Git.Hit.Internal — duplicates of unexported functions in
   Data.Git.Named(hit). -}

{-# LANGUAGE OverloadedStrings #-}

module Git.Hit.Internal
       ( toRefTy
       , fromRefTy
       , toPath
       ) where


import Data.Git.Named
import Data.Git.Revision (fromString)
import Data.List (isPrefixOf)
import Filesystem.Path.CurrentOS (FilePath, (</>))
import Prelude hiding (FilePath)

toRefTy :: String -> RefSpecTy
toRefTy s
    | "refs/tags/" `isPrefixOf` s    = RefTag $ RefName $ drop 10 s
    | "refs/heads/" `isPrefixOf` s   = RefBranch $ RefName $ drop 11 s
    | "refs/remotes/" `isPrefixOf` s = RefRemote $ RefName $ drop 13 s
    | "refs/patches/" `isPrefixOf` s = RefPatches $ drop 13 s
    | "refs/stash" == s              = RefStash
    | "HEAD" == s                    = RefHead
    | "ORIG_HEAD" == s               = RefOrigHead
    | "FETCH_HEAD" == s              = RefFetchHead
    | otherwise                      = RefOther $ s

fromRefTy :: RefSpecTy -> String
fromRefTy spec = case spec of
    RefBranch h  -> "refs/heads/" ++ refNameRaw h
    RefTag h     -> "refs/tags/" ++ refNameRaw h
    RefRemote h  -> "refs/remotes/" ++ refNameRaw h
    RefPatches h -> "refs/patches/" ++ h
    RefStash     -> "refs/stash"
    RefHead      -> "HEAD"
    RefOrigHead  -> "ORIG_HEAD"
    RefFetchHead -> "FETCH_HEAD"
    RefOther h   -> h

toPath :: FilePath -> RefSpecTy -> FilePath
toPath repo spec = case spec of
    RefBranch h  -> repo </> "refs" </> "heads" </> fromString (refNameRaw h)
    RefTag h     -> repo </> "refs" </> "tags" </> fromString (refNameRaw h)
    RefRemote h  -> repo </> "refs" </> "remotes" </> fromString (refNameRaw h)
    RefPatches h -> repo </> "refs" </> "patches" </> fromString h
    RefStash     -> repo </> "refs" </> "stash"
    RefHead      -> repo </> "HEAD"
    RefOrigHead  -> repo </> "ORIG_HEAD"
    RefFetchHead -> repo </> "FETCH_HEAD"
    RefOther h   -> repo </> fromString h
