module Data.Git.Backend.S3 where

import Bindings.Libgit2.OdbBackend
import Bindings.Libgit2.Types
import Bindings.Libgit2.Oid
import Foreign.C.String
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Ptr
import Network.AWS.AWSConnection
import Network.AWS.AWSResult
import Network.AWS.S3Bucket

data GitS3Backend = GitS3Backend { parent :: ForeignPtr C'git_odb_backend
                                 , s3conn :: AWSConnection }

gitS3_backend__read_header ::
  Ptr CSize -> Ptr C'git_otype -> Ptr C'git_odb_backend -> Ptr C'git_oid
  -> IO CInt
gitS3_backend__read_header len_p type_p _backend oid = undefined
  -- gitS3_backend *backend;
  -- int error;
  -- redisReply *reply;

  -- assert(len_p && type_p && _backend && oid);

  -- backend = (gitS3_backend *) _backend;
  -- error = GIT_ERROR;

  -- reply = redisCommand(backend->db, "HMGET %b %s %s", oid->id, GIT_OID_RAWSZ,
  --         "type", "size");

  -- if (reply && reply->type == REDIS_REPLY_ARRAY) {
  --     if (reply->element[0]->type != REDIS_REPLY_NIL &&
  --             reply->element[0]->type != REDIS_REPLY_NIL) {
  --         *type_p = (git_otype) atoi(reply->element[0]->str);
  --         *len_p = (size_t) atoi(reply->element[1]->str);
  --         error = GIT_SUCCESS;
  --     } else {
  --         error = GIT_ENOTFOUND;
  --     }
  -- } else {
  --     error = GIT_ERROR;
  -- }

  -- freeReplyObject(reply);
  -- return error;

gitS3_backend__read ::
  Ptr (Ptr ()) -> Ptr CSize -> Ptr C'git_otype -> Ptr C'git_odb_backend
  -> Ptr C'git_oid -> IO CInt
gitS3_backend__read data_p len_p type_p _backend oid = undefined
  -- gitS3_backend *backend;
  -- int error;
  -- redisReply *reply;

  -- assert(data_p && len_p && type_p && _backend && oid);

  -- backend = (gitS3_backend *) _backend;
  -- error = GIT_ERROR;

  -- reply = redisCommand(backend->db, "HMGET %b %s %s %s", oid->id,
  --         GIT_OID_RAWSZ, "type", "size", "data");

  -- if (reply && reply->type == REDIS_REPLY_ARRAY) {
  --     if (reply->element[0]->type != REDIS_REPLY_NIL &&
  --             reply->element[1]->type != REDIS_REPLY_NIL &&
  --             reply->element[2]->type != REDIS_REPLY_NIL) {
  --         *type_p = (git_otype) atoi(reply->element[0]->str);
  --         *len_p = (size_t) atoi(reply->element[1]->str);
  --         *data_p = malloc(*len_p);
  --         if (*data_p == NULL) {
  --             error = GIT_ENOMEM;
  --         } else {
  --             memcpy(*data_p, reply->element[2]->str, *len_p);
  --             error = GIT_SUCCESS;
  --         }
  --     } else {
  --         error = GIT_ENOTFOUND;
  --     }
  -- } else {
  --     error = GIT_ERROR;
  -- }

  -- freeReplyObject(reply);
  -- return error == GIT_SUCCESS;

-- gitS3_backend_readstream_callback :: Ptr (Ptr <git_odb_stream>) -> Ptr <git_odb_backend> -> Ptr <git_oid> -> IO CInt

gitS3_backend__read_prefix ::
  Ptr C'git_oid -> Ptr (Ptr ()) -> Ptr CSize -> Ptr C'git_otype
  -> Ptr C'git_odb_backend -> Ptr C'git_oid -> CUInt -> IO CInt
gitS3_backend__read_prefix out_oid data_p len_p type_p _backend short_oid len = undefined
  -- if (len >= GIT_OID_HEXSZ) {
  --   /* Just match the full identifier */
  --   int error = gitS3_backend__read(data_p, len_p, type_p, _backend, short_oid);
  --   if (error == GIT_SUCCESS)
  --     git_oid_cpy(out_oid, short_oid);

  --   return error;
  -- } else if (len < GIT_OID_HEXSZ) {
  --   /* TODO */
  --   return GIT_ENOTIMPLEMENTED;
  -- }

gitS3_backend__exists :: Ptr C'git_odb_backend -> Ptr C'git_oid -> IO CInt
gitS3_backend__exists _backend oid = undefined
  -- gitS3_backend *backend;
  -- int found;
  -- redisReply *reply;

  -- assert(_backend && oid);

  -- backend = (gitS3_backend *) _backend;
  -- found = 0;

  -- reply = redisCommand(backend->db, "exists %b", oid->id, GIT_OID_RAWSZ);
  -- if (reply && reply->type != REDIS_REPLY_NIL && reply->type != REDIS_REPLY_ERROR)
  --     found = 1;

  -- freeReplyObject(reply);
  -- return found;

gitS3_backend__write ::
  Ptr C'git_oid -> Ptr C'git_odb_backend -> Ptr () -> CSize -> C'git_otype
  -> IO CInt
gitS3_backend__write oid _backend contents len objType = undefined
  -- gitS3_backend *backend;
  -- int error;
  -- redisReply *reply;

  -- assert(id && _backend && data);

  -- backend = (gitS3_backend *) _backend;
  -- error = GIT_ERROR;

  -- if ((error = git_odb_hash(id, data, len, type)) < 0)
  --     return error;

  -- reply = redisCommand(backend->db, "HMSET %b "
  --         "type %d "
  --         "size %d "
  --         "data %b ", id->id, GIT_OID_RAWSZ,
  --         (int) type, len, data, len);

  -- error = (reply == NULL || reply->type == REDIS_REPLY_ERROR) ? GIT_ERROR : GIT_SUCCESS;

  -- freeReplyObject(reply);
  -- return error;

-- gitS3_backend_writestream_callback :: Ptr (Ptr <git_odb_stream>) -> Ptr <git_odb_backend> -> CSize -> <git_otype> -> IO CInt

gitS3_backend__free :: Ptr C'git_odb_backend -> IO ()
gitS3_backend__free _backend = undefined
  -- gitS3_backend *backend;
  -- assert(_backend);
  -- backend = (gitS3_backend *) _backend;

  -- redisFree(backend->db);

  -- free(backend);

git_odb_backend_s3 :: String -> String -> String -> GitS3Backend
git_odb_backend_s3 accessKey secretAccessKey bucketName = undefined
  -- Ptr (Ptr C'git_odb_backend>)
  -- backend = calloc(1, sizeof (gitS3_backend));
  -- if (backend == NULL)
  --     return GIT_ENOMEM;

  -- backend->db = redisConnect(host, port);
  -- if (backend->db->err) {
  --         free(backend);
  --         return GIT_ERROR;
  --       }

  -- backend->parent.read = &gitS3_backend__read;
  -- backend->parent.read_prefix = &gitS3_backend__read_prefix;
  -- backend->parent.read_header = &gitS3_backend__read_header;
  -- backend->parent.write = &gitS3_backend__write;
  -- backend->parent.exists = &gitS3_backend__exists;
  -- backend->parent.free = &gitS3_backend__free;

  -- *backend_out = (git_odb_backend *) backend;

  -- return 0
