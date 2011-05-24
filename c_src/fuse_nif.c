#define FUSE_USE_VERSION 26
#define _FILE_OFFSET_BITS 64

#include <fuse.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <fcntl.h>
#include <erl_nif.h>

#define error_tuple(msg) enif_make_tuple(env, enif_make_atom(env, "error"), enif_make_string(env, msg, ERL_NIF_LATIN1)) 

static ERL_NIF_TERM mount(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  char path[1024];
  long fd;

  if (argc != 1)
    return error_tuple("Bad argument count");

  if (enif_get_string(env, argv[0], path, sizeof(path), ERL_NIF_LATIN1) <= 0)
    return error_tuple("Bad path");

  fd = (long)fuse_mount(path, NULL);

  return enif_make_tuple(env, enif_make_atom(env, "ok"), enif_make_long(env, fd));
}

static ErlNifFunc nif_funcs[] =
{
    {"mount", 1, mount}
};
ERL_NIF_INIT(fuse, nif_funcs, NULL,NULL,NULL,NULL)
