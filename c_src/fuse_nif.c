#define FUSE_USE_VERSION 26
#define _FILE_OFFSET_BITS 64

#include <fuse.h>
#include <fuse_lowlevel.h>

#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <fcntl.h>
#include <erl_nif.h>

#define error_tuple(msg) enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_string(env, msg, ERL_NIF_LATIN1)) 

static ERL_NIF_TERM mount(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  char path[1024];
  struct fuse_chan *ch;
  int fd;

  if (argc != 1)
    return error_tuple("Bad argument count");

  if (enif_get_string(env, argv[0], path, sizeof(path), ERL_NIF_LATIN1) <= 0)
    return error_tuple("Bad path");

  char *defarg[2] = {"efuse", NULL};
  struct fuse_args fargs = FUSE_ARGS_INIT(1, defarg);

  ch = fuse_mount(path, &fargs);
  if (!ch) {
    return error_tuple("Could not mount filesystem");
  }

  fd = fuse_chan_fd(ch);
  return enif_make_tuple2(env, enif_make_atom(env, "ok"), enif_make_int(env, fd));
}

static ErlNifFunc nif_funcs[] =
{
    {"mount", 1, mount}
};
ERL_NIF_INIT(fuse, nif_funcs, NULL,NULL,NULL,NULL)
