#include "erl_nif.h"

static ERL_NIF_TERM mount(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return enif_make_string(env, "Hello world!", ERL_NIF_LATIN1);
}
static ErlNifFunc nif_funcs[] =
{
    {"mount", 0, mount}
};
ERL_NIF_INIT(fuse,nif_funcs,NULL,NULL,NULL,NULL)
