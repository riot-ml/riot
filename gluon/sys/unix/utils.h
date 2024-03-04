#ifndef __GLUON_SYS_UNIX_UTILS_H__
#define __GLUON_SYS_UNIX_UTILS_H__

#include <caml/alloc.h>

typedef value (*ocaml_alloc_first_arg_fn) (char const *);
typedef char const * const * (ocaml_alloc_second_arg);

#endif // __GLUON_SYS_UNIX_UTILS_H__