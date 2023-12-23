/*

  This code taken from Eio's eio_posix_stub.c

*/

#include <caml/memory.h>
#include <caml/unixsupport.h>
#include <caml/bigarray.h>
#include <sys/uio.h>

/* Fill [iov] with pointers to the cstructs in the array [v_bufs]. */
static void fill_iov(struct iovec *iov, value v_bufs) {
  int n_bufs = Wosize_val(v_bufs);
  for (int i = 0; i < n_bufs; i++) {
    value v_cs = Field(v_bufs, i);
    value v_ba = Field(v_cs, 0);
    value v_off = Field(v_cs, 1);
    value v_len = Field(v_cs, 2);
    iov[i].iov_base = (uint8_t *)Caml_ba_data_val(v_ba) + Long_val(v_off);
    iov[i].iov_len = Long_val(v_len);
  }
}

CAMLprim value caml_riot_posix_readv(value v_fd, value v_bufs) {
  CAMLparam1(v_bufs);
  ssize_t r;
  int n_bufs = Wosize_val(v_bufs);
  struct iovec iov[n_bufs];

  fill_iov(iov, v_bufs);

  r = readv(Int_val(v_fd), iov, n_bufs);
  if (r < 0) uerror("readv", Nothing);

  CAMLreturn(Val_long(r));
}

CAMLprim value caml_riot_posix_writev(value v_fd, value v_bufs) {
  CAMLparam1(v_bufs);
  ssize_t r;
  int n_bufs = Wosize_val(v_bufs);
  struct iovec iov[n_bufs];

  fill_iov(iov, v_bufs);

  r = writev(Int_val(v_fd), iov, n_bufs);
  if (r < 0) uerror("writev", Nothing);

  CAMLreturn(Val_long(r));
}

