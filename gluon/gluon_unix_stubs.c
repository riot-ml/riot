#include <caml/alloc.h>
#include <caml/bigarray.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/unixsupport.h>
#include <sys/socket.h>
#include <sys/time.h>
#include <sys/types.h>
#include <sys/uio.h>
#include <time.h>


CAMLprim value caml_riot_posix_sendfile(value v_fd, value v_s, value v_offset, value v_len) {
  CAMLparam4(v_fd, v_s, v_offset, v_len);
  int fd = Int_val(v_fd);
  int s = Int_val(v_s);
  off_t offset = Int_val(v_offset);
  off_t len = Int_val(v_len);

  int ret = sendfile(fd, s, offset, &len, NULL, 0);
  if (ret == -1) uerror("sendfile", Nothing);

  CAMLreturn(Val_int(len));
}

/*

This code taken from Eio's eio_posix_stub.c, licensed as:

Copyright (C) 2021 Anil Madhavapeddy  
Copyright (C) 2022 Thomas Leonard  

Permission to use, copy, modify, and distribute this software for any
purpose with or without fee is hereby granted, provided that the above
copyright notice and this permission notice appear in all copies.

THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

*/

/* Fill [iov] with pointers to the cstructs in the array [v_bufs]. */
static void fill_iov(struct iovec *iov, value v_bufs) {
  int n_bufs = Wosize_val(v_bufs);
  for (int i = 0; i < n_bufs; i++) {
    value v_cs = Field(v_bufs, i);
    value v_ba = Field(v_cs, 0);
    value v_off = Field(v_cs, 1);
    value v_len = Field(v_cs, 2);
    iov[i].iov_base = Bytes_val(v_ba) + Long_val(v_off);
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

/*

This code taken from Jane Street's time_now, licensed as:

The MIT License

Copyright (c) 2019--2023 Jane Street Group, LLC opensource-contacts@janestreet.com

Permission is hereby granted, free of charge, to any person obtaining a copy of
this software and associated documentation files (the "Software"), to deal in
the Software without restriction, including without limitation the rights to
use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
of the Software, and to permit persons to whom the Software is furnished to do
so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.

*/


#define NANOS_PER_SECOND 1000000000

CAMLprim value caml_riot_posix_gettimeofday() {
  CAMLparam0();
  CAMLlocal1(res);

  struct timeval tp;
  if (gettimeofday(&tp, NULL) == -1) {
    res = caml_copy_int64(0);
  } else {
    res = caml_copy_int64(NANOS_PER_SECOND * (uint64_t)tp.tv_sec +
                          (uint64_t)tp.tv_usec * 1000);
  }

  CAMLreturn(res);
}
