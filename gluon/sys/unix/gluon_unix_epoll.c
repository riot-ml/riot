#ifdef __LINUX__

#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/unixsupport.h>
#include <sys/epoll.h>

CAMLprim value gluon_unix_epoll_create1(value flags) {
    CAMLparam1(flags);
    int epoll_fd = epoll_create1(Int_val(flags));
    if (epoll_fd == -1) uerror("epoll_create1", Nothing);
    CAMLreturn(Val_int(epoll_fd));
}

value epoll_event_to_record(struct epoll_event *eevent) {
  CAMLparam0();
  CAMLlocal1(event);
  event = caml_alloc_tuple(2);
  Store_field(event, 0, caml_copy_int64(eevent->data.u64));
  Store_field(event, 1, Val_int(eevent->events));
  CAMLreturn(event);
}

CAMLprim value gluon_unix_epoll_wait(value v_timeout, value v_max_events, value v_epoll) {
    CAMLparam3(v_timeout, v_max_events, v_epoll);
    CAMLlocal1(event_array);
    int max_events = Int_val(v_max_events);
    struct epoll_event *events = malloc(sizeof(struct epoll_event) * max_events);
    if (events == NULL) caml_failwith("Memory allocation failed");
    
    int ready = epoll_wait(Int_val(v_epoll), events, max_events, Int64_val(v_timeout));
    if (ready == -1) {
        free(events);
        uerror("epoll_wait", Nothing);
    }

    struct epoll_event **event_ptrs = malloc((num_events + 1) * sizeof(struct epoll_event *));
    for (int i = 0; i < num_events; i++) {
        event_ptrs[i] = &events[i];
    }
    event_ptrs[num_events] = NULL;
    event_array = caml_alloc_array(epoll_event_to_record, event_ptrs);

    free(events);
    CAMLreturn(event_array);
}


CAMLprim value gluon_unix_epoll_ctl(value v_epoll, value v_flags, value v_fd, value v_event) {
    CAMLparam4(v_epoll, v_flags, v_fd, v_event);
    struct epoll_event event;
    event.events = Int_val(Field(v_event, 1));
    event.data.u64 = Int64_val(Field(v_event, 0));

    if (epoll_ctl(Int_val(v_epoll), Int_val(v_flags), Int_val(v_fd), &event) == -1) {
        uerror("epoll_ctl", Nothing);
    }

    CAMLreturn(Val_unit);
}

CAMLprim value gluon_unix_epoll_ctl_null(value v_epoll, value v_flags, value v_fd) {
    CAMLparam3(v_epoll, v_flags, v_fd);

    if (epoll_ctl(Int_val(v_epoll), Int_val(v_flags), Int_val(v_fd), NULL) == -1) {
        uerror("epoll_ctl", Nothing);
    }

    CAMLreturn(Val_unit);
}

CAMLprim value gluon_unix_epoll_event(value v_events, value v_token) {
    CAMLparam2(v_events, v_token);
    CAMLlocal1(ocaml_event);

    ocaml_event = caml_alloc_tuple(2);
    Store_field(ocaml_event, 0, caml_copy_int64(Int64_val(v_token)));
    Store_field(ocaml_event, 1, Val_int(Int_val(v_events)));

    CAMLreturn(ocaml_event);
}

#endif
