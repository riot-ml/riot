#ifdef __linux__

#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/unixsupport.h>
#include <sys/epoll.h>
#include <errno.h>

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
    
  value *stored_value = (value *)(intptr_t)eevent->data.u64;
  Store_field(event, 0, *stored_value);
  Store_field(event, 1, Val_int(eevent->events));
  caml_remove_generational_global_root(stored_value);
  free(stored_value);
  CAMLreturn(event);
}

CAMLprim value gluon_unix_epoll_wait(value v_timeout, value v_max_events, value v_epoll) {
    CAMLparam3(v_timeout, v_max_events, v_epoll);
    CAMLlocal1(event_array);
    int max_events = Int_val(v_max_events);

    struct epoll_event *events = malloc(sizeof(struct epoll_event) * max_events);
    if (events == NULL) caml_failwith("Memory allocation failed");
    
    caml_enter_blocking_section();
    int ready = epoll_wait(Int_val(v_epoll), events, max_events, Int64_val(v_timeout));
    caml_leave_blocking_section();
    if (ready == -1) {
        free(events);
        uerror("epoll_wait", Nothing);
    }

    if (ready > 0) {
      struct epoll_event **event_ptrs = malloc((ready + 1) * sizeof(struct epoll_event *));
      for (int i = 0; i < ready; i++) {
        event_ptrs[i] = &events[i];
      }
      event_ptrs[ready] = NULL;
      event_array = caml_alloc_array(epoll_event_to_record, event_ptrs);
      free(event_ptrs);
    } else {
      event_array = Atom(0);
    }

    free(events);
    CAMLreturn(event_array);
}


CAMLprim value gluon_unix_epoll_ctl(value v_epoll, value v_flags, value v_fd, value v_event) {
    CAMLparam4(v_epoll, v_flags, v_fd, v_event);

    struct epoll_event event;
    event.events = Int_val(Field(v_event, 1));

    value* ocaml_value = malloc (sizeof (value*));
    *ocaml_value = Field(v_event, 0);
    caml_register_generational_global_root(ocaml_value);
    event.data.u64 = ocaml_value;

    caml_enter_blocking_section();
    int res = epoll_ctl(Int_val(v_epoll), Int_val(v_flags), Int_val(v_fd), &event);
    caml_leave_blocking_section();
    if (res == -1) uerror("epoll_ctl", Nothing);

    CAMLreturn(Val_unit);
}

CAMLprim value gluon_unix_epoll_ctl_null(value v_epoll, value v_flags, value v_fd) {
    CAMLparam3(v_epoll, v_flags, v_fd);

    caml_enter_blocking_section();
    int res = epoll_ctl(Int_val(v_epoll), Int_val(v_flags), Int_val(v_fd), NULL);
    caml_leave_blocking_section();
    if (res == -1) uerror("epoll_ctl_null", Nothing);

    CAMLreturn(Val_unit);
}

#endif
