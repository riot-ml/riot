#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/unixsupport.h>
#include <errno.h>
#include <fcntl.h>
#include <string.h>
#include <sys/event.h>
#include <unistd.h>

CAMLprim value gluon_unix_kevent(value max_events_val, value timeout_val, value fd_val) {
    CAMLparam3(max_events_val, timeout_val, fd_val);
    CAMLlocal1(event_array);

    int max_events = Int_val(max_events_val);
    int64_t timeout_ns = Int64_val(timeout_val);
    int fd = Long_val(fd_val);

    struct kevent *events = malloc(sizeof(struct kevent) * max_events);
    if (events == NULL) {
        caml_failwith("Memory allocation failed");
    }

    int num_events = -1;

    if (timeout_ns < 0) {
      caml_enter_blocking_section();
      num_events = kevent(fd, NULL, 0, events, max_events, NULL);
      caml_leave_blocking_section();
    } else {
      struct timespec timeout;
      timeout.tv_sec = timeout_ns / 1000000000;
      timeout.tv_nsec = timeout_ns % 1000000000;
      caml_enter_blocking_section();
      num_events = kevent(fd, NULL, 0, events, max_events, &timeout);
      caml_leave_blocking_section();
    }

    if (num_events == -1) {
        free(events);
        uerror("kevent", Nothing);
    }

    event_array = caml_alloc(num_events, Abstract_tag);
    for (int i = 0; i < num_events; i++) {
      Store_field(event_array, i, &events[i]);
    }

    free(events);
    CAMLreturn(event_array);
}

CAMLprim value gluon_unix_fcntl(value fd, value cmd, value arg) {
    CAMLparam3(fd, cmd, arg);

    int c_fd = Int_val(fd);
    int c_cmd = Int_val(cmd);
    int c_arg = Int_val(arg);
    int result = fcntl(c_fd, c_cmd, c_arg);

    if (result == -1) uerror("fcntl", Nothing);

    CAMLreturn(Val_int(result));
}

CAMLprim value gluon_unix_kqueue(value unit) {
    CAMLparam1(unit);

    int fd = kqueue();
    if (fd == -1) uerror("kqueue", Nothing);

    CAMLreturn(Val_int(fd));
}

CAMLprim value gluon_unix_kevent_register(value fd_val, value events_val, value ignored_errors_val) {
    CAMLparam3(fd_val, events_val, ignored_errors_val);
    int fd = Int_val(fd_val);
    int num_events = Wosize_val(events_val);
    int num_ignored_errors = Wosize_val(ignored_errors_val);

    struct kevent *changes = (struct kevent *)malloc(sizeof(struct kevent) * num_events);
    if (changes == NULL) {
        caml_failwith("Memory allocation failed");
    }

    // Access events directly from OCaml array
    for (int i = 0; i < num_events; i++) {
        value e = Field(events_val, i);
        changes[i] = *(struct kevent *)Field(e, 0);
        // fprintf(stderr, "Event %d: ident=%lu, filter=%d, flags=%u, fflags=%u, data=%ld, udata=%p\n",
        //     i,
        //     changes[i].ident,
        //     changes[i].filter,
        //     changes[i].flags,
        //     changes[i].fflags,
        //     changes[i].data,
        //     changes[i].udata);
    }

    caml_enter_blocking_section();
    int result = kevent(fd, changes, num_events, NULL, 0, NULL);
    caml_leave_blocking_section();
    free(changes);

    if (result == -1) {
        if (errno == EINTR) {
            // According to the manual page of FreeBSD: "When kevent() call fails
            // with EINTR error, all changes in the changelist have been applied",
            // so we can safely ignore it.
            CAMLreturn(Val_unit);
        }
        // fprintf(stderr, "debug: error %d count %d", errno, num_ignored_errors);
        for (int i = 0; i < num_ignored_errors; i++) {
            if (Int_val(Field(ignored_errors_val, i)) == errno) {
                // Ignore this specific error
                CAMLreturn(Val_unit);
            }
        }
        // fprintf(stderr, "debug: error %d", errno);
        uerror("kevent_register", Nothing);
    }

    CAMLreturn(Val_unit);
}

CAMLprim value gluon_unix_kevent_create(value fd_val, value filter_val, value flags_val, value token_val) {
    CAMLparam4(fd_val, filter_val, flags_val, token_val);
    CAMLlocal1(kevent_val);

    struct kevent *kevent = malloc(sizeof(struct kevent));
    if (kevent == NULL) {
        caml_failwith("Memory allocation failed");
    }

    int fd = Int_val(fd_val);
    int filter = Int_val(filter_val);
    int flags = Int_val(flags_val); 
    int token = Int_val(token_val);

    EV_SET(kevent, fd, filter, flags, 0, 0, (void *)(intptr_t)(token));

    // fprintf(stderr, "kevent: ident=%lu, filter=%d, flags=%u, fflags=%u, data=%ld, udata=%p\n",
    //     kevent->ident,
    //     kevent->filter,
    //     kevent->flags,
    //     kevent->fflags,
    //     kevent->data,
    //     kevent->udata);

    kevent_val = caml_alloc(1, Abstract_tag);
    Store_field(kevent_val, 0, (value)kevent);

    CAMLreturn(kevent_val);
}

CAMLprim value gluon_unix_kevent_udata(value kevent_val) {
    CAMLparam1(kevent_val);
    struct kevent *kevent = Data_abstract_val(kevent_val);
    // fprintf(stderr, "kevent: ident=%lu, filter=%d, flags=%u, fflags=%u, data=%ld, udata=%p\n",
    //     kevent->ident,
    //     kevent->filter,
    //     kevent->flags,
    //     kevent->fflags,
    //     kevent->data,
    //     kevent->udata);
    intptr_t udata = (intptr_t)(kevent->udata);
    CAMLreturn(Val_long(udata));
}

CAMLprim value gluon_unix_kevent_filter(value kevent_val) {
    CAMLparam1(kevent_val);
    struct kevent *kevent = Data_abstract_val(kevent_val);
    intptr_t filter = (intptr_t)(kevent->filter);
    CAMLreturn(Val_long(filter));
}

CAMLprim value gluon_unix_kevent_flags(value kevent_val) {
    CAMLparam1(kevent_val);
    struct kevent *kevent = Data_abstract_val(kevent_val);
    CAMLreturn(Val_long(kevent->flags));
}
