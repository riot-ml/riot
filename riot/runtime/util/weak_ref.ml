type 'a t = 'a Weak.t

let make x =
  let t = Weak.create 1 in
  Weak.set t 0 (Some x);
  t

let get t = Weak.get t 0
