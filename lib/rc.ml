type 'a t = { refc : int Atomic.t; value : 'a }

let make value = { refc = Atomic.make 1; value }
let get t = t.value
