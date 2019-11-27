(* Result type for visitor *)
type ('state, 'ast) res =
  | Success of 'state * 'ast
  | Error 

(* State monad for visitor *)
type ('state, 'ast) state = 'state -> ('state, 'ast) res

val (>>=): ('s, 'a) state -> ('a -> ('s, 'b) state) -> ('s, 'b) state
val success: 'a -> ('s, 'a) state
val error: ('s, 'a) state
val map: ('s, 'a) state -> ('a -> 'b) -> ('s, 'b) state

val seqOpt: ('state, 'v) state option -> ('state, 'v option) state
val seqList: ('state, 'v) state list -> ('state, 'v list) state

val get: ('state, 'state) state
val put: 'state -> ('state, unit) state