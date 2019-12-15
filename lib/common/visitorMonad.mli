(* Result type for visitor *)
type ('state, 'ast, 'err) res =
  | Success of 'state * 'ast
  | Error of 'err

(* State monad for visitor *)
type ('state, 'ast, 'err) state = 'state -> ('state, 'ast, 'err) res

val (>>=): ('s, 'a, 'e) state -> ('a -> ('s, 'b, 'e) state) -> ('s, 'b, 'e) state
val success: 'a -> ('s, 'a, 'e) state
val error: 'e -> ('s, 'a, 'e) state
val map: ('s, 'a, 'e) state -> ('a -> 'b) -> ('s, 'b, 'e) state

val seqOpt: ('state, 'v, 'e) state option -> ('state, 'v option, 'e) state
val seqList: ('state, 'v, 'e) state list -> ('state, 'v list, 'e) state

val get: ('state, 'state, 'err) state
val put: 'state -> ('state, unit, 'err) state