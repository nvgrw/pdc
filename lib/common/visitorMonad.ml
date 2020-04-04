module Option = Core.Option

(* Result type for visitor *)
type ('state, 'ast, 'err) res =
  | Success of 'state * 'ast
  | Error of 'err

(* State monad for visitor *)
type ('state, 'ast, 'err) state = 'state -> ('state, 'ast, 'err) res

let (>>=) m f =
  fun s -> 
  match m s with
  | Error err -> Error err
  | Success (t, v) -> (f v) t

let success v = fun s -> Success (s, v)
let error err = fun _ -> Error err
let map m f = m >>= fun v -> success (f v)

(* let seqOpt (o: ('state, 'v) state option): ('state, 'v option) state =  *)
let seqOpt o =
  let stateOpt = Option.map ~f:(fun s -> s >>= fun v -> success (Some v)) o 
  in Option.value stateOpt ~default:(success None)

let seqList (s: ('state, 'v, 'e) state list): ('state, 'v list, 'e) state = 
  let combine = fun acc elem ->
    acc >>= fun results -> 
    elem >>= fun result -> success (results @ [result])
  in List.fold_left combine (success []) s

let get = fun s -> Success (s, s)

let put newState = fun _ -> Success (newState, ())