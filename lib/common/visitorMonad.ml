(* Result type for visitor *)
type ('state, 'ast) res =
  | Success of 'state * 'ast
  | Error 

(* State monad for visitor *)
type ('state, 'ast) state = 'state -> ('state, 'ast) res

let (>>=) m f =
  fun s -> 
  match m s with
  | Error -> Error
  | Success (t, v) -> (f v) t

let success v = fun s -> Success (s, v)
let error = fun _ -> Error
let map m f = m >>= fun v -> success (f v)

(* let seqOpt (o: ('state, 'v) state option): ('state, 'v option) state =  *)
let seqOpt o =
  let stateOpt = Option.map (fun s -> s >>= fun v -> success (Some v)) o 
  in Option.value stateOpt ~default:(success None)

let seqList (s: ('state, 'v) state list): ('state, 'v list) state = 
  let combine = fun acc elem ->
    acc >>= fun results -> 
    elem >>= fun result -> success (results @ [result]) 
  in List.fold_left combine (success []) s

let get = fun s -> Success (s, s)

let put newState = fun _ -> Success (newState, ())