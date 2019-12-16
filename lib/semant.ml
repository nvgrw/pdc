open Common.VisitorMonad
open PassContext

let empty_context: context = { scopes = []; other = 0 }

let passes = [
  Passes.ScopeRes.process;
  (* Passes.Print.process; *)
  Passes.TypeCheck.process
]

let check p = 
  let run = List.fold_left (fun acc elem -> acc >>= fun newP -> elem newP) (success p) passes 
  in run empty_context