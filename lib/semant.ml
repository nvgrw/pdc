open Common.VisitorMonad

open Semantic.Context

let empty_context: context = { scopes = []; other = 0 }

let passes = [
  Semantic.ScopePass.process;
  (* Semantic.Print.process; *)
  Semantic.TypeCheckPass.process
]

let check p = 
  let run = List.fold_left (fun acc elem -> acc >>= fun newP -> elem newP) (success p) passes 
  in run empty_context