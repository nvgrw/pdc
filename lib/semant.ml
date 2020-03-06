open Common.VisitorMonad

open Semantic.Context

let passes = [
  Semantic.ScopePass.process;
  (* Semantic.Print.process; *)
  Semantic.TypeCheckPass.process
]

let check p = 
  let run = List.fold_left (fun acc elem -> acc >>= fun newP -> elem newP) (success p) passes 
  in run empty_context