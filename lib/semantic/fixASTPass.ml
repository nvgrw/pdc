open Common.VisitorMonad
open Common.AST
(* open Common.Data *)
open Common.Meta
open Common.Context

module Walker_FixASTPass = Common.Walker.Make(struct 
    type ctx = context
    type err = pass_error
    type mta = meta

    let visit_program_pre p = success p
    let visit_program_pos p = success p

    let scope_block_pre b = success b
    let scope_block_pos b = success b
    let visit_block_pre b = success b
    let visit_block_pos b = success b

    let visit_stmt_pre s = success s
    let visit_stmt_pos s = success s

    let visit_decl_pre d = success d
    let visit_decl_pos d = success d

    let visit_expr_pre e = success e
    let visit_expr_pos e = success e

    let visit_loc_pre l = success l
    let visit_loc_pos l = success l

    (* due to the bottom-up nature of the parser and syntactic 
       declaration of arrays, the tree needs to be rearranged *)
    let rearrange_data: (int * mta) list ref = ref []
    let rearrange_inner: mta typ option ref = ref None
    let rearrange_depth: int ref = ref 0
    let visit_typ_pre = function
      | Array (Array (_next, _nsize, _), size, m) as t ->
        (* forgive me for the wrong i have done *)
        rearrange_data := (size, m) :: !rearrange_data;
        Core.Int.incr rearrange_depth;
        success t 
      | Array (_typ, _size, _) as t -> 
        rearrange_inner := Some t;
        success t
      | _ as t -> success t
    let visit_typ_pos = function
      (* return a new array containing the rrdata, + clear data *)
      | Array (_typ, _size, _m) as t ->
        success @@ if !rearrange_depth == 0 then
          match !rearrange_inner with
          | Some Array (otyp, osize, om) ->
            let folder acc (size, m) = Array (acc, size, m) in
            let folded = List.fold_left folder otyp (List.rev !rearrange_data) in
            rearrange_data := [];
            rearrange_inner := None;
            Array (folded, osize, om)
          | _ -> assert false
        else begin
          Core.Int.decr rearrange_depth;
          t
        end
      | _ as t -> success t
  end)

let process = Walker_FixASTPass.walk_program