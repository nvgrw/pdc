module Walker_TypeCheck = Walker.Make(struct 
    let visit_program_pre p = p
    let visit_program_pos p = p

    let visit_block_pre b = b
    let visit_block_pos b = b

    let visit_stmt_pre s = s
    let visit_stmt_pos s = s

    let visit_decl_pre d = d
    let visit_decl_pos d = d

    let visit_expr_pre e = e
    let visit_expr_pos e = e

    let visit_loc_pre l = l
    let visit_loc_pos l = l

    let visit_typ_pre t = t
    let visit_typ_pos t = t
  end)

let process = Walker_TypeCheck.walk_program