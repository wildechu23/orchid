open Math
(* TODO: Decrease size *)
type move = int

(* 
    Let flags be PROMOTION | CAPTURE | SPECIAL 1 | SPECIAL 0 
    - SPECIAL 0: quiet
    - SPECIAL 1: double pawn push
    - SPECIAL 2: king castle
    - SPECIAL 3: queen castle
    - SPECIAL 1 CAPTURE: en-passant
    - PROMOTIONS:
        - 0: Knight
        - 1: Bishop
        - 2: Rook
        - 3: Queen
*)
let normal_flag = 0
let double_pp_flag = 1
let castle_k_flag = 2
let castle_q_flag = 3
let capture_flag = 4
let ep_flag = 5
let promotion_flag = 8
let npromotion_flag = 8
let bpromotion_flag = 9
let rpromotion_flag = 10
let qpromotion_flag = 11

let create_move (from_square : int) (to_square : int) (flags : int) : move =
    let from8x8, to8x8 = from0x88 from_square, from0x88 to_square in 
    ((flags land 0xf) lsl 12) lor ((from8x8 land 0x3f) lsl 6) lor (to8x8 land 0x3f)

let get_to move =
    to0x88 (move land 0x3f)

let get_from move =
    to0x88 ((move lsr 6) land 0x3f)

let get_flags move =
    (move lsr 12) land 0xf

let get_promotion_type move_flags =
    let open Board in
    match move_flags with
    | f when f land qpromotion_flag = qpromotion_flag -> Queen
    | f when f land rpromotion_flag = rpromotion_flag -> Rook
    | f when f land bpromotion_flag = bpromotion_flag -> Bishop
    | f when f land npromotion_flag = npromotion_flag -> Knight
    | _ -> failwith "Invalid flag"

let set_to move to_square =
    (move land (lnot 0x3f)) lor (to_square land 0x3f)

let set_from move from_square =
    (move land (lnot 0xfc0)) lor ((from_square land 0x3f) lsl 6)

let is_capture move =
    (move land (1 lsl 14)) <> 0

let get_butterfly_index move =
    move land 0x0fff

let compare_moves a b =
    (a land 0xffff) = (b land 0xffff)

let make_move board move =
    let open Board in
    let move_from = get_from move in
    let move_to = get_to move in
    let move_flags = get_flags move in
    let current_piece = get_piece board move_from in
    let cap_piece = get_piece board move_to in
    clear_square board move_from;

    if get_piece board move_to <> Empty then
        clear_square board move_to;
    
    (* promotion flags *)
    if move_flags land promotion_flag = promotion_flag then (
        let p = get_promotion_type move_flags in
        place_piece board board.stm p move_to
    )
    else (
        if current_piece = King then (
            let i = match board.stm with
            | White -> 0
            | Black -> 1 
            | _ -> failwith "Board has no color" in
            board.king_loc.(i) <- move_to
        );
        place_piece board board.stm current_piece move_to;
    );

    let new_state = {
        hash = board.state.hash;
        castle = board.state.castle;
        en_passant = board.state.en_passant;
        halfmove = board.state.halfmove;
        previous = None;
        captured = None;
    } in

    let key = ref new_state.hash in
    key := Int64.logxor !key zobrist.color; (* swap color *)

    (* castle flags *)
    key := Int64.logxor !key zobrist.castling.(board.state.castle); (* remove old castle rights *)

    (match move_from with 
    | 7 -> new_state.castle <- new_state.castle land lnot castle_wk; (* H1 *)
    | 4 -> new_state.castle <- new_state.castle land lnot (castle_wk lor castle_wq) (* E1 *)
    | 0 -> new_state.castle <- new_state.castle land lnot castle_wq; (* A1 *)
    | 119 -> new_state.castle <- new_state.castle land lnot castle_bk; (* H8 *)
    | 116 -> new_state.castle <- new_state.castle land lnot (castle_bk lor castle_bq) (* E8 *)
    | 112 -> new_state.castle <- new_state.castle land lnot castle_bq; (* A8 *)
    | _ -> ();

    match move_to with 
    | 7 -> new_state.castle <- new_state.castle land lnot castle_wk; (* H1 *)
    | 0 -> new_state.castle <- new_state.castle land lnot castle_wq; (* A1 *)
    | 119 -> new_state.castle <- new_state.castle land lnot castle_bk; (* H8 *)
    | 112 -> new_state.castle <- new_state.castle land lnot castle_bq; (* A8 *)
    | _ -> ();
    );
    key := Int64.logxor !key zobrist.castling.(new_state.castle); (* add new castle rights *)
    
    (* castle move *)
    if board.stm = White then (
        if move_flags = castle_k_flag then (
            clear_square board 7;
            place_piece board White Rook 5;
        )
        else if move_flags = castle_q_flag then (
            clear_square board 0;
            place_piece board White Rook 3;
        )
    )
    else if board.stm = Black then (
        if move_flags = castle_k_flag then (
            clear_square board 119;
            place_piece board Black Rook 117;
        )
        else if move_flags = castle_q_flag then (
            clear_square board 112;
            place_piece board Black Rook 115;
        )
    );
    

    (* en passant flag *)
    match new_state.en_passant with
    | Some target ->
        let en_passant_file = get_file target in
        key := Int64.logxor !key zobrist.en_passant.(en_passant_file);
        new_state.en_passant <- None;
    | None -> ();

    if (current_piece = Pawn) && (move_flags = double_pp_flag) then (
        let target = (move_from + move_to) / 2 in
        new_state.en_passant <- Some target;
        let en_passant_file = get_file target in
        key := Int64.logxor !key zobrist.en_passant.(en_passant_file);
    );

    (* en passant capture *)
    if move_flags = ep_flag then (
        let ep_pos = match board.stm with
        | White -> move_to + south
        | Black -> move_to + north
        | _ -> failwith "Board has no color" in

        new_state.captured <- Some (board.pieces.(ep_pos));
        clear_square board ep_pos;
    )
    (* capture flag *)
    else if move_flags land capture_flag <> 0 then (
        new_state.captured <- Some (cap_piece);
    );
    
    (* Replace hash *)
    new_state.hash <- !key;

    board.stm <- not_color board.stm;
    new_state.halfmove <- new_state.halfmove + 1;
    new_state.previous <- Some (ref board.state);
    board.state <- new_state
    

let undo_move board move =
    let open Board in
    let move_from = get_from move in
    let move_to = get_to move in
    let move_flags = get_flags move in
    let current_piece = get_piece board move_to in
    
    board.stm <- not_color board.stm;
    clear_square board move_to;

    (* promotion flags *)
    if move_flags land promotion_flag <> 0 then
        place_piece board board.stm Pawn move_from
    else (
        if current_piece = King then (
            let i = match board.stm with
            | White -> 0
            | Black -> 1 
            | _ -> failwith "Board has no color" in
            board.king_loc.(i) <- move_from
        );
        place_piece board board.stm current_piece move_from;
    );

    (* castle move *)
    if board.stm = White then (
        if move_flags = castle_k_flag then
            (clear_square board 5;
            place_piece board White Rook 7;)
        else if move_flags = castle_q_flag then
            (clear_square board 3;
            place_piece board White Rook 0;)
    )
    else if board.stm = Black then (
        if move_flags = castle_k_flag then
            (clear_square board 117;
            place_piece board Black Rook 119;)
        else if move_flags = castle_q_flag then
            (clear_square board 115;
            place_piece board Black Rook 112;)
    );

    (* captured *)
    if move_flags land capture_flag <> 0 then (
        let capsq =
            if move_flags = ep_flag then (
                match board.stm with
                | White -> move_to + south
                | Black -> move_to + north
                | _ -> failwith "Board has no color"
            ) else
                move_to
            in
        match board.state.captured with
        | Some cap -> (
                place_piece board (not_color board.stm) cap capsq;
            );
        | None ->
            failwith "No captured piece";
    );

    match board.state.previous with
    | Some prev -> board.state <- !prev
    | None -> failwith "No previous state to revert to"
