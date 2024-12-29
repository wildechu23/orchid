open Board
open Move
open Math

let push_move moves count from_sq to_sq flag =
    moves.(!count) <- Some (create_move from_sq to_sq flag);
    incr count

(* let gen_pawn_moves sq board moves count =
    let direction, capture_left, capture_right, double_push, dpp_row =
        match board.stm with
        | White -> (north, nw, ne, nn, 1)
        | Black -> (south, sw, se, ss, 6)
        | _ -> failwith "Invalid Color"
        in

    (* Normal move *)
    if is_valid_square (sq + direction) && board.pieces.(sq + direction) = Empty then (
        push_move moves count sq (sq + direction) normal_flag;
        (* Double pawn push *)
        let row = sq/16 in
        if row = dpp_row && board.pieces.(sq + double_push) = Empty then
            push_move moves count sq (sq + double_push) double_pp_flag;
    );
    
    (* Captures *)
    let capture_move sq direction =
        let target_sq = sq + direction in
        if is_valid_square target_sq && 
            (board.colors.(target_sq) = (not_color board.stm) || board.state.en_passant = Some target_sq) then
            let move_flag = if board.state.en_passant = Some target_sq then ep_flag else capture_flag in
            push_move moves count sq target_sq move_flag in
    capture_move sq capture_left;
    capture_move sq capture_right *)

let gen_pawn_moves sq board moves count =
    let direction, capture_left, capture_right, double_push, promotion_row, dpp_row =
        match board.stm with
        | White -> (north, nw, ne, nn, 7, 1)
        | Black -> (south, sw, se, ss, 0, 6)
        | _ -> failwith "Invalid Color"
    in

    let is_promotion_target target_sq = target_sq / 16 = promotion_row in

    (* Helper function for adding moves, with promotion handling *)
    let add_pawn_move src target move_flag =
        if is_promotion_target target then (
            push_move moves count src target (npromotion_flag lor move_flag); (* Knight promotion *)
            push_move moves count src target (bpromotion_flag lor move_flag); (* Bishop promotion *)
            push_move moves count src target (rpromotion_flag lor move_flag); (* Rook promotion *)
            push_move moves count src target (qpromotion_flag lor move_flag); (* Queen promotion *)
        ) else
            push_move moves count src target move_flag
    in

    (* Normal move *)
    let target_sq = sq + direction in
    if is_valid_square target_sq && board.pieces.(target_sq) = Empty then (
        add_pawn_move sq target_sq normal_flag;

        (* Double pawn push *)
        let row = sq / 16 in
        if row = dpp_row && board.pieces.(sq + double_push) = Empty then
            push_move moves count sq (sq + double_push) double_pp_flag;
    );

    (* Capture moves *)
    let try_capture direction =
        let target_sq = sq + direction in
        if is_valid_square target_sq &&
            (board.colors.(target_sq) = (not_color board.stm) || board.state.en_passant = Some target_sq) then
            let move_flag = if board.state.en_passant = Some target_sq then ep_flag else capture_flag in
            add_pawn_move sq target_sq move_flag
    in

    try_capture capture_left;
    try_capture capture_right

let gen_knight_moves sq board moves count =
    Array.iter (fun dest ->
        let pos = sq + dest in
        if is_valid_square pos then
            if board.pieces.(pos) = Empty then
                push_move moves count sq pos normal_flag
            else if board.colors.(pos) <> board.stm then
                push_move moves count sq pos capture_flag
    ) knight_dests

(* Tail recursive? *)
let rec slide_dir sq dest board moves count dir =
    if is_valid_square dest then
        if board.pieces.(dest) <> Empty then (
            if board.colors.(dest) <> board.stm then
                push_move moves count sq dest capture_flag
        )
        else (
            push_move moves count sq dest normal_flag;
            slide_dir sq (dest + dir) board moves count dir
        )

let gen_bishop_moves sq board moves count =
    Array.iter (fun dir ->
        slide_dir sq (sq + dir) board moves count dir
    ) bishop_dirs

let gen_rook_moves sq board moves count =
    Array.iter (fun dir ->
        slide_dir sq (sq + dir) board moves count dir
    ) rook_dirs

let gen_queen_moves sq board moves count =
    Array.iter (fun dir ->
        slide_dir sq (sq + dir) board moves count dir
    ) queen_dirs

let gen_king_moves sq board moves count =
    let dests = [|north; ne; east; se; south; sw; west; nw|] in
    Array.iter (fun dest ->
        let pos = sq + dest in
        if is_valid_square pos then
            if board.pieces.(pos) = Empty then
                push_move moves count sq pos normal_flag
            else if board.colors.(pos) <> board.stm then
                push_move moves count sq pos capture_flag
    ) dests

let gen_castling_moves board moves count =
    match board.stm with
    | White ->
        if (board.state.castle land castle_wk) <> 0 then
            if board.pieces.(5) = Empty && (* F1 *)
                board.pieces.(6) = Empty && (* G1 *)
                not (attacked_by board (not_color board.stm) 4) && (* E1 *)
                not (attacked_by board (not_color board.stm) 5) && (* F1 *)
                not (attacked_by board (not_color board.stm) 6) then(* G1 *)
                push_move moves count 4 6 castle_k_flag;
        if (board.state.castle land castle_wq) <> 0 then
            if board.pieces.(1) = Empty && (* B1 *)
                board.pieces.(2) = Empty && (* C1 *)
                board.pieces.(3) = Empty && (* D1 *)
                not (attacked_by board (not_color board.stm) 2) && (* C1 *)
                not (attacked_by board (not_color board.stm) 3) && (* D1 *)
                not (attacked_by board (not_color board.stm) 4) then(* E1 *)
                push_move moves count 4 2 castle_q_flag
    | Black ->
        if (board.state.castle land castle_bk) <> 0 then
            if board.pieces.(117) = Empty && (* F8 *)
                board.pieces.(118) = Empty && (* G8 *)
                not (attacked_by board (not_color board.stm) 116) && (* E1 *)
                not (attacked_by board (not_color board.stm) 117) && (* F1 *)
                not (attacked_by board (not_color board.stm) 118) then(* G1 *)
                push_move moves count 116 118 castle_k_flag;
        if (board.state.castle land castle_bq) <> 0 then
            if board.pieces.(113) = Empty && (* B8 *)
                board.pieces.(114) = Empty && (* C8 *)
                board.pieces.(115) = Empty && (* D8 *)
                not (attacked_by board (not_color board.stm) 114) && (* C8 *)
                not (attacked_by board (not_color board.stm) 115) && (* D8 *)
                not (attacked_by board (not_color board.stm) 116) then(* E8 *)
                push_move moves count 116 114 castle_q_flag
    | _ -> failwith "Board has no side to move"

(* Main function that generates moves and counts them *)
let generate_moves (board : board) (moves : move option array) =
    let count = ref 0 in

    gen_castling_moves board moves count;

    for sq = 0 to 119 do
        if board.colors.(sq) = board.stm then (
            match board.pieces.(sq) with
            | Pawn   -> gen_pawn_moves sq board moves count
            | Knight -> gen_knight_moves sq board moves count
            | Bishop -> gen_bishop_moves sq board moves count
            | Rook   -> gen_rook_moves sq board moves count
            | Queen  -> gen_queen_moves sq board moves count
            | King   -> gen_king_moves sq board moves count
            | Empty  -> ());
    done;
    !count

let rec perft b depth =
    let moves: move option array = Array.make 256 None in
    if depth = 0 then
        1
    else
        let _n_moves = generate_moves b moves in
        Array.fold_left (fun acc move -> 
            match move with
            | Some m ->
                make_move b m;
                let king_attacked = match b.stm with
                | White -> attacked_by b White b.king_loc.(1)
                | Black -> attacked_by b Black b.king_loc.(0)
                | _ -> failwith "Board has no color" in
                let new_perft =
                    if not king_attacked then
                        perft b (depth - 1)
                    else 0 in
                undo_move b m;
                acc + new_perft
            | None -> 
                acc 
        ) 0 moves

let perft_print depth =
    let start_fen = "rnbq1k1r/pp1Pbppp/2p5/8/2B5/8/PPP1NnPP/RNBQK2R w KQ - 1 8" in
    let b = init_empty_board () in
    setup_board_from_fen start_fen;
    
    let moves: move option array = Array.make 256 None in

    if depth = 0 then
        1
    else
        let _n_moves = generate_moves b moves in
        Array.fold_left (fun acc move -> 
            match move with
            | Some m -> 
                make_move b m;
                let king_attacked = match b.stm with
                | White -> attacked_by b White b.king_loc.(1)
                | Black -> attacked_by b Black b.king_loc.(0)
                | _ -> failwith "Board has no color" in
                let new_perft = 
                    if not king_attacked then
                        perft b (depth - 1)
                    else 0 in
                undo_move b m;
                Printf.printf "%d %d: %d\n" (get_from m) (get_to m) (new_perft);
                acc + new_perft
            | None -> 
                acc 
        ) 0 moves