open Board
open Limits

let start_fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

let convert_string_move board move =
    let open Move in
    let open String in
    let open Math in

    (* Assume at least length 4 *)
    let start_sq = convert_alg_0x88 (sub move 0 2) in
    let end_sq = convert_alg_0x88 (sub move 2 2) in

    (* capture *)
    let capture = if board.pieces.(end_sq) != Empty then capture_flag else normal_flag in
    
    (* double pp *)
    let dpp_row = match board.stm with
    | White -> 1
    | Black -> 6 
    | _ -> failwith "Invalid color"
    in
    let dpp = if board.pieces.(start_sq) = Pawn && (start_sq / 16) = dpp_row then 
        double_pp_flag else normal_flag in

    (* castle *)
    let castle = if board.pieces.(start_sq) = King then
        match start_sq, end_sq with
        | 4, 2 | 116, 114-> castle_q_flag
        | 4, 6 | 116, 118 -> castle_k_flag
        | _ -> normal_flag
        else normal_flag in

    (* en passant *)
    let sq_diff = abs (start_sq - end_sq) in
    let en_passant = if board.pieces.(start_sq) = Pawn && board.pieces.(end_sq) = Empty && 
        (sq_diff = nw || sq_diff = ne) then ep_flag else normal_flag in

    (* promotion *)
    let promotion_flags = if length move = 5 then (
        match move.[4] with
        | 'n' -> npromotion_flag
        | 'b' -> bpromotion_flag
        | 'r' -> rpromotion_flag
        | 'q' -> qpromotion_flag
        | _ -> failwith "Invalid promotion piece"
    ) else 0 in

    (* Printf.printf "cap: %d dpp: %d castle: %d ep: %d pr: %d" capture dpp castle en_passant promotion_flags; *)
    let flag = capture lor dpp lor castle lor en_passant lor promotion_flags in
    (* Printf.printf "flag: %d\n" flag; *)
    create_move start_sq end_sq flag


let rec uci_go tokens limits =
    match tokens with
    | [] -> limits
    | "searchmoves" :: moves ->  (* must be last command in line, in line w/ stockfish*)
        (* TODO: Probably better to make sure they're all valid moves *)
        let move_list = List.map (convert_string_move !sboard) moves in
        {limits with searchmoves = move_list}
    | "ponder" :: rest ->
        uci_go rest {limits with ponder = true}; 
    | "wtime" :: msec :: rest ->
        let time = [|int_of_string msec; limits.time.(1)|] in 
        uci_go rest {limits with time};
    | "btime" :: msec :: rest -> 
        let time = [|limits.time.(0); int_of_string msec|] in 
        uci_go rest {limits with time};
    | "winc" :: winc :: rest -> 
        let inc = [|int_of_string winc; limits.inc.(1)|] in 
        uci_go rest {limits with inc};
    | "binc" :: binc :: rest ->
        let inc = [|limits.inc.(0); int_of_string binc|] in 
        uci_go rest {limits with inc};
    | "movestogo" :: count :: rest ->  (* moves to next time control *)
        uci_go rest {limits with movestogo = int_of_string count};
    | "depth" :: ply :: rest -> 
        (* let eval, move = search_root !sboard (int_of_string ply) in
        Printf.printf "info score %d from %d to %d\n" eval (get_from move) (get_to move); *)
        uci_go rest {limits with depth = int_of_string ply};
    | "nodes" :: count :: rest -> 
        uci_go rest {limits with nodes = int_of_string count};
    | "mate" :: moves :: rest ->
        uci_go rest {limits with mate = int_of_string moves};
    | "movetime" :: msec :: rest -> (* search exactly msec mseconds*)
        uci_go rest {limits with movetime = int_of_string msec; infinite = true};  (* TODO: DOES NOT CURRENTLY SEARCH EXACTLY MSEC SECONDS *)
    | "infinite" :: rest -> (* search until stop *)
        uci_go rest {limits with infinite = true}; 
    | _ -> limits

let uci line =
    let open Board in
    let open Movegen in
    let open Search in
    
    match String.split_on_char ' ' line with
    | head :: tokens -> (
        match head with
        | "uci" ->
            Printf.printf "id name Stinky Poopoo Butthead\n";
            Printf.printf "uciok\n"
        | "debug" ->
            () (* No debug *)
        | "isready" ->
            Printf.printf "readyok\n"
        | "setoption" ->
            () (* No options currently *)
        | "register" ->
            ()
        | "ucinewgame" ->
            ()
        | "position" -> (
            let fen, rest = match tokens with
            | h :: t ->
                if h = "startpos" then start_fen, t
                else if h = "fen" then (
                    match t with
                    | piece_placement :: active_color :: castling :: en_passant :: halfmove_clock :: fullmove_number :: tl ->
                        String.concat " " [piece_placement; active_color; castling; en_passant; halfmove_clock; fullmove_number],  tl
                    | _ -> failwith "Invalid position fen"
                )
                else failwith "Invalid position command"
            | _ -> failwith "Invalid position command" in
            
            setup_board_from_fen fen;

            (* Parse any following moves *)
            match rest with
            | "moves" :: moves ->
                let open Move in
                List.iter (fun move ->
                    convert_string_move !sboard move |> make_move !sboard)
                    moves;
            | _ ->
                ();
        )
        | "go" -> (
            Atomic.set stop_signal false;
            let limits = uci_go tokens default_limits in
            if limits.perft then let _count = perft_print limits.depth in ()
            else search_run !sboard limits 
        )
        (* | "stop" ->
            Atomic.set stop_signal true;
            Printf.printf "Search stopped\n" *)
        | "ponderhit" ->
            ()
        | "quit" ->
            raise Exit
        | _ ->
            ()
    )
    | _ ->
        ()

    