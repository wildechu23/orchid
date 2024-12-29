open Board
open Movegen
open Move
open Limits


let send_move move =
    let open Move in
    let open Math in
    let move_from = get_from move in
    let move_to = get_to move in
    let alg_from = convert_0x88_alg move_from in
    let alg_to = convert_0x88_alg move_to in
    Printf.printf "bestmove %s%s\n" alg_from alg_to

(* TODO: MOVE TO DEDICATED EVALUATE FILE *)
(* List of weights of pieces *)
let piece_wt = [| 1; 3; 3; 5; 9; 200 |]

let inf = 10000

(* Evaluate the board *)
let evaluate board =
    let flip = match board.stm with
        | White -> 1
        | Black -> -1 
        | _ -> failwith "Invalid color" in
    flip * List.fold_left 
        (fun acc piece ->
            let index = match piece with
                | Pawn -> 0
                | Knight -> 1
                | Bishop -> 2
                | Rook -> 3
                | Queen -> 4
                | King -> 5 
                | _ -> failwith "Invalid piece" in
            let weight = piece_wt.(index) * (board.piece_count.(0).(index) - board.piece_count.(1).(index)) in
            acc + weight)
        0
        all_pieces
    
(* Obsolete Negamax: For reference purposes only *)
let rec negamax_aux board depth =
    if depth = 0 then evaluate board
    else (
        let moves: move option array = Array.make 256 None in
        let _move_count = generate_moves board moves in
        Array.fold_left 
            (fun max_score move -> 
                match move with
                | Some m ->
                    make_move board m;
                    let score = -negamax_aux board (depth - 1) in
                    undo_move board m;
                    max score max_score
                | None ->
                    max_score
            )
            (-inf)
            moves
    )

(* negamax search root *)
let negamax board depth =    
    if depth = 0 then (evaluate board, 0)
    else (
        let moves: move option array = Array.make 256 None in
        let _move_count = generate_moves board moves in
        Array.fold_left 
            (fun (max_score, max_move) move -> 
                match move with
                | Some m ->
                    (* print_int (get_from m);
                    print_int (get_to m);
                    print_endline ""; *)
                    make_move board m;
                    let score = -negamax_aux board (depth - 1) in
                    undo_move board m;
                    (* print_int score;
                    print_endline ""; *)
                    if score > max_score then
                        (score, m)
                    else
                        (max_score, max_move)
                | None ->
                    (max_score, max_move)
            )
            ((-inf), 0)
            moves
    )

(* Relevant search functions *)
(* Quiescent search, only check captures *)
let rec quiesce board alpha beta =
    let stand = evaluate board in
    if stand >= beta then beta
    else (
        let current_alpha = ref alpha in
        if alpha < stand then current_alpha := stand;

        let moves: move option array = Array.make 256 None in
        let move_count = generate_moves board moves in
        (* MAKE EVERY CAPTURE *)
        try
            for i = 0 to move_count - 1 do
                match moves.(i) with
                | Some move when is_capture move->
                    make_move board move;
                    let score = -quiesce board (-beta) (- !current_alpha) in
                    undo_move board move;

                    if score >= beta then
                        raise Exit;
                    if score > !current_alpha then
                        current_alpha := score;
                | _ -> ()
            done;
            !current_alpha;
        with Exit -> beta;
    )

(* Basic alpha beta recursion *)
let rec alpha_beta_aux board alpha beta depth =
    (* TODO: swap evaluate to quiesce *)
    if depth = 0 then quiesce board alpha beta
    else (
        let moves: move option array = Array.make 256 None in
        let move_count = generate_moves board moves in

        let max_score = ref (-inf) in
        let current_alpha = ref alpha in

        (* TT_PROBE GOES HERE (checks if position has happened before) *)

        (* Iterative since that's what Array.fold_left does and I'm not complaining *)
        try
            for i = 0 to move_count - 1 do
                match moves.(i) with
                | Some move ->
                    make_move board move;
                    let score = -alpha_beta_aux board (-beta) (- !current_alpha) (depth - 1) in
                    undo_move board move;

                    (* Update max_score and alpha *)
                    if score > !max_score then (
                        max_score := score;
                        if score > !current_alpha then current_alpha := score;
                    );
                    (* Fail beta cutoff *)
                    if score >= beta then (
                        raise Exit
                    )
                | None -> ()
            done;
            !max_score
        with Exit -> !max_score
    )

(* Root level alpha beta *)
let search_root board limits =
    let depth = limits.depth in
    let alpha, beta = (-inf), inf in

    if depth = 0 then (quiesce board alpha beta, 0)
    else (
        let moves: move option array = Array.make 256 None in
        let move_count = generate_moves board moves in

        let max_score = ref (-inf) in
        let max_move = ref (0) in
        let current_alpha = ref alpha in

        (* Iterative since that's what Array.fold_left does and I'm not complaining *)
        try
            let process_move move =
                make_move board move;
                let score = -alpha_beta_aux board (-beta) (- !current_alpha) (depth - 1) in
                undo_move board move;

                (* Update max_score, max_move and alpha *)
                if score > !max_score then (
                    max_score := score;
                    max_move := move;
                    if score > !current_alpha then current_alpha := score;
                );
                (* Fail beta cutoff *)
                if score >= beta then (
                    raise Exit
                ) 
            in
            
            (* Only search moves in limits *)
            if limits.searchmoves = [] then (
                for i = 0 to move_count - 1 do
                    match moves.(i) with
                    | Some move -> process_move move
                    | None -> ()
                done;
            ) else (
                for i = 0 to move_count - 1 do
                    match moves.(i) with
                    | Some move when List.mem move limits.searchmoves ->
                        process_move move
                    | _ -> ()
                done;
            );
            (!max_score, !max_move)
        with Exit ->
            (!max_score, !max_move)
    )

    
(* TODO: Requires transposition tables *)
let rec search_iterate board current_depth limits =
    if limits.infinite || limits.ponder || current_depth <= limits.depth then (
        if Atomic.get stop_signal then
            0
        else (
            (* Increment depth each run *)
            let eval, move = search_root board {limits with depth = current_depth} in

            (* send info *)
            Printf.printf "info depth %d score cp %d" current_depth (eval * 100);
            (* TODO: PV relies on TT *)

            let next_move = search_iterate board (current_depth + 1) limits  in
            if next_move == 0 then move else next_move
        )
    ) else (
        Atomic.set stop_signal true;
        0
    )

(* Probably convert to LWT, but this functions for now *)
let search_run board limits =
    let open Thread in
    let start_time = int_of_float(Sys.time() *. 1000.0) in

    let result = ref None in
    let thread_fn () = search_iterate board 1 limits in
    let search_thread = create (fun () ->
        let value = thread_fn () in
        result := Some value
    ) () in
    
    let rec poll_thread () =
        flush stdout;
        let read_ready, _, _ = Unix.select [Unix.descr_of_in_channel stdin] [] [] 0.1 in
        if read_ready <> [] then (
            let input = read_line () in
            match input with
            | "stop" -> Atomic.set stop_signal true (* STOP only works on next iteration (but it does technically work) *)
            | _ -> ()
        );
        if int_of_float(Sys.time() *. 1000.0) - start_time > limits.movetime then (
            Atomic.set stop_signal true
        );
        if not (Atomic.get stop_signal) then (
            poll_thread ()
        )
    in

    poll_thread ();
    join search_thread;

    match !result with
    | Some best_move -> send_move best_move;
    | None -> ()