(* TODO
- quiescence
- alpha-beta + move ordering
- i/o
- better eval function lmao *)

(* let () =  *)
    (* let open Chess.Board in *)
    (* let open Chess.Movegen in *)
    (* let open Chess.Move in *)
    (* let start_fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1" in
    let b = setup_board_from_fen start_fen in
    print_board b; *)

    (* let moves: move option array = Array.make 256 None in
    let count = generate_moves b moves in
    print_int count;
    print_endline ("") *)

    (* let count = perft_print 4 in
    print_int count;
    print_endline "" *)

let () =
    let open Chess.Input in
    try
        while true do
            let line = read_line () in
            (* print_endline line; *)
            uci line;
        done;
    with Exit | End_of_file ->
        ()