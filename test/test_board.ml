open Alcotest
open Chess.Board
open Chess.Math

let test_init_empty_board () =
    let empty_board_string =
        ". . . . . . . . \n" ^
        ". . . . . . . . \n" ^
        ". . . . . . . . \n" ^
        ". . . . . . . . \n" ^
        ". . . . . . . . \n" ^
        ". . . . . . . . \n" ^
        ". . . . . . . . \n" ^
        ". . . . . . . . \n"
    in
    let empty_board = init_empty_board () in
    check string "same string" (to_string empty_board) empty_board_string

let test_is_valid_square () =
    check bool "valid square" (is_valid_square 0) true;
    check bool "valid square" (is_valid_square 7) true;
    check bool "valid square" (is_valid_square 112) true;
    check bool "valid square" (is_valid_square 119) true;
    
    check bool "not valid square" (is_valid_square (-1)) false;
    check bool "not valid square" (is_valid_square 8) false;
    check bool "not valid square" (is_valid_square (-16)) false;
    check bool "not valid square" (is_valid_square (-9)) false;
    check bool "not valid square" (is_valid_square 111) false;
    check bool "not valid square" (is_valid_square 120) false;
    check bool "not valid square" (is_valid_square 128) false;
    check bool "not valid square" (is_valid_square 135) false

let test_to_index () =
    check int "to_index 0 0" (to_index 0 0) 0;
    check int "to_index 0 0" (to_index 0 7) 7;
    check int "to_index 0 0" (to_index 1 0) 16;
    check int "to_index 0 0" (to_index 7 0) 112;
    check int "to_index 0 0" (to_index 7 7) 119

let test_place_piece () =
    let open Testable in
    let a = init_empty_board () in
    let b = init_empty_board () in
    place_piece_rf a White Knight 0 1;
    place_piece_rf b White Knight 0 1;
    check board "place_piece white knight b1" a b

let test_get_piece () =
    let open Testable in
    let a = init_empty_board () in
    place_piece_rf a White Knight 0 1;
    let n = get_piece_rf a 0 1 in
    check piece "Same piece knight" n Knight;
    let e = get_piece_rf a 1 1 in
    check piece "Same piece empty" e Empty

let test_get_color () =
    let open Testable in
    let a = init_empty_board () in
    place_piece_rf a White Knight 0 1;
    let n = get_color_rf a 0 1 in
    check color "Same piece knight" n White;
    let e = get_color_rf a 1 1 in
    check color "Same piece empty" e NoColor

let test_board_equal () =
    let a = init_empty_board () in
    let b = init_empty_board () in
    check bool "Empty boards equal" (board_equal a b) true;
    place_piece_rf a White Knight 0 1;
    check bool "Boards not equal" (board_equal a b) false;
    place_piece_rf b White Knight 0 1;
    check bool "Boards equal" (board_equal a b) true

let test_piece_equal () =
    let open Testable in
    check piece "Equal Pawn" Pawn Pawn;
    check piece "Equal Knight" Knight Knight

let test_fen () =
    let start_fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1" in
    let b = init_empty_board () in
    setup_board_from_fen start_fen;
    let default_board_string =
        "R N B Q K B N R \n" ^
        "P P P P P P P P \n" ^
        ". . . . . . . . \n" ^
        ". . . . . . . . \n" ^
        ". . . . . . . . \n" ^
        ". . . . . . . . \n" ^
        "P P P P P P P P \n" ^
        "R N B Q K B N R \n"
    in
    check string "same string" (to_string b) default_board_string;

    let some_fen = "2rr2k1/b4p2/p4npp/4p3/N2n3q/1PPP1PRP/P2Q1P2/2K3R1 w - - 2 22" in
    let b2 = init_empty_board () in
    setup_board_from_fen some_fen;
    let some_board_string =
        ". . R R . . K . \n" ^
        "B . . . . P . . \n" ^
        "P . . . . N P P \n" ^
        ". . . . P . . . \n" ^
        "N . . N . . . Q \n" ^
        ". P P P . P R P \n" ^
        "P . . Q . P . . \n" ^
        ". . K . . . R . \n"
    in
    check string "same string" (to_string b2) some_board_string


let tests =
    [
        ("Empty Board", `Quick, test_init_empty_board);
        ("Is Valid Square", `Quick, test_is_valid_square);
        ("To Index", `Quick, test_to_index);
        ("Place Piece", `Quick, test_place_piece);
        ("Get Piece", `Quick, test_get_piece);
        ("Get Color", `Quick, test_get_color);
        ("Board Equal", `Quick, test_board_equal);
        ("Piece Equal", `Quick, test_piece_equal);
        ("FEN", `Quick, test_fen)
    ]