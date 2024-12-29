open Alcotest
open Chess.Move

let test_create_move () =
    let move1 = create_move 0 1 0 in
    let move2 = create_move 112 119 0 in
    check int "same int" 1 move1;
    check int "same int" 3647 move2

let test_get_from () =
    let move1 = create_move 0 1 0 in
    let move2 = create_move 112 119 0 in
    check int "same int" 0 (get_from move1);
    check int "same int" 112 (get_from move2)

let test_get_to () =
    let move1 = create_move 0 1 0 in
    let move2 = create_move 112 119 0 in
    check int "same int" 1 (get_to move1);
    check int "same int" 119 (get_to move2)


let tests =
    [
        ("Create Move", `Quick, test_create_move);
        ("Get From Move", `Quick, test_get_from);
        ("Get To Move", `Quick, test_get_to);
    ]