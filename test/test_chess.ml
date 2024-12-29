let () = 
    Alcotest.run "Chess"
        [
            ("board", Test_board.tests);
            ("move", Test_move.tests)
        ]