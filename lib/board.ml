open Math
open Transposition

(* Represent empty squares and some basic piece types *)
type piece = Empty | Pawn | Knight | Bishop | Rook | Queen | King [@@deriving show]
let piece_pp formatter (p : piece) =
    Format.fprintf formatter "%s" (show_piece p)

let all_pieces = [Pawn; Knight; Bishop; Rook; Queen; King]

type color = White | Black | NoColor [@@deriving show]
let not_color = function
  | White -> Black
  | Black -> White
  | NoColor -> NoColor

let color_pp formatter (c : color) =
    Format.fprintf formatter "%s" (show_color c)

let castle_wk = 1
let castle_wq = 2
let castle_bk = 4
let castle_bq = 8

type stateinfo = {
    mutable hash: key;
    mutable castle: int;
    mutable en_passant: int option; 
    mutable halfmove: int;
    mutable previous: stateinfo ref option;
    mutable captured: piece option;
}

(* A board is an array of pieces, initialized to Empty *)
type board = {
    pieces: piece array;
    colors: color array;
    piece_count: int array array;
    mutable king_loc : int array;
    mutable stm: color;
    mutable state : stateinfo;
} 

(* Initialize the 0x88 board *)
let init_empty_board () : board =
    let empty_stateinfo = {
        hash = 0L;
        castle = 15;
        en_passant = None;
        halfmove = 0;
        previous = None;
        captured = None;
    } in
    {
        pieces = Array.make board_size Empty;
        colors = Array.make board_size NoColor;
        piece_count = Array.make_matrix 2 6 0;
        king_loc = Array.make 2 0;
        stm = White;
        state = empty_stateinfo;
    }

(* GLOBAL CONSTANTS *)
let zobrist = init_zobrist ()
let sboard = ref (init_empty_board ())

(* Example: place a piece on the board *)
let place_piece board color piece sq =
    board.pieces.(sq) <- piece;
    board.colors.(sq) <- color;

    let color_i = match color with
        | White -> 0
        | Black -> 1
        | _ -> failwith "Invalid color" in
    let piece_i = match piece with
        | Pawn -> 0
        | Knight -> 1
        | Bishop -> 2
        | Rook -> 3
        | Queen -> 4
        | King -> 5
        | _ -> failwith "Invalid piece" in
    board.piece_count.(color_i).(piece_i) <- board.piece_count.(color_i).(piece_i) + 1;

    board.state.hash <- Int64.logxor board.state.hash zobrist.piece_square.{piece_i, color_i, sq}

let place_piece_rf board color piece rank file =
    let index = to_index rank file in
    place_piece board color piece index

let clear_square board sq =
    let color = board.colors.(sq) in
    let piece = board.pieces.(sq) in
    board.pieces.(sq) <- Empty;
    board.colors.(sq) <- NoColor;

    let color_i = match color with
        | White -> 0
        | Black -> 1
        | _ -> failwith "Invalid color" in
    let piece_i = match piece with
        | Pawn -> 0
        | Knight -> 1
        | Bishop -> 2
        | Rook -> 3
        | Queen -> 4
        | King -> 5
        | _ -> failwith "Invalid piece" in
    board.piece_count.(color_i).(piece_i) <- board.piece_count.(color_i).(piece_i) - 1;

    board.state.hash <- Int64.logxor board.state.hash zobrist.piece_square.{piece_i, color_i, sq}

let clear_square_rf board rank file =
    let index = to_index rank file in
    clear_square board index

let is_piece board color piece sq = 
    board.pieces.(sq) = piece && board.colors.(sq) = color 

let get_piece board sq =
    if is_valid_square sq then
        board.pieces.(sq)
    else
        Empty

let get_piece_rf board rank file =
    let index = to_index rank file in
    if is_valid_square index then
        board.pieces.(index)
    else
        Empty

let get_color board sq =
    if is_valid_square sq then
        board.colors.(sq)
    else
        NoColor

let get_color_rf board rank file =
    let index = to_index rank file in
    if is_valid_square index then
        board.colors.(index)
    else
        NoColor

let attacked_by_knight board color sq =
    Array.exists (fun dest ->
        let pos = sq + dest in
        is_valid_square pos && is_piece board color Knight pos
    ) knight_dests

(* Tail recursive? *)
let rec check_attacked_straight_dir dest board color dir =
    if is_valid_square dest then
        if is_piece board color Rook dest || is_piece board color Queen dest then
            true
        else if board.pieces.(dest) = Empty then
            check_attacked_straight_dir (dest + dir) board color dir
        else
            false
    else
        false    

let attacked_straight board color sq =
    Array.exists (fun dir ->
        check_attacked_straight_dir (sq + dir) board color dir
    ) rook_dirs

(* Tail recursive? *)
let rec check_attacked_diag_dir dest board color dir =
    if is_valid_square dest then
        if is_piece board color Bishop dest || is_piece board color Queen dest then
            true
        else if board.pieces.(dest) = Empty then
            check_attacked_diag_dir (dest + dir) board color dir
        else 
            false
    else
        false    

let attacked_diag board color sq =
    Array.exists (fun dir ->
        check_attacked_diag_dir (sq + dir) board color dir
    ) bishop_dirs

(* color is the attacking color *)
let attacked_by board color sq =
    let (pawn_dir1, pawn_dir2) = match color with
        | White -> (se, sw)
        | Black -> (ne, nw)
        | _ -> failwith "Board has no side to move"
    in
    (is_valid_square (sq + pawn_dir1) && is_piece board color Pawn (sq + pawn_dir1)) ||
    (is_valid_square (sq + pawn_dir2) && is_piece board color Pawn (sq + pawn_dir2)) ||
    attacked_by_knight board color sq ||
    attacked_straight board color sq ||
    attacked_diag board color sq

(* Create a string representation of the board *)
let to_string (board: board) =
    let buffer = Buffer.create 128 in  (* Initial buffer size *)
    for rank = 7 downto 0 do
        for file = 0 to 7 do
            let index = to_index rank file in
            let piece_string = match board.pieces.(index), board.colors.(index) with
                | Empty, NoColor -> ". "
                | Pawn, White -> "P "
                | Pawn, Black -> "p "
                | Knight, White -> "N "
                | Knight, Black -> "n "
                | Bishop, White -> "B "
                | Bishop, Black -> "b "
                | Rook, White -> "R "
                | Rook, Black -> "r "
                | Queen, White -> "Q "
                | Queen, Black -> "q "
                | King, White -> "K "
                | King, Black -> "k "
                | _ , _ -> failwith "Invalid piece"
            in
            Buffer.add_string buffer piece_string
        done;
        Buffer.add_char buffer '\n'
    done;
    Buffer.contents buffer

let board_pp formatter (board : board) =
    Format.fprintf formatter "%s" (to_string board)

let board_equal a b =
    (a.pieces = b.pieces) && (a.colors = b.colors)

let print_board board =
    print_string (to_string board)

(* FEN stuff *)
let piece_id_map =
    [
        ("P", Pawn);
        ("R", Rook);
        ("B", Bishop);
        ("N", Knight);
        ("Q", Queen);
        ("K", King);
    ]

let piece_id_of_string s =
    try List.assoc s piece_id_map
    with Not_found -> failwith "invalid string identifier"

let color_of_string s = if String.uppercase_ascii s = s then White else Black

(* Note: Setup directly references sboard (may not be best practice but simplifies code) *)
(* Setup board from FEN string recursively *)
let rec setup_board_from_fen_aux fen i rank file =
    if i >= String.length fen || rank < 0 then
        ()  (* Base case: stop when end of FEN or ranks are exhausted *)
    else
        match fen.[i] with
        | '/' ->
            (* Move to the next rank *)
            setup_board_from_fen_aux fen (i + 1) (rank - 1) 0  (* Increment index and decrement rank *)
        | '1' .. '8' as c ->
            (* Skip empty squares *)
            let skip_count = Char.code c - Char.code '0' in
            setup_board_from_fen_aux fen (i + 1) rank (file + skip_count)  (* Skip squares *)
        | piece_char when List.mem piece_char ['P'; 'p'; 'N'; 'n'; 'B'; 'b'; 'R'; 'r'; 'Q'; 'q'; 'K'; 'k'] ->
            let c = fen.[i] in
            let piece = piece_id_of_string (String.uppercase_ascii (Char.escaped c)) in
            let color = color_of_string (Char.escaped c) in
            
            place_piece_rf !sboard color piece rank file;
            if piece_char = 'K' then
                !sboard.king_loc.(0) <- to_index rank file
            else if piece_char = 'k' then
                !sboard.king_loc.(1) <- to_index rank file;

            setup_board_from_fen_aux fen (i + 1) rank (file + 1)  (* Move to the next file *)
        | _ -> 
            ()
            (* failwith "Invalid FEN format"  Raise exception for invalid character *)

let set_active_color active_color =
    !sboard.stm <- match active_color with
        | "w" -> White
        | "b" -> (
            !sboard.state.hash <- Int64.logxor !sboard.state.hash zobrist.color;
            Black
        )
        | _ -> failwith "Invalid FEN format - Invalid Side to Move"

let set_castling_rights castling =
    String.fold_left
        (fun acc c ->
            match c with
            | 'K' -> acc lor castle_wk
            | 'Q' -> acc lor castle_wq
            | 'k' -> acc lor castle_bk
            | 'q' -> acc lor castle_bq
            | _ -> acc) (* Ignore any unexpected characters *)
        0 castling
    

let set_en_passant en_passant_str =
    match en_passant_str with
    | "-" -> None  (* No en passant target *)
    | _ ->
        let file = Char.code en_passant_str.[0] - Char.code 'a' in
        let rank = int_of_string (String.make 1 en_passant_str.[1]) - 1 in
        Some (to_index rank file)

let setup_board_from_fen fen =
    let open Str in
    let parts = split (regexp " ") fen in
    match parts with
    | [piece_placement; active_color; castling; en_passant; halfmove_clock; _fullmove_number] ->
        (* References sboard directly *)
        setup_board_from_fen_aux piece_placement 0 7 0;
        set_active_color active_color;

        let hash = !sboard.state.hash in
        
        (* Does not reference sboard *)
        let castling_rights = set_castling_rights castling in

        let en_passant_target = set_en_passant en_passant in

        let en_passant_hash = match en_passant_target with
            | Some t -> 
                let en_passant_file = get_file t in
                zobrist.en_passant.(en_passant_file)
            | None -> 0L 
        in

        let new_hash = Int64.logxor 
            (Int64.logxor hash zobrist.castling.(castling_rights)) 
            en_passant_hash 
        in
        
        let state = {
            hash = new_hash;
            castle = castling_rights;
            en_passant = en_passant_target;
            halfmove = int_of_string halfmove_clock;
            previous = None;
            captured = None;
        } in
        !sboard.state <- state;
    | _ -> failwith "Invalid FEN format"