let north = 16
let south = -16
let nn = 32
let ss = -32
let east = 1
let west = -1
let ne = 17
let sw = -17
let nw = 15
let se = -15

let knight_dests = [|ss - 1; ss + 1; south - 2; south + 2; north - 2; north + 2; nn - 1; nn + 1|]
let bishop_dirs = [|nw; ne; sw; se|]
let rook_dirs = [|north; south; east; west|]
let queen_dirs = [|north; ne; east; se; south; sw; west; nw|]

(* Define the dimensions and mask for the 0x88 board *)
let board_size = 128
let invalid_square_mask = 0x88

(* Check if a square is within the valid 8x8 board bounds *)
let is_valid_square square =
    (square land invalid_square_mask) = 0

(* Convert rank and file to 0x88 board index *)
let to_index rank file =
    (rank lsl 4) + file

let get_file sq =
    sq land 7

let get_rank sq =
    sq lsr 4

let to0x88 sq =
    ((sq land lnot 7) lsl 1) lor (sq land 7)

let from0x88 sq =
    ((sq land lnot 7) lsr 1) lor (sq land 7)

let convert_0x88_alg sq =
    let open Char in
    let file = sq land 7 in
    let rank = sq lsr 4 in
    String.make 1 (chr (file + code 'a')) ^ String.make 1 (chr (rank + code '1'))

let convert_alg_0x88 str =
    let open Char in
    let file = (code str.[0]) - (code 'a') in
    let rank = (code str.[1]) - (code '1') in
    to_index rank file
