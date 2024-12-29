open Bigarray

type key = int64

type tt_entry = {
    hash: key;
    value: int;
    dpeth: int;
    flags: int;
    best_move: int; (* TODO: fix dependency cycle by resolving types*)
}

(* Zobrist Hashing *)
type zobrist = {
    piece_square: (key, int64_elt, c_layout) Array3.t;
    color: key;
    castling: key array;
    en_passant: key array;
}

let init_zobrist () =
  (* TODO: Maybe convert to own PRNG *)
    let open Random in
    self_init (); (* Note fully random, may need to change *)

    let piece_square = Array3.create Bigarray.int64 c_layout 6 2 128 in
    for i = 0 to 5 do
        for j = 0 to 1 do
            for k = 0 to 127 do
                piece_square.{i, j, k} <- bits64 ()
            done
        done
    done;

    let color = Random.bits64 () in

    let castling = Array.make 16 0L in
    for i = 0 to 15 do
        castling.(i) <- Random.bits64 ()
    done;

    let en_passant = Array.make 8 0L in
    for i = 0 to 7 do
        en_passant.(i) <- Random.bits64 ()
    done;

    {
        piece_square = piece_square;
        color = color;
        castling = castling;
        en_passant = en_passant;
    }

(* let tt_probe depth alpha beta =
    () *)
