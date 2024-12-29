open Move

type limits = {
    time: int array;
    inc: int array;
    movestogo: int;
    depth: int;
    nodes: int;
    mate: int;
    movetime : int;
    perft: bool;
    ponder: bool;
    infinite: bool;
    searchmoves: move list;
}

let default_limits = {
  time = [| 0; 0 |];  (* [wtime; btime] *)
  inc = [| 0; 0 |];   (* [winc; binc] *)
  movestogo = 0;
  depth = 0;
  nodes = 0;
  mate = 0;
  movetime = 0;
  perft = false;
  ponder = false;
  infinite = false;
  searchmoves = [];
}

let stop_signal = Atomic.make false
