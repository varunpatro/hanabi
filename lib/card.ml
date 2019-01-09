open Core

module Color = struct
  module T = struct
    type t = G | B | R | Y | W [@@deriving enumerate, compare, sexp]
  end

  include T
  include Comparator.Make (T)
  module Set = Set.Make (T)
  module Map = Map.Make (T)
end

module Number = struct
  module T = struct
    type t = N_1 | N_2 | N_3 | N_4 | N_5
    [@@deriving enumerate, compare, sexp]

    let count = function N_1 -> 3 | N_2 | N_3 | N_4 -> 2 | N_5 -> 1

    let to_int = function
      | N_1 -> 1
      | N_2 -> 2
      | N_3 -> 3
      | N_4 -> 4
      | N_5 -> 5
  end

  include T
  include Comparator.Make (T)
  module Set = Set.Make (T)
end

module T = struct
  type t = Color.t * Number.t [@@deriving enumerate, compare, sexp]

  let deck =
    List.concat_map all ~f:(fun c ->
        List.init ~f:(const c) (Number.count (snd c)) )

  let random_deck () = List.permute deck
end

include T
include Comparator.Make (T)
module Set = Set.Make (T)
module Map = Map.Make (T)
