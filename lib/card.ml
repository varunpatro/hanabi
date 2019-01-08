open Core

module Color = struct
  module T = struct
    type t = Green | Blue | Red | Yellow | White
    [@@deriving enumerate, compare, sexp]
  end

  include T
  include Comparator.Make (T)
  module Set = Set.Make (T)
  module Map = Map.Make (T)
end

module Number = struct
  module T = struct
    type t = One | Two | Three | Four | Five
    [@@deriving enumerate, compare, sexp]

    let count = function One -> 3 | Two | Three | Four -> 2 | Five -> 1

    let to_int = function
      | One -> 1
      | Two -> 2
      | Three -> 3
      | Four -> 4
      | Five -> 5
  end

  include T
  include Comparator.Make (T)
  module Set = Set.Make (T)
end

module T = struct
  type t = {color: Color.t; number: Number.t}
  [@@deriving enumerate, compare, sexp]

  let deck =
    List.concat_map all ~f:(fun c ->
        List.init ~f:(const c) (Number.count c.number) )

  let random_deck () = List.permute deck
end

include T
include Comparator.Make (T)
module Set = Set.Make (T)
module Map = Map.Make (T)
