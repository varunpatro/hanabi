open Core;;

module Color = struct
    module T = struct
        type t = Green | Blue | Red | Yellow | White
        [@@deriving enumerate, compare, sexp]
    end
    include T
    include Comparator.Make(T)
end

module Number = struct
    module T = struct
        type t = One | Two | Three | Four | Five
        [@@deriving enumerate, compare, sexp]

        let count = function
            | One -> 3
            | Two | Three | Four -> 2
            | Five -> 1
        ;;
    end
    include T
    include Comparator.Make(T)
end

module T = struct
    type t = Color.t * Number.t
    [@@deriving enumerate, compare, sexp]

    let deck =
        List.cartesian_product Color.all Number.all
        |> List.concat_map ~f:(fun (c, n) -> List.init ~f:(fun _ -> c, n) (Number.count n))
    ;;

    let random_deck () = List.permute deck

end

include T
include Comparator.Make(T)
module ColorSet = Set.Make(Color)
module NumberSet = Set.Make(Number)
module CardSet = Set.Make(T)
module CardMap = Map.Make(T)