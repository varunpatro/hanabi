open Core;;

module Color = struct
    type t = Green | Blue | Red | Yellow | White
    [@@deriving enumerate]
end

module Number = struct
    type t = One | Two | Three | Four | Five
    [@@deriving enumerate]

    let count = function
    | One -> 3
    | Two | Three | Four -> 2
    | Five -> 1
;;
end

type t = Color.t * Number.t

let deck =
    List.cartesian_product Color.all Number.all
    |> List.concat_map ~f:(fun (c, n) -> Util.repeat (c, n) (Number.count n))
;;

let random_deck () = List.permute deck

