open! Core

module Member = struct
  module T = struct
    type t = {card: Card.t; pos_c: Card.Color.Set.t; pos_n: Card.Number.Set.t}
    [@@deriving compare, sexp]
  end

  include T
  include Comparable.Make (T)
end

module T = struct
  type t = Member.t list [@@deriving sexp]

  let add_card t (card : Card.t) =
    let m : Member.t =
      { card
      ; pos_c= Card.Color.Set.of_list Card.Color.all
      ; pos_n= Card.Number.Set.of_list Card.Number.all }
    in
    m :: t

  let init deck size =
    let cards, rem_deck = List.split_n deck size in
    let hand = List.fold cards ~init:[] ~f:add_card in
    (hand, rem_deck)

  let draw_card t (deck : Card.t list) =
    match deck with [] -> None | c :: cs -> Some (add_card t c, cs)

  let is_valid_hint (t : t) hint =
    let count =
      match hint with
      | First c -> List.count t ~f:(fun m -> fst m.card = c)
      | Second n -> List.count t ~f:(fun m -> snd m.card = n)
    in
    count > 0

  let take_hint (t : t) hint =
    match hint with
    | First color ->
        List.map t ~f:(fun m ->
            if fst m.card = color then
              {m with pos_c= Card.Color.Set.singleton color}
            else {m with pos_c= Card.Color.Set.remove m.pos_c color} )
    | Second number ->
        List.map t ~f:(fun m ->
            if snd m.card = number then
              {m with pos_n= Card.Number.Set.singleton number}
            else {m with pos_n= Card.Number.Set.remove m.pos_n number} )
end

include T
