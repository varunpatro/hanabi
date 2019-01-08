open! Core

module Member = struct
  module T = struct
    type t =
      { card: Card.t
      ; possible_colors: Card.Color.Set.t
      ; possible_numbers: Card.Number.Set.t }
    [@@deriving compare, sexp]
  end

  include T
  include Comparable.Make (T)
end

module T = struct
  type t = Member.t list

  let add_card t (card : Card.t) =
    let m : Member.t =
      { card
      ; possible_colors= Card.Color.Set.of_list Card.Color.all
      ; possible_numbers= Card.Number.Set.of_list Card.Number.all }
    in
    m :: t

  let init deck size =
    let cards, rem_deck = List.split_n deck size in
    let hand = List.fold cards ~init:[] ~f:add_card in
    (hand, rem_deck)

  let draw_card t (deck : Card.t list) =
    match deck with [] -> (t, deck) | c :: cs -> (add_card t c, cs)

  let is_valid_hint (t : t) hint =
    let count =
      match hint with
      | First c -> List.count t ~f:(fun m -> m.card.color = c)
      | Second n -> List.count t ~f:(fun m -> m.card.number = n)
    in
    count > 0

  let take_hint (t : t) hint =
    match hint with
    | First color ->
        List.map t ~f:(fun m ->
            if m.card.color = color then
              {m with possible_colors= Card.Color.Set.singleton color}
            else
              { m with
                possible_colors= Card.Color.Set.remove m.possible_colors color
              } )
    | Second number ->
        List.map t ~f:(fun m ->
            if m.card.number = number then
              {m with possible_numbers= Card.Number.Set.singleton number}
            else
              { m with
                possible_numbers=
                  Card.Number.Set.remove m.possible_numbers number } )
end

include T
