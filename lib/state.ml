open! Core

module Field = struct
  type t = Card.Number.t Card.Color.Map.t

  let empty = Card.Color.Map.empty

  let size t =
    Card.Color.Map.fold t ~init:0 ~f:(fun ~key:_ ~data ->
        ( + ) (Card.Number.to_int data) )

  let is_complete t = size t = 25

  let play_card t (card : Card.t) =
    let prev_num =
      Map.find t card.color
      |> Option.map ~f:Card.Number.to_int
      |> Option.value ~default:0
    in
    let cur_num = Card.Number.to_int card.number in
    if cur_num = prev_num + 1 then
      Some (Map.update t card.color ~f:(const card.number))
    else None
end

type t =
  { hands: Hand.t list
  ; deck: Card.t list
  ; field: Field.t
  ; discard: unit list Card.Map.t
  ; num_hint: int
  ; num_bomb: int }

type action =
  | Hint of (Card.Color.t, Card.Number.t) Either.t * int
  | Discard of int
  | Play of int

let num_player_cards = function
  | `Two -> (2, 5)
  | `Three -> (3, 5)
  | `Four -> (4, 4)
  | `Five -> (5, 4)

let init num_players =
  let np, nc = num_player_cards num_players in
  let deck = Card.random_deck () in
  let rec n_hands n hs deck =
    if n <= 0 then (hs, deck)
    else
      let hand, rem_deck = Hand.init deck nc in
      n_hands (n - 1) (hand :: hs) rem_deck
  in
  let hands, deck = n_hands np [] deck in
  { hands
  ; deck
  ; field= Field.empty
  ; discard= Card.Map.empty
  ; num_hint= 0
  ; num_bomb= 0 }

let transition t action =
  let left, rem_hands = List.split_n t.hands 1 in
  let hand = List.hd_exn left in
  match action with
  | Hint (hint, player) ->
      if player < 0 || player >= List.length t.hands then
        failwith "invalid hint index"
      else if t.num_hint <= 1 then failwith "no hints left"
      else
        let left, right = List.split_n t.hands player in
        let hintee_hand = List.hd_exn right in
        let new_hintee_hand =
          if not (Hand.is_valid_hint hintee_hand hint) then
            failwith "hint positively identifies zero cards"
          else Hand.take_hint hintee_hand hint
        in
        let rem_hands = left @ [new_hintee_hand] @ right in
        let hands = rem_hands @ [hand] in
        let num_hint = t.num_hint - 1 in
        {t with hands; num_hint}
  | Discard i -> (
      let left, rest = List.split_n hand i in
      match rest with
      | [] -> failwith "invalid discard index"
      | m :: right ->
          let new_hand, deck = Hand.draw_card (left @ right) t.deck in
          let hands = rem_hands @ [new_hand] in
          let discard = Card.Map.add_multi t.discard ~key:m.card ~data:() in
          let num_hint = max t.num_hint 8 in
          {t with hands; deck; discard; num_hint} )
  | Play i -> (
      let left, rest = List.split_n hand i in
      match rest with
      | [] -> failwith "invalid play index"
      | m :: right -> (
          let new_hand, deck = Hand.draw_card (left @ right) t.deck in
          let hands = rem_hands @ [new_hand] in
          match Field.play_card t.field m.card with
          | Some field -> {t with hands; deck; field}
          | None ->
              let discard =
                Card.Map.add_multi t.discard ~key:m.card ~data:()
              in
              let num_bomb = t.num_bomb + 1 in
              {t with hands; deck; discard; num_bomb} ) )

let is_win_state t = Field.is_complete t.field

let is_dead_state t =
  t.num_bomb >= 3
  || Card.Map.existsi t.discard ~f:(fun ~key ~data ->
         List.length data >= Card.Number.count key.number )
  || List.length t.deck + List.length t.hands + Field.size t.field
     < List.length Card.all

let rec can_win_state t ~s =
  is_win_state t
  || ((not (is_dead_state t)) && can_win_state (transition t (s t)) ~s)
