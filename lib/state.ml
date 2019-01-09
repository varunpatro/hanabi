open! Core

module Field = struct
  type t = Card.Number.t Card.Color.Map.t [@@deriving sexp]

  let format_t fmt s =
    Caml.Format.pp_print_string fmt (Sexp.to_string_hum @@ sexp_of_t s)

  type card_state = Discardable | Playable_now | Playable_later

  let empty = Card.Color.Map.empty

  let size t =
    Card.Color.Map.fold t ~init:0 ~f:(fun ~key:_ ~data ->
        ( + ) (Card.Number.to_int data) )

  let is_complete t = size t = 25

  let card_state t (card : Card.t) =
    let prev_num =
      Map.find t (fst card)
      |> Option.map ~f:Card.Number.to_int
      |> Option.value ~default:0
    in
    let cur_num = Card.Number.to_int (snd card) in
    let diff = cur_num - prev_num in
    if diff <= 0 then Discardable
    else if diff = 1 then Playable_now
    else Playable_later

  let play_card t card =
    let color, number = card in
    match card_state t card with
    | Discardable | Playable_later -> None
    | Playable_now -> Some (Map.update t color ~f:(const number))
end

type t =
  { hands: Hand.t list
  ; deck: Card.t list
  ; field: Field.t
  ; discard: unit list Card.Map.t
  ; num_hint: int
  ; num_bomb: int }
[@@deriving sexp]

type action =
  | Hint of (Card.Color.t, Card.Number.t) Either.t * int
  | Discard of int
  | Play of int
[@@deriving sexp]

let format_t fmt s =
  Caml.Format.pp_print_string fmt (Sexp.to_string_hum @@ sexp_of_t s)

let format_to_string t = sprintf !"%{sexp:t}" t

let players_of_int i =
  match i with
  | 2 -> `Two
  | 3 -> `Three
  | 4 -> `Four
  | 5 -> `Five
  | _ -> failwith "invalid player count"

let players_of_string s = Int.of_string s |> players_of_int

let num_players = function `Two -> 2 | `Three -> 3 | `Four -> 4 | `Five -> 5

let num_cards = function `Two | `Three -> 5 | `Four | `Five -> 4

let init players =
  let np, nc = (num_players players, num_cards players) in
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
  ; num_hint= 8
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
          let discard = Card.Map.add_multi t.discard ~key:m.card ~data:() in
          let num_hint = min (t.num_hint + 1) 8 in
          let hands, deck =
            match Hand.draw_card (left @ right) t.deck with
            | None -> (rem_hands, t.deck)
            | Some (hand, deck) -> (rem_hands @ [hand], deck)
          in
          {t with hands; deck; discard; num_hint} )
  | Play i -> (
      let left, rest = List.split_n hand i in
      match rest with
      | [] -> failwith "invalid play index"
      | m :: right -> (
          let hands, deck =
            match Hand.draw_card (left @ right) t.deck with
            | None -> (rem_hands, t.deck)
            | Some (hand, deck) -> (rem_hands @ [hand], deck)
          in
          match Field.play_card t.field m.card with
          | Some field ->
              let num_hint_extra =
                if snd m.card = Card.Number.N_5 then 1 else 0
              in
              let num_hint = min (t.num_hint + num_hint_extra) 8 in
              {t with hands; deck; field; num_hint}
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
         List.length data >= Card.Number.count (snd key) )
  || List.length t.deck + List.length t.hands + Field.size t.field
     < List.length Card.all

let rec can_win_state ~s t =
  is_win_state t
  || ((not (is_dead_state t)) && can_win_state ~s (transition t (s t)))
