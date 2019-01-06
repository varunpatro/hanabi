open! Core

type t = 
    {  hands : Hand.t list
    ;  field : Card.CardSet.t
    ;  discard : int Card.CardMap.t
    }

let nums = function
    | `Two -> 2, 5
    | `Three -> 3, 5
    | `Four -> 4, 4
    | `Five -> 5, 4
;;

let init num_players = 
    let np, nc = nums num_players in
    let deck = Card.random_deck () in
    let rec n_hands n hs deck =
        if n <= 0 then
            hs, deck
        else
            let hand, rem_deck = Hand.init deck nc in
            n_hands (n - 1) (hand :: hs) rem_deck
    in
    n_hands np [] deck
;;