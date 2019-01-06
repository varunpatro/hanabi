open! Core

module Member = struct
    module T = struct
        type t = 
        {  card : Card.t
        ;  possible_colors : Card.ColorSet.t
        ;  possible_numbers : Card.NumberSet.t
        } [@@deriving compare, sexp]
    end

    include T
    include Comparable.Make(T)
end

module T = struct
    type t = { cards : Member.t list }

    let add_card t (card : Card.t) =
        let m : Member.t = 
                {  card
                ;  possible_colors = Card.ColorSet.empty
                ;  possible_numbers = Card.NumberSet.empty
                }
        in
        { cards = m :: t.cards }
    ;;

    let draw_card t (deck : Card.t list) = 
        match deck with 
        | [] -> t, deck
        | c :: cs -> add_card t c, cs
    ;;

    let init deck size =
        let cards, rem_deck = List.split_n deck size in
        let hand = List.fold cards ~init:{ cards = [] } ~f:add_card in
        hand, rem_deck
    ;;
end

include T