type player_count = Two | Three | Four | Five

let num_cards = function
    | Two | Three -> 5
    | Four | Five -> 4
;;

type hand = 
    | Four of Card.t option * Card.t option * Card.t option * Card.t option
    | Five of Card.t option * Card.t option * Card.t option * Card.t option * Card.t option



