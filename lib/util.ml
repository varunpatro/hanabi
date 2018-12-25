let rec repeat x n = 
    match n with
    | 0 -> []
    | n -> x :: repeat x (n - 1)
;;