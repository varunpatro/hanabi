open Core
open Lib

let simulate ~num_games num_players =
  let ng = Option.value num_games ~default:1 in
  let count =
    List.range 0 ng
    |> List.map ~f:(fun _ ->
           State.can_win_state (State.init num_players) ~s:Strategy.naive )
    |> List.count ~f:Fn.id
  in
  let percentage = Float.of_int (count * 100) /. Float.of_int ng in
  printf "%d / %d = %f%%\n" count ng percentage

let command =
  Command.basic ~summary:"Simulate hanabi games"
    Command.Let_syntax.(
      let%map_open num_games =
        flag "-n" (optional int) ~doc:"number of games to simulate"
      in
      fun () -> simulate ~num_games `Three)

let () = Command.run command
