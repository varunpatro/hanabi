open Core
open Lib

let simulate ~log ~num_games ~num_players =
  let count =
    List.range 0 num_games
    |> List.map ~f:(fun _ ->
           State.can_win_state
             ~s:(Strategy.init ~log `Greedy_god)
             (State.init num_players) )
    |> List.count ~f:Fn.id
  in
  let percentage = Float.of_int (count * 100) /. Float.of_int num_games in
  printf "%d / %d = %f%%\n" count num_games percentage

let command =
  Command.basic ~summary:"Simulate hanabi games"
    Command.Let_syntax.(
      let%map_open num_games =
        flag "-num-games"
          (optional_with_default 1 int)
          ~doc:"number of games to simulate"
      and num_players =
        flag "-num-players"
          (optional_with_default `Three
             (Arg_type.create State.players_of_string))
          ~doc:"number of players"
      and log = flag "-l" no_arg ~doc:"log states" in
      fun () -> simulate ~log ~num_games ~num_players)

let () = Command.run command
