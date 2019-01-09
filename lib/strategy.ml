open Core

let naive _ = State.Play 0

(* Play the first playable card. If none, then discard first discardable card. *)
let greedy_god (state : State.t) =
  let open State.Field in
  let card_statuses =
    List.hd_exn state.hands
    |> List.map ~f:(fun c -> c.card)
    |> List.map ~f:(card_state state.field)
  in
  match List.findi card_statuses ~f:(fun _ -> ( = ) Playable_now) with
  | Some (i, _) -> State.Play i
  | None -> (
    match List.findi card_statuses ~f:(fun _ -> ( = ) Discardable) with
    | Some (i, _) -> State.Discard i
    | None -> (
      match List.findi card_statuses ~f:(fun _ -> ( = ) Playable_later) with
      | Some (i, _) -> State.Discard i
      | None -> failwith "impossible state" ) )

let init ?(log = false) name =
  let s = match name with `Naive -> naive | `Greedy_god -> greedy_god in
  fun state ->
    let action = s state in
    let () =
      if log then
        let () =
          print_endline "==================================================="
        in
        let () = State.format_to_string state |> print_endline in
        printf !"%{sexp:State.action}\n" action
      else ()
    in
    action
