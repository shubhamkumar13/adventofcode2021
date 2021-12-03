let file name =
  let ic = open_in name in
  let rec loop acc =
    match input_line ic with
    | exception End_of_file -> List.rev acc
    | l -> loop (l :: acc)
  in
  loop []

let input =
  [ "forward 5"; "down 5"; "forward 8"; "up 3"; "down 8"; "forward 2" ]

let input () = try file Sys.argv.(1) with _ -> input

type move = Forward of int | Down of int | Up of int

type part = One | Two

type pos = { h : int; v : int; aim : int }

let init_pos () = { h = 0; v = 0; aim = 0 }

let update_pos part pos move =
  match (part, move) with
  | One, Forward x -> { pos with h = pos.h + x }
  | Two, Forward x -> { pos with h = pos.h + x; v = pos.v + (pos.aim * x) }
  | One, Down x -> { pos with v = pos.v + x }
  | Two, Down x -> { pos with aim = pos.aim + x }
  | One, Up x -> { pos with v = pos.v - x }
  | Two, Up x -> { pos with aim = pos.aim - x }

let parse_move x y =
  match String.lowercase_ascii x with
  | "forward" -> Forward y
  | "down" -> Down y
  | "up" -> Up y
  | _ -> failwith "unmatches string"

let f input =
  String.split_on_char ' ' input |> fun s ->
  parse_move (List.nth s 0) (List.nth s 1 |> int_of_string)

let output = function
  | One ->
      List.map f @@ input () |> List.fold_left (update_pos One) (init_pos ())
      |> fun m -> m.h * m.v |> Printf.printf "%d\n"
  | Two ->
      List.map f @@ input () |> List.fold_left (update_pos Two) (init_pos ())
      |> fun m -> m.h * m.v |> Printf.printf "%d\n"

let part1 () = output One

let part2 () = output Two

let _ =
  part1 ();
  part2 ()
