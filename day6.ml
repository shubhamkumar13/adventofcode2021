let input () = {|3,4,3,1,2|}

let file name =
  let ic = open_in name in
  let rec loop acc =
    match input_line ic with
    | exception End_of_file -> List.rev acc
    | l -> loop (l :: acc)
  in
  loop [] |> List.fold_left (fun acc a -> acc ^ a ^ "\n") ""

let input () = try file Sys.argv.(1) with _ -> input ()

let parse_input : string -> int list =
 fun str ->
  let split sep = Str.split @@ Str.regexp sep in
  split "\n" str |> List.hd |> split "," |> List.map (fun e -> int_of_string e)

let part1 _ =
  parse_input @@ input () |> fun lst ->
  let rec loop lst days =
    match days with
    | days when days < 1 -> lst
    | _ ->
        let rec loop' acc lst =
          match lst with
          | [] -> acc
          | hd :: tl -> (
              match hd with
              | 0 -> loop' (6 :: 8 :: acc) tl
              | _ -> loop' ((hd - 1) :: acc) tl)
        in
        loop (loop' [] lst) (days - 1)
  in
  loop lst 80 |> List.length |> Printf.printf "%d\n"

let _ = part1 ()

(* let _ = parse_input @@ input () |> List.iter (Printf.printf "%d\n") *)
