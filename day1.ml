let file name =
  let ic = open_in name in
  let rec loop acc =
    match input_line ic with
    | exception End_of_file -> List.rev acc
    | l -> loop (l :: acc)
  in
  loop [] |> List.map int_of_string

let input = [ 199; 200; 208; 210; 200; 207; 240; 269; 260; 263 ]

let input () = try file Sys.argv.(1) with _ -> input

module List = struct
  include List

  let combine lst1 lst2 =
    let rec loop acc l1 l2 =
      match (l1, l2) with
      | [], _ -> List.rev acc
      | _, [] -> List.rev acc
      | hd1 :: tl1, hd2 :: tl2 -> loop ((hd1, hd2) :: acc) tl1 tl2
    in
    loop [] lst1 lst2
end

let part1 () =
  let lst2 = List.tl @@ input () in
  let lst1 = input () in
  List.combine lst1 lst2
  |> List.filter (fun (a, b) -> a < b)
  (* |> List.iter (fun (a, b) -> Printf.printf "(%d, %d)\n" a b) *)
  |> List.length
  |> Printf.printf "%d\n"

let part2 () : unit =
  let lst3 = List.tl @@ List.tl @@ input () in
  let lst2 = List.tl @@ input () in
  let lst1 = input () in
  let lst =
    List.combine lst1 @@ List.combine lst2 lst3
    |> List.map (fun (a, (b, c)) -> (a, b, c))
  in
  let lst = List.map (fun (a, b, c) -> a + b + c) lst in
  let lst2 = List.tl lst in
  let lst1 = lst in
  List.combine lst1 lst2
  |> List.filter (fun (a, b) -> a < b)
  (* |> List.iter (fun (a, b) -> Printf.printf "(%d, %d)\n" a b) *)
  |> List.length
  |> Printf.printf "%d\n"

let _ =
  part1 ();
  part2 ()
