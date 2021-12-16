let file name =
  let ic = open_in name in
  let rec loop acc =
    match input_line ic with
    | exception End_of_file -> List.rev acc
    | l -> loop (l :: acc)
  in
  loop [] |> List.fold_left (fun acc a -> acc ^ a ^ "\n") ""

let input () =
  {|7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1

22 13 17 11  0
 8  2 23  4 24
21  9 14 16  7
 6 10  3 18  5
 1 12 20 15 19

 3 15  0  2 22
 9 18 13 17  5
19  8  7 25 23
20 11 10 24  4
14 21 16 12  6

14 21 17 24  4
10 16 15  9 19
18  8 23 26 20
22 11 13  6  5
 2  0 12  3  7
|}

let input () = try file Sys.argv.(1) with _ -> input ()

module List = struct
  include List

  let singleton : 'a -> 'a list = fun e -> [ e ]

  let transpose : 'a list list -> 'a list list =
   fun lst ->
    List.map (fun row -> List.map singleton row) lst |> fun lst ->
    let acc = List.hd lst in
    let rest = List.tl lst in
    List.fold_left
      (fun acc x ->
        List.combine acc x |> fun acc ->
        List.map (fun (a, b) -> List.append a b) acc)
      acc rest
end

type array_element = { element : int; is_marked : bool }

let create_array_element element = { element; is_marked = false }

let create_bingo_matrix : string list list -> array_element list list =
 fun lst ->
  List.map
    (fun row -> List.map (fun s -> create_array_element @@ int_of_string s) row)
    lst

let parse_bingo_matrix_row str =
  str
  |> Str.split @@ Str.regexp " +"
  |> List.filter (fun s ->
         Bool.not @@ String.equal s (String.make 1 (Char.chr 32)))

let parse_input : string -> int list * array_element list list list =
 fun str ->
  let lst = String.split_on_char '\n' str in
  let input_stream =
    List.hd lst |> String.split_on_char ',' |> List.map int_of_string
  in
  let lst = List.tl @@ List.tl lst in
  lst
  |> List.map parse_bingo_matrix_row
  |> List.map (fun row ->
         List.fold_left (fun acc x -> acc ^ x ^ "b") "" row ^ "bb")
  |> List.fold_left (fun acc x -> acc ^ x) ""
  |> Str.split (Str.regexp "bbbb")
  |> List.map (fun row -> Str.split (Str.regexp "bbb") row)
  |> List.map (fun row ->
         List.map (fun el -> Str.split (Str.regexp "b") el) row)
  |> List.rev |> List.tl |> List.rev
  |> List.map create_bingo_matrix
  |> fun lst -> (input_stream, lst)

let mark_bingo_matrix :
    int -> array_element list list -> array_element list list =
 fun input lst ->
  List.map
    (fun row ->
      List.map
        (fun e -> if e.element == input then { e with is_marked = true } else e)
        row)
    lst

let is_row_bingo : array_element list list -> bool =
 fun lst ->
  List.map
    (fun row -> List.fold_left (fun acc el -> acc && el.is_marked) true row)
    lst
  |> List.memq true

let is_col_bingo : array_element list list -> bool =
 fun lst -> lst |> List.transpose |> is_row_bingo

let is_bingo :
    (array_element list list -> bool) ->
    array_element list list list ->
    int option =
 fun f lst ->
  List.map f lst
  |> List.mapi (fun ix x -> (ix, x))
  |> List.filter (fun x -> snd x)
  |> fun lst -> match lst with [] -> None | hd :: _ -> Some (fst hd)

let sum_unmarked : int -> array_element list list -> int =
 fun last_element_marked lst ->
  List.fold_left
    (fun acc x ->
      acc
      + List.fold_left
          (fun acc' x' -> if x'.is_marked then acc' else acc' + x'.element)
          0 x)
    0 lst
  |> fun sum -> sum * last_element_marked

let part1 : unit -> unit =
 fun () ->
  input () |> parse_input |> fun (input, lst) ->
  let rec loop index input lst =
    match input with
    | [] -> [ None ]
    | hd :: tl -> (
        let lst = List.map (mark_bingo_matrix hd) lst in
        match is_bingo is_row_bingo lst with
        | None -> (
            match is_bingo is_col_bingo lst with
            | None -> loop index tl lst
            | Some i ->
                List.filteri (fun ix _ -> i == ix) lst
                |> List.map (fun e -> Some (sum_unmarked hd e)))
        | Some i ->
            List.filteri (fun ix _ -> i == ix) lst
            |> List.map (fun e -> Some (sum_unmarked hd e)))
  in

  loop None input lst
  |> List.iter (fun s ->
         match s with
         | None -> Printf.printf "None "
         | Some s -> Printf.printf "%d " s)

(* let part2 : unit -> unit = fun () -> input () |> parse_input |> fun (input, lst) ->
   let rec loop acc input lst = match input with
     | [] -> acc
     | hd :: tl -> let lst = List.map (mark_bingo_matrix hd) lst in
       let last_row_bingo_found = (List.length (List.filter is_row_bingo lst)) == (List.length lst) in
       let last_col_bingo_found = (List.length (List.filter is_col_bingo lst)) == (List.length lst) in
       begin
         match last_row_bingo_found with
         | false -> begin
           match last_col_bingo_found with
           | false -> *)

let _ = part1 ()
