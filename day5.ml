let file name =
  let ic = open_in name in
  let rec loop acc =
    match input_line ic with
    | exception End_of_file -> List.rev acc
    | l -> loop (l :: acc)
  in
  loop [] |> List.fold_left (fun acc a -> acc ^ a ^ "\n") ""

let input () =
  {|
0,9 -> 5,9
8,0 -> 0,8
9,4 -> 3,4
2,2 -> 2,1
7,0 -> 7,4
6,4 -> 2,0
0,9 -> 2,9
3,4 -> 1,4
0,0 -> 8,8
5,5 -> 8,2|}

let input () = try file Sys.argv.(1) with _ -> input ()

module List = struct
  include List

  let hd_opt : 'a list -> 'a option =
   fun lst -> match lst with [] -> None | hd :: _ -> Some hd

  let range : int -> int -> int list =
   fun x y ->
    let rec loop acc start finish =
      if start <= finish then loop (start :: acc) (start + 1) finish
      else List.rev acc
    in
    if x < y then loop [] x y else loop [] y x |> List.rev

  let repeat_n : 'a -> int -> 'a list =
   fun x n -> List.map (fun _ -> x) @@ range 0 (n - 1)

  let fold : ('a -> 'a -> 'b) -> 'a list -> 'b =
   fun f lst ->
    let head =
      List.hd lst
      (* List.hd_opt lst |> fun e ->
         match e with None -> failwith "List is empty" | Some e -> e *)
    in
    let tail = List.tl lst in
    List.fold_left f head tail

  let intersection_opt :
      ('a -> 'a -> bool) -> 'a list -> 'a list -> 'a list option =
   fun f l1 l2 ->
    let common_elements =
      List.map (fun e1 -> List.filter (fun e2 -> f e1 e2) l2) l1
    in
    match common_elements with
    | [] -> None
    | _ -> Some (fold (fun acc a -> List.append acc a) common_elements)
end

let ( <.> ) (lst : 'a list) (i : int) : 'a = List.nth lst i

type point = { x : int; y : int }

type line = { p1 : point; p2 : point }

let create_point : int -> int -> point = fun x y -> { x; y }

let create_line : point -> point -> line = fun p1 p2 -> { p1; p2 }

let print_point : point -> unit = fun p -> Printf.printf "(%d,%d)" p.x p.y

let is_equal : point -> point -> bool =
 fun p1 p2 -> p1.x == p2.x && p1.y == p2.y

let max_x : point list -> int =
 fun lst ->
  let max_point = List.fold (fun acc a -> if acc.x > a.x then acc else a) lst in
  max_point.x

let min_x : point list -> int =
 fun lst ->
  let max_point = List.fold (fun acc a -> if acc.x < a.x then acc else a) lst in
  max_point.x

let max_y : point list -> int =
 fun lst ->
  let max_point = List.fold (fun acc a -> if acc.y > a.y then acc else a) lst in
  max_point.y

let min_y : point list -> int =
 fun lst ->
  let max_point = List.fold (fun acc a -> if acc.y < a.y then acc else a) lst in
  max_point.y

let print_line : line -> unit =
 fun l ->
  print_point l.p1;
  Printf.printf " -> ";
  print_point l.p2;
  Printf.printf "\n"
(* Printf.printf "(%d,%d) -> (%d,%d)\n" l.p1.x l.p1.y l.p2.x l.p2. *)

let pair_points_to_line : int * int -> int * int -> line =
 fun (x1, y1) (x2, y2) -> create_line (create_point x1 y1) (create_point x2 y2)

let parse_input : string -> line list =
 fun str ->
  let lines sep = Str.split @@ Str.regexp sep in
  let lst =
    List.map (fun row ->
        let elements = lines " -> " row in
        let elements =
          List.fold_left
            (fun acc x -> acc @ List.map int_of_string @@ lines "," x)
            [] elements
        in
        let elements =
          pair_points_to_line
            (elements <.> 0, elements <.> 1)
            (elements <.> 2, elements <.> 3)
        in
        elements)
    @@ lines "\n" str
  in
  lst

let part1 _ =
  parse_input @@ input ()
  |> List.filter (fun l -> l.p1.x == l.p2.x || l.p1.y == l.p2.y)
  (* |> fun l -> List.iter (fun l -> print_line l) l; l *)
  |> List.map (fun l ->
         match l with
         | l when l.p1.x == l.p2.x ->
             let point_list = List.range l.p1.y l.p2.y in
             (* List.iter (Printf.printf "%d, ") point_list;
                print_endline ""; *)
             let point_list =
               List.combine
                 (List.repeat_n l.p1.x (List.length point_list))
                 point_list
             in
             let point_list =
               List.map (fun (a, b) -> create_point a b) point_list
             in
             point_list
         | l when l.p1.y == l.p2.y ->
             let point_list = List.range l.p1.x l.p2.x in
             let point_list =
               List.combine point_list
                 (List.repeat_n l.p1.y (List.length point_list))
             in
             let point_list =
               List.map (fun (a, b) -> create_point a b) point_list
             in
             point_list
         | _ -> failwith "dune wants this, save me I'm held at gunpoint")
  (* |> List.iter (fun point_list ->
         List.iter print_point point_list;
         print_endline "") *)
  |>
  fun lst ->
  let l = List.concat lst in
  (* List.iter (fun l -> print_point l; Printf.printf "\n") l; *)
  let x = max_x l in
  let y = max_y l in
  let size = max x y + 1 in
  (* Printf.printf "%d\n" size; *)
  let arr = Array.make_matrix size size 0 in
  List.iter (fun p -> arr.(p.x).(p.y) <- arr.(p.x).(p.y) + 1) l;
  let counter = ref 0 in
  for i = 0 to pred @@ Array.length arr do
    for j = 0 to pred @@ Array.length arr.(0) do
      if arr.(i).(j) > 1 then incr counter else ()
    done
  done;
  Printf.printf "%d" !counter

let _ = part1 ()
