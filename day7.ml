let input () = {|16,1,2,0,4,2,7,1,2,14|}

module List = struct
  include List

  let range : int -> int -> int list =
   fun x y ->
    let rec loop acc start finish =
      if start <= finish then loop (start :: acc) (start + 1) finish
      else List.rev acc
    in
    if x < y then loop [] x y else loop [] y x |> List.rev

  let repeat_n : 'a -> int -> 'a list =
   fun x n -> List.map (fun _ -> x) @@ range 0 (n - 1)
end

let file name =
  let ic = open_in name in
  let rec loop acc =
    match input_line ic with
    | exception End_of_file -> List.rev acc
    | l -> loop (l :: acc)
  in
  loop [] |> List.fold_left (fun acc a -> acc ^ a ^ "\n") ""

let input () = try file Sys.argv.(1) with _ -> input ()

let split : string -> string -> string list =
 fun str sep -> Str.split (Str.regexp sep) str

let parse_input : string -> int list =
 fun str ->
  split str "\n" |> List.hd |> fun str ->
  split str "," |> List.map int_of_string

let part1 _ =
  let lst = parse_input @@ input () in
  let max_element = List.fold_left (fun acc x -> max acc x) (-1) lst in
  let min_element = List.fold_left (fun acc x -> min acc x) max_element lst in
  let least_sum =
    List.fold_left
      (fun acc e ->
        let l = List.combine (List.repeat_n e (List.length lst)) lst in
        min acc @@ List.fold_left (fun acc (a, b) -> acc + (abs @@ (a - b))) 0 l)
      Int.max_int
    @@ List.range min_element max_element
  in
  Printf.printf "%d\n" least_sum

let _ = part1 ()
