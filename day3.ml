let file name =
  let ic = open_in name in
  let rec loop acc =
    match input_line ic with
    | exception End_of_file -> List.rev acc
    | l -> loop (l :: acc)
  in
  loop []

let to_list_singleton a = [ a ]

let to_char_list str = str |> String.to_seq |> List.of_seq

let of_char_list chr_lst = chr_lst |> List.to_seq |> String.of_seq

let binary_to_decimal b = "0b" ^ b |> int_of_string

(* [[0], [0], [1], [0], [0]] <+> [[1], [1], [1], [1], [0]] == [[0,1], [0,1], [1,1], [0,1], [0,0]]
   => List.combine |> List.map (fun (a,b) -> List.append a b) *)
let ( <@> ) lst1 lst2 =
  List.combine lst1 lst2 |> List.map (fun (a, b) -> List.append a b)

let transpose_list lst =
  let lst = lst |> List.map (fun l -> List.map to_list_singleton l) in
  let head = List.hd lst in
  let tail = List.tl lst in
  List.fold_left (fun acc a -> acc <@> a) head tail

module List = struct
  include List

  let count of_ lst = lst |> List.filter (fun a -> a == of_) |> List.length
end

let most_common lst =
  if List.count '0' lst <= List.count '1' lst then '1' else '0'

let least_common lst =
  if List.count '0' lst <= List.count '1' lst then '0' else '1'

let gamma_rate lst_of_lst =
  lst_of_lst
  |> List.fold_left (fun acc lst -> most_common lst :: acc) []
  |> List.rev |> of_char_list |> binary_to_decimal

let epsilon_rate lst_of_lst =
  lst_of_lst
  |> List.fold_left (fun acc lst -> least_common lst :: acc) []
  |> List.rev |> of_char_list |> binary_to_decimal

let enumerate_list lst = List.mapi (fun i _ -> i) lst

let filter_out_suspects filter_fun lst index =
  if List.length lst == 1 then lst
  else
    let res = transpose_list lst |> fun l -> List.nth l index |> filter_fun in
    match res with
    | '0' -> List.filter (fun x -> List.nth x index == '0') lst
    | '1' -> List.filter (fun x -> List.nth x index == '1') lst
    | _ -> failwith "unexpected output"

let oxygen_generator_rating (lst : char list list) =
  let lst_col_index = enumerate_list @@ List.hd lst in
  List.fold_left
    (fun acc i -> filter_out_suspects most_common acc i)
    lst lst_col_index
  |> List.hd |> of_char_list |> binary_to_decimal

let co2_scrubber_rating (lst : char list list) =
  let lst_col_index = enumerate_list @@ List.hd lst in
  List.fold_left
    (fun acc i -> filter_out_suspects least_common acc i)
    lst lst_col_index
  |> List.hd |> of_char_list |> binary_to_decimal

let input () =
  [
    "00100";
    "11110";
    "10110";
    "10111";
    "10101";
    "01111";
    "00111";
    "11100";
    "10000";
    "11001";
    "00010";
    "01010";
  ]

let input () = try file Sys.argv.(1) with _ -> input ()

let part1 () =
  input () |> List.map to_char_list |> transpose_list |> fun l ->
  gamma_rate l * epsilon_rate l |> Printf.printf "%d\n"

let part2 () =
  input () |> List.map to_char_list |> fun l ->
  oxygen_generator_rating l * co2_scrubber_rating l |> Printf.printf "%d\n"

let _ =
  part1 ();
  part2 ()
