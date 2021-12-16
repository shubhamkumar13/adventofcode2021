let input () =
  {|
be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe
edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc
fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg
fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb
aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea
fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb
dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe
bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef
egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb
gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce|}

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

(* 0 -> 6
   1 -> 2
   2 -> 5
   3 -> 5
   4 -> 4
   5 -> 5
   6 -> 6
   7 -> 3
   8 -> 7
   9 -> 6 *)

(* be cfbegad cgeb edb

   ([0; 0; 1]) *)

let tbl_part1 =
  let tbl = Hashtbl.create 10 in
  Hashtbl.add tbl 6 0;
  Hashtbl.add tbl 2 1;
  Hashtbl.add tbl 5 2;
  Hashtbl.add tbl 5 3;
  Hashtbl.add tbl 4 4;
  Hashtbl.add tbl 5 5;
  Hashtbl.add tbl 6 6;
  Hashtbl.add tbl 3 7;
  Hashtbl.add tbl 7 8;
  Hashtbl.add tbl 6 9;
  tbl

let parse_input : string -> (string list * string list) list =
 fun str ->
  let split sep = Str.split @@ Str.regexp sep in
  let lst =
    List.map (fun e ->
        let a, b =
          match split " | " e with
          | [ hd; tl ] -> (hd, tl)
          | _ -> failwith "can't split on | "
        in
        (split " " a, split " " b))
    @@ split "\n" str
  in
  lst

let print_input lst =
  lst
  |> List.iter (fun (a, b) ->
         List.iter (Printf.printf "%s ") a;
         Printf.printf "| ";
         List.iter (Printf.printf "%s ") b;
         Printf.printf "\n")

let part1 : unit -> unit =
 fun _ ->
  parse_input @@ input ()
  |> List.map (fun (_, b) -> List.map String.length b)
  |> List.map (fun lst ->
         List.filter
           (fun k ->
             let v = Hashtbl.find tbl_part1 k in
             v == 1 || v == 4 || v == 7 || v == 8)
           lst)
  |> List.map List.length
  |> List.fold_left (fun acc x -> acc + x) 0
  |> Printf.printf "%d\n"

let string_to_string_list : string -> string list =
 fun str ->
  String.fold_left (fun acc ch -> String.make 1 ch :: acc) [] str |> List.rev

let string_to_char_list : string -> char list = fun str ->
  String.fold_left (fun acc ch -> ch :: acc) [] str |> List.rev

module CharOrdType = struct
  type t = char
  let compare : t -> t -> int = fun c1 c2 ->
    Int.compare (Char.code c2) (Char.code c1)
end

module CharSet = Set.Make(CharOrdType)

let string_to_charset : string -> CharSet.t = fun str ->
    let set = CharSet.empty in 
    List.fold_left (fun acc ch -> CharSet.add ch acc) set @@ string_to_char_list str

let set_list = fun input ->
  List.map string_to_charset input
  |> List.sort (fun a b -> compare (CharSet.cardinal a) (CharSet.cardinal b))

let print_char_set : CharSet.t -> unit = fun lst -> CharSet.iter (Printf.printf "%c ") lst; Printf.printf "\n"

let pattern_of_one : CharSet.t list -> CharSet.t list = fun lst -> List.filter (fun set -> CharSet.cardinal set == 2) lst
let pattern_of_seven : CharSet.t list -> CharSet.t list = fun lst -> List.filter (fun set -> CharSet.cardinal set == 3) lst
let pattern_of_four : CharSet.t list -> CharSet.t list = fun lst -> List.filter (fun set -> CharSet.cardinal set == 4) lst
let pattern_of_eight : CharSet.t list -> CharSet.t list = fun lst -> List.filter (fun set -> CharSet.cardinal set == 7) lst

let pattern_of_six : CharSet.t list -> CharSet.t list = fun lst -> 
  let condition1_list = List.filter (fun s -> CharSet.cardinal s == 6) lst in
  let condition2_list = List.filter (fun s -> Bool.not @@ CharSet.subset (List.hd @@ pattern_of_one lst) s) condition1_list in
  condition2_list

let pattern_of_nine : CharSet.t list -> CharSet.t list = fun lst -> 
  let condition1_list = List.filter (fun s -> CharSet.cardinal s == 6) lst in
  let condition2_list = List.filter (fun s -> CharSet.subset (List.hd @@ pattern_of_one lst) s) condition1_list in
  let condition3_list = List.filter (fun s -> CharSet.subset (List.hd @@ pattern_of_four lst) s) condition2_list in
  condition3_list

let pattern_of_zero : CharSet.t list -> CharSet.t list = fun lst -> 
  let condition1_list = List.filter (fun s -> CharSet.cardinal s == 6) lst in
  let condition2_list = List.filter (fun s -> CharSet.subset (List.hd @@ pattern_of_one lst) s) condition1_list in
  let nine = List.hd @@ pattern_of_nine lst in
  let condition3_list = List.filter (fun s -> Bool.not @@ CharSet.equal nine s) condition2_list in
  condition3_list

let pattern_of_three : CharSet.t list -> CharSet.t list = fun lst ->
  let condition1_list = List.filter (fun s -> CharSet.cardinal s == 5) lst in
  let condition2_list = List.filter (fun s -> CharSet.subset (List.hd @@ pattern_of_seven lst) s) condition1_list in
  condition2_list

let pattern_of_five : CharSet.t list -> CharSet.t list = fun lst ->
  let condition1_list = List.filter (fun s -> CharSet.cardinal s == 5) lst in
  let nine = List.hd @@ pattern_of_nine lst in
  let three = List.hd @@ pattern_of_three lst in
  let condition2_list = List.filter (fun s -> Bool.not @@ CharSet.equal three s) condition1_list in
  let condition3_list = List.filter (fun s -> CharSet.subset s nine) condition2_list in
  condition3_list

let pattern_of_two : CharSet.t list -> CharSet.t list = fun lst ->
  let condition1_list = List.filter (fun s -> CharSet.cardinal s == 5) lst in
  let condition2_list = List.filter (fun s -> Bool.not @@ CharSet.equal s @@ List.hd @@ pattern_of_three lst) condition1_list in
  let condition3_list = List.filter (fun s -> Bool.not @@ CharSet.equal s @@ List.hd @@ pattern_of_five lst) condition2_list in
  condition3_list

let contains : (CharSet.t, int) Hashtbl.t -> CharSet.t -> int = fun tbl set ->
  let lst = Hashtbl.to_seq tbl |> List.of_seq in
  let lst = List.filter (fun (s, _) -> CharSet.equal s set) lst in
  List.hd @@ List.map snd lst

let part2 : unit -> unit = fun _ ->
  let tbl = Hashtbl.create 10 in
  parse_input @@ input ()
  |> List.map (fun (input, output) ->
      let input_char_list = set_list input in
      let output_char_list = List.map (fun str -> (str, string_to_charset str)) @@ output in
      let zero = List.hd @@ pattern_of_zero input_char_list in
      let one = List.hd @@ pattern_of_one input_char_list in
      let two = List.hd @@ pattern_of_two input_char_list in
      let three = List.hd @@ pattern_of_three input_char_list in
      let four = List.hd @@ pattern_of_four input_char_list in
      let five = List.hd @@ pattern_of_five input_char_list in
      let six = List.hd @@ pattern_of_six input_char_list in
      let seven = List.hd @@ pattern_of_seven input_char_list in
      let eight = List.hd @@ pattern_of_eight input_char_list in
      let nine = List.hd @@ pattern_of_nine input_char_list in
      Hashtbl.add tbl zero 0;
      Hashtbl.add tbl one 1;
      Hashtbl.add tbl two 2;
      Hashtbl.add tbl three 3;
      Hashtbl.add tbl four 4;
      Hashtbl.add tbl five 5;
      Hashtbl.add tbl six 6;
      Hashtbl.add tbl seven 7;
      Hashtbl.add tbl eight 8;
      Hashtbl.add tbl nine 9;
      let output_list = List.map (fun (str, set) -> (str, contains tbl set)) output_char_list in
      Hashtbl.clear tbl;
      List.fold_left (fun acc (_, i) -> (acc * 10) + i) 0 output_list
    ) 
  |> List.fold_left (fun acc x -> acc + x) 0
  |> Printf.printf "%d\n"

let _ = part1 (); part2 ()