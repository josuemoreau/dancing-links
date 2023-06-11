open Effect
open Effect.Deep
open Dlx_effects

module StringElt : DLXElement with type t = string = struct
  type t = string
  let to_string s = s
  let pp p s = Format.fprintf p "%s" s
  let compare = String.compare
  let null = ""
end

let items n =
  let items = Array.make (6 * n - 2) "" in
  for i = 0 to n - 1 do
    items.(i) <- "l" ^ string_of_int i
  done;
  for i = 0 to n - 1 do
    items.(n + i) <- "c" ^ string_of_int i
  done;
  for i = 0 to 2 * n - 2 do
    items.(2 * n + i) <- "d" ^ string_of_int (n - 1 - i)
  done;
  for i = 0 to 2 * n - 2 do
    items.(4 * n - 1 + i) <- "ad" ^ string_of_int (n - 1 - i)
  done;
  items

let options n =
  let options = ref [] in
  for i = 0 to n - 1 do
    for j = 0 to n - 1 do
      let l = ["l" ^ string_of_int i;
               "c" ^ string_of_int j;
               "d" ^ string_of_int (i - j);
               "ad" ^ string_of_int ((i + j) - n + 1)] in
      options := l :: !options
    done
  done;
  for i = -n + 1 to n - 1 do
    options := ["d" ^ string_of_int i] ::
               ["ad" ^ string_of_int i] ::
               !options
  done;
  Array.of_list (List.rev !options)

module X = DLX(StringElt)

let print_solution s =
  let n = (List.length s + 2) / 3 in
  let nn = n * n in
  let a = Array.make_matrix n n false in
  List.iter (fun r -> if r < nn then a.(r / n).(r mod n) <- true) s;
  for i = 0 to n - 1 do Format.printf "|---" done;
  Format.printf "|@.";
  for y = 0 to n - 1 do
    for x = 0 to n - 1 do
      if a.(y).(x) then Format.printf "| X "
      else Format.printf "|   "
    done;
    Format.printf "|@.";
    for i = 0 to n - 1 do Format.printf "|---" done;
    Format.printf "|@."
  done
  (* Format.printf "@." *)

let get1char () =
    let termio = Unix.tcgetattr Unix.stdin in
    let () =
        Unix.tcsetattr Unix.stdin Unix.TCSADRAIN
            { termio with Unix.c_icanon = false; Unix.c_echo = false } in
    let res = input_char stdin in
    Unix.tcsetattr Unix.stdin Unix.TCSADRAIN termio;
    res

let () =
  if Array.length Sys.argv < 2 then
    (Format.printf "The first argument was not specified.@."; exit 1);
  let n = int_of_string (Sys.argv.(1)) in
  let items   = items n in
  let options = options n in
  let x = X.create items options in
  X.enum := true;
  Sys.set_signal Sys.sigint (Sys.Signal_handle (fun i ->
                                 if !X.debug then Format.printf "@.%a@." X.pp_table x;
                                 exit 1));
  try_with X.search x
    { effc = fun (type a) (eff: a t) ->
          match eff with
          | X.Solution (s, d) -> Some (fun (k: (a, _) continuation) ->
              Format.printf "Solution found in %f seconds (P: Print) (S: Skip once) (A: Skip all): @." d;
              let c = get1char () in
              match Char.lowercase_ascii c with
              | 'p' -> print_solution s; continue k true
              | 's' -> continue k true
              | 'a' -> continue k false
              | _   -> continue k true)
          | _ -> None }
