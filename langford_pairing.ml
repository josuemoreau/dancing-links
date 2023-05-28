open Dlx

let langford_pairs_items n =
  Array.init (3 * n)
             (fun i ->
               if i < n then string_of_int (i + 1)
               else "s" ^ (string_of_int (i - n + 1)))

let langford_pairs_options n =
  let options = ref [] in
  for i = 1 to n do
    for j = 1 to 2 * n do
      for k = 1 to 2 * n do
        if k = i + j + 1 then
          options := [ string_of_int i;
                       "s" ^ string_of_int j;
                       "s" ^ string_of_int k ] :: !options
      done
    done
  done;
  Array.of_list (List.rev !options)

module StringElt : DLXElement with type t = string = struct
  type t = string
  let to_string s = s
  let pp p s = Format.fprintf p "%s" s
  let compare = String.compare
  let null = ""
end

module X = DLX(StringElt)

let () =
  if Array.length Sys.argv < 2 then
    (Format.printf "The first argument was not specified.@."; exit 1);
  let n = int_of_string (Sys.argv.(1)) in
  let items   = langford_pairs_items n in
  let options = langford_pairs_options n in
  (* Format.printf "Langford pairs:@.";
     Format.printf "items: ";
     Array.iter (Format.printf "%s ") items;
     Format.printf "@.";
     Format.printf "options:@.";
     Array.iter (fun opt ->
         Array.iter (Format.printf "%s ") opt;
         Format.printf "@.") options *)
  let x = X.create items options in
  (* X.debug := true; *)
  (* Format.printf "%a" X.pp_table x; *)
  Sys.set_signal Sys.sigint (Sys.Signal_handle (fun i ->
                                 if !X.debug then Format.printf "@.%a@." X.pp_table x;
                                 exit 1));
  X.search x
