open Dlx

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
  (* Array.iter (Format.printf "%s@.") items; *)
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

let () =
  if Array.length Sys.argv < 2 then
    (Format.printf "The first argument was not specified.@."; exit 1);
  let n = int_of_string (Sys.argv.(1)) in
  let items   = items n in
  let options = options n in
  let x = X.create items options in
  Sys.set_signal Sys.sigint (Sys.Signal_handle (fun i ->
                                 if !X.debug then Format.printf "@.%a@." X.pp_table x;
                                 exit 1));
  X.search x
