open Dlx

let test_items =
  [| "a"; "b"; "c"; "d"; "e"; "f"; "g" |]

let test_rows =
  [|
    [| "c"; "e" |];
    [| "a"; "d"; "g" |];
    [| "b"; "c"; "f" |];
    [| "a"; "d"; "f" |];
    [| "b"; "g" |];
    [| "d"; "e"; "g" |]
  |]

module StringElt : DLXElement with type t = string = struct
  type t = string
  let to_string s = s
  let pp p s = Format.fprintf p "%s" s
  let compare = String.compare
  let null = ""
end

module X = DLX(StringElt)

let () =
  let x = X.create test_items test_rows in
  Format.printf "%a" X.pp_table x;
  X.search x
