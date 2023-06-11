open Effect
open Effect.Deep

module type DLXElement = sig
  type t
  val to_string : t -> string
  val pp : Format.formatter -> t -> unit
  val compare : t -> t -> int
  val null : t
end

type node_type = NotInitialized | Header | Option | Spacer

type node = {
    mutable t: node_type;
    mutable tl: int;
    mutable up: int;
    mutable down: int
}

type 'a item = {
    obj:   'a;
    mutable left:  int;
    mutable right: int
}

exception InitError
exception WrongTop of int
exception WrongLen of int

module type S = sig
  type elt
  type t

  val debug: bool ref

  val create: elt array -> elt list array -> t

  val pp_table: Format.formatter -> t -> unit

  val search: t -> unit
end

module DLX (E : DLXElement) = struct
  module EMap = Map.Make(E)
  type elt = E.t
  type _ Effect.t += Solution: (int list * float) -> bool Effect.t
  type t = {
      items:      elt item array;
      options:    node array;
      rows:       elt list array;
      corresp:    int EMap.t;
      rcorresp:   int array;
      nb_options: int;
      nb_items:   int;
      mutable nb_solutions: int;
      mutable time: float
  }

  let debug = ref false
  let enum  = ref false

  let create (items: elt array) (rows: elt list array) : t =
    let nb_items = Array.length items in
    let items' = Array.init (nb_items + 1) (fun i ->
                              if i = 0 then
                                { obj = E.null; left = nb_items; right = 1 }
                              else
                                { obj = items.(i - 1);
                                  left = i - 1; right = (i + 1) mod (nb_items + 1) }) in
    let nb_options = Array.fold_left (fun n r ->
                              n + List.length r + 1
                            ) (nb_items + 2) rows in
    let options = Array.init nb_options (fun _ -> { t  = NotInitialized;
                                                    tl = 0; up   = -1; down = -1 }) in
    let i = ref 0 in
    let corresp = Array.fold_left (fun c e ->
                      incr i;
                      (* options.(!i).t    <- NotInitialized; *)
                      options.(!i).up   <- !i;
                      options.(!i).down <- !i;
                      EMap.add e !i c) EMap.empty items in
    let rcorresp = Array.make nb_options (-1) in
    incr i;
    assert (!i = nb_items + 1);
    Array.iteri (fun l r ->
        let j = !i in
        options.(j).t <- Spacer;
        List.iter (fun e ->
            incr i;
            (* Format.printf "%d : " !i; *)
            rcorresp.(!i) <- l;
            match EMap.find e corresp with
            | k ->
              (* Format.printf "id of %s is %d@." (E.to_string e) k; *)
              options.(!i).t <- Option;
              options.(!i).tl <- k;
              options.(!i).up <- options.(k).up;
              options.(!i).down <- k;
              options.(options.(k).up).down <- !i;
              options.(k).tl <- options.(k).tl + 1;
              options.(k).up <- !i;
              match options.(k).t with
              | NotInitialized ->
                options.(k).down <- !i;
                options.(k).t    <- Header;
              | _ -> ()
            | exception Not_found -> raise InitError) r;
        incr i;
        options.(!i).t    <- Option;
        options.(!i).tl   <- -(l + 1);
        options.(!i).up   <- j + 1;
        options.(!i).down <- !i - 1;
        options.(j).down  <- !i - 1
      ) rows;
    options.(nb_options - 1).down <- -1;
    { items      = items';
      options    = options;
      rows       = rows;
      corresp    = corresp;
      rcorresp   = rcorresp;
      nb_options = nb_options;
      nb_items   = nb_items;
      nb_solutions = 0;
      time = 0.0 }

  let pp_table p (t: t) : unit =
    let b = ref 0 in
    let iter fname f =
      Format.fprintf p "%s:" fname;
      for i = String.length fname to 5 do Format.fprintf p " " done;
      Format.fprintf p "| ";
      for i = !b to min (!b + t.nb_items) (t.nb_options - 1) do f i done;
      Format.fprintf p "@." in
    let line () =
      Format.fprintf p "--------";
      for i = !b to min (!b + t.nb_items) (t.nb_options - 1) do
        Format.fprintf p "--------"
      done;
      Format.fprintf p "@." in
    Format.fprintf p "items: %d    options: %d@." t.nb_items t.nb_options;
    line ();
    iter "i" (fun i -> Format.fprintf p "%5d | " i);
    iter "obj" (fun i -> Format.fprintf p "%5s | " (E.to_string t.items.(i).obj));
    iter "llink" (fun i -> Format.fprintf p "%5d | " t.items.(i).left);
    iter "rlink" (fun i -> Format.fprintf p "%5d | " t.items.(i).right);
    line ();
    for l = 0 to t.nb_options / (t.nb_items + 1) do
      iter "x" (fun i -> Format.fprintf p "%5d | " i);
      iter (if !b = 0 then "len" else "top")
           (fun i -> Format.fprintf p "%5d | " t.options.(i).tl);
      iter "ulink" (fun i -> Format.fprintf p "%5d | " t.options.(i).up);
      iter "dlink" (fun i -> Format.fprintf p "%5d | " t.options.(i).down);
      line ();
      b := !b + t.nb_items + 1
    done

  let top t i =
    match t.options.(i).t with
    | Option | Spacer -> t.options.(i).tl
    | _ -> raise (WrongTop i)
  let len t i =
    match t.options.(i).t with
    | Header -> t.options.(i).tl
    | _ -> raise (WrongLen i)
  let ulink t i = t.options.(i).up
  let dlink t i = t.options.(i).down
  let llink t i = t.items.(i).left
  let rlink t i = t.items.(i).right

  let hide t p =
    let q = ref (p + 1) in
    while !q <> p do
      let x = top t !q in
      let u = ulink t !q in
      let d = dlink t !q in
      if x <= 0 then q := u
      else (t.options.(u).down <- d;
            t.options.(d).up   <- u;
            t.options.(x).tl   <- len t x - 1;
            incr q)
    done

  let cover t i =
    let p = ref (dlink t i) in
    if !debug then Format.printf "[h";
    while !p <> i do
      if !debug then Format.printf " %d" !p;
      hide t !p; p := dlink t !p
    done;
    if !debug then Format.printf "]";
    let l = llink t i in
    let r = rlink t i in
    t.items.(l).right <- r;
    t.items.(r).left  <- l

  let unhide t p =
    let q = ref (p - 1) in
    while !q <> p do
      let x = top t !q in
      let u = ulink t !q in
      let d = dlink t !q in
      if x <= 0 then q := d
      else (t.options.(u).down <- !q;
            t.options.(d).up   <- !q;
            t.options.(x).tl   <- len t x + 1;
            decr q)
    done

  let uncover t i =
    let l = llink t i in
    let r = rlink t i in
    t.items.(l).right <- i;
    t.items.(r).left  <- i;
    let p = ref (ulink t i) in
    while !p <> i do unhide t !p; p := ulink t !p done

  let pp_row p r =
    List.iter (Format.fprintf p "%a " E.pp) r

  let rec pp_solution p (t, x) =
    match x with
    | [] -> ()
    | x :: [] ->
      let r = t.rcorresp.(x) in
      Format.fprintf p "@[row %d: @[%a@]@]" r pp_row t.rows.(r)
    | x :: xs ->
      let r = t.rcorresp.(x) in
      Format.fprintf p "%a@,@[row %d: @[%a@]@]" pp_solution (t, xs) r pp_row t.rows.(r)

  let choose t =
    let m = ref (rlink t 0) in
    let p = ref (rlink t !m) in
    while !p <> 0 do
      if len t !p < len t !m then m := !p;
      p := rlink t !p
    done;
    !m

  let time = ref 0.0

  let rec step t x l =
    if rlink t 0 = 0 then begin
      (* A solution have been found ! *)
      t.nb_solutions <- t.nb_solutions + 1;
      let sol_time = Unix.gettimeofday () -. !time in
      t.time <- t.time +. sol_time;
      if !debug then
        Format.printf "\027[31m@,Solution: @[<v>%a@]\027[39m@." pp_solution (t, x);
      if !enum then begin
        let sol = List.map (fun i -> t.rcorresp.(i)) x in
        enum := perform (Solution (sol, sol_time))
      end;
      time := Unix.gettimeofday ()
    end else begin
      let i = choose t in
      if !debug then Format.printf "@[- %d @[<v 0>" i;
      cover t i;
      if !debug then Format.printf " ";
      if !debug && dlink t i = i then Format.printf "X";
      explore t i (dlink t i) x l;
      (* We tried all possibilities for item i *)
      uncover t i;
      if !debug then Format.printf "@]@]"
    end
  and explore t i xl x l =
    if xl = i then ()
    else begin
      if !debug then Format.printf "@,@[(%d ->" xl;
      let p = ref (xl + 1) in
      while !p <> xl do
        let j = top t !p in
        if j <= 0 then p := ulink t !p
        else (if !debug then Format.printf " %d" j; cover t j; incr p)
      done;
      if !debug then Format.printf ")@]@,";
      step t (xl :: x) (l + 1);
      let p = ref (xl - 1) in
      while !p <> xl do
        let j = top t !p in
        if j <= 0 then p := dlink t !p
        else (uncover t j; decr p)
      done;
      explore t i (dlink t xl) x l
    end

  let search t =
    time := Unix.gettimeofday ();
    step t [] 0;
    t.time <- t.time +. (Unix.gettimeofday () -. !time);
    Format.printf "@,End of search: %d solutions in %.3f seconds.@." t.nb_solutions t.time

end
