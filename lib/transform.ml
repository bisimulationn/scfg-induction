type t = Merge of string * string | Chunk of Grammar.symbol list

let describe = function
  | Merge (a, b) -> Printf.sprintf "merge %s,%s" a b
  | Chunk seq ->
      Printf.sprintf "chunk (%s)"
        (String.concat " " (List.map Grammar.symbol_to_string seq))

let starts_with seq lst =
  let m = List.length seq in
  let rec go i seq lst =
    if i = 0 then true
    else
      match (seq, lst) with
      | [], _ -> true
      | _, [] -> false
      | x :: xs, y :: ys -> Grammar.equal_symbol x y && go (i - 1) xs ys
  in
  List.length lst >= m && go m seq lst

let drop n lst =
  let rec go n lst = if n <= 0 then lst else match lst with [] -> [] | _ :: r -> go (n - 1) r in
  go n lst

let replace_all seq name rhs =
  let m = List.length seq in
  let rec go acc lst count =
    match lst with
    | _ when starts_with seq lst ->
        go (Grammar.Nonterm name :: acc) (drop m lst) (count + 1)
    | [] -> (List.rev acc, count)
    | x :: rest -> go (x :: acc) rest count
  in
  go [] rhs 0

let replace_first seq name rhs =
  let m = List.length seq in
  let rec go acc lst =
    match lst with
    | _ when starts_with seq lst -> (List.rev_append acc (Grammar.Nonterm name :: drop m lst), 1)
    | [] -> (List.rev acc, 0)
    | x :: rest -> go (x :: acc) rest
  in
  go [] rhs

let fresh_name (g : Grammar.t) _base =
  let existing = Grammar.nonterminals g in
  let candidates = [ "X"; "Y"; "Z"; "W"; "V"; "U"; "P"; "Q"; "R" ] in
  match List.find_opt (fun c -> not (List.mem c existing)) candidates with
  | Some c -> c
  | None ->
      let rec loop i =
        let c = "X" ^ string_of_int i in
        if List.mem c existing then loop (i + 1) else c
      in
      loop 1

let apply_merge (g : Grammar.t) a b =
  let survivor, removed =
    if String.equal a g.Grammar.start then (a, b)
    else if String.equal b g.Grammar.start then (b, a)
    else if String.compare a b <= 0 then (a, b)
    else (b, a)
  in
  let map_sym = function
    | Grammar.Nonterm x when String.equal x removed -> Grammar.Nonterm survivor
    | s -> s
  in
  let prods =
    List.map
      (fun (p : Grammar.production) ->
        let lhs = if String.equal p.lhs removed then survivor else p.lhs in
        { p with lhs; rhs = List.map map_sym p.rhs })
      g.Grammar.productions
  in
  Grammar.copy_with g prods

let apply_chunk ?(occurrence = Scoring.All_occurrences) (g : Grammar.t) seq =
  if List.length seq < 2 then g
  else begin
    let name = fresh_name g "X" in
    let replace =
      match occurrence with
      | Scoring.All_occurrences -> replace_all
      | Scoring.First_occurrence -> replace_first
    in
    let total = ref 0 in
    let prods =
      List.map
        (fun (p : Grammar.production) ->
          let rhs, n = replace seq name p.rhs in
          total := !total + n;
          { p with rhs })
        g.Grammar.productions
    in
    let newp =
      {
        Grammar.lhs = name;
        rhs = seq;
        count = float_of_int (max 1 !total);
      }
    in
    Grammar.copy_with g (prods @ [ newp ])
  end

let apply ?occurrence (g : Grammar.t) = function
  | Merge (a, b) -> apply_merge g a b
  | Chunk seq -> apply_chunk ?occurrence g seq

let candidates (g : Grammar.t) (config : Scoring.config) =
  let nts = Grammar.nonterminals g in
  let merges =
    let rec pairs = function
      | [] -> []
      | x :: rest -> List.map (fun y -> Merge (x, y)) rest @ pairs rest
    in
    pairs nts
  in
  let counts = Hashtbl.create 256 in
  let order = ref [] in
  List.iter
    (fun (p : Grammar.production) ->
      let arr = Array.of_list p.rhs in
      let n = Array.length arr in
      for len = 2 to min config.Scoring.max_chunk n do
        for i = 0 to n - len do
          let sub = Array.to_list (Array.sub arr i len) in
          match Hashtbl.find_opt counts sub with
          | Some c -> Hashtbl.replace counts sub (c + 1)
          | None ->
              Hashtbl.replace counts sub 1;
              order := sub :: !order
        done
      done)
    g.Grammar.productions;

  let chunks =
    List.rev !order
    |> List.filter (fun k -> Hashtbl.find counts k >= 1)
    |> List.map (fun seq -> Chunk seq)
  in
  merges @ chunks

type change = {
  before_nts : string list;
  after_nts : string list;
  before_prods : Grammar.production list;
  after_prods : Grammar.production list;
}

let change_of (before : Grammar.t) t (after : Grammar.t) =
  match t with
  | Merge (a, b) ->
      let survivor =
        if String.equal a before.Grammar.start then a
        else if String.equal b before.Grammar.start then b
        else if String.compare a b <= 0 then a
        else b
      in
      {
        before_nts = [ a; b ];
        after_nts = [ survivor ];
        before_prods =
          List.filter
            (fun p -> String.equal p.Grammar.lhs a || String.equal p.Grammar.lhs b)
            before.Grammar.productions;
        after_prods =
          List.filter
            (fun p -> String.equal p.Grammar.lhs survivor)
            after.Grammar.productions;
      }
  | Chunk seq ->
      let name = fresh_name before "X" in
      let affected_before =
        List.filter (fun p -> starts_with seq p.Grammar.rhs) before.Grammar.productions
      in
      let affected_after =
        List.filter
          (fun p ->
            String.equal p.Grammar.lhs name
            || List.exists
                 (function
                   | Grammar.Nonterm x -> String.equal x name
                   | Grammar.Term _ -> false)
                 p.Grammar.rhs)
          after.Grammar.productions
      in
      {
        before_nts = name :: List.map (fun p -> p.Grammar.lhs) affected_before;
        after_nts = [ name ];
        before_prods = affected_before;
        after_prods = affected_after;
      }
