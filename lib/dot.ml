let escape s =
  let b = Buffer.create (String.length s) in
  String.iter
    (fun c ->
      match c with
      | '"' -> Buffer.add_string b "\\\""
      | '\\' -> Buffer.add_string b "\\\\"
      | '\n' -> Buffer.add_string b "\\n"
      | c -> Buffer.add_char b c)
    s;
  Buffer.contents b

let sanitize s =
  String.map
    (fun c ->
      if
        (c >= 'a' && c <= 'z')
        || (c >= 'A' && c <= 'Z')
        || (c >= '0' && c <= '9')
        || c = '_'
      then c
      else '_')
    s

let nt_id name = "nt_" ^ sanitize name

let prob_of probs lhs rhs =
  match Hashtbl.find_opt probs lhs with
  | None -> 0.0
  | Some l -> (
      match List.find_opt (fun (q, _) -> Grammar.equal_rhs q.Grammar.rhs rhs) l with
      | Some (_, x) -> x
      | None -> 0.0)

let prod_key p =
  (p.Grammar.lhs, String.concat " " (List.map Grammar.symbol_to_string p.Grammar.rhs))

let dot_grammar ?(highlight_nts = []) ?(highlight_prods = [])
    ?(title = "grammar") g =
  let probs = Grammar.probabilities g in
  let hl_nt name = List.mem name highlight_nts in
  let hl_prods = List.map prod_key highlight_prods in
  let buf = Buffer.create 2048 in
  Buffer.add_string buf "digraph grammar {\n";
  Buffer.add_string buf "  rankdir=LR;\n";
  Buffer.add_string buf "  labelloc=\"t\";\n";
  Buffer.add_string buf (Printf.sprintf "  label=\"%s\";\n" (escape title));
  Buffer.add_string buf
    "  node [fontname=\"Helvetica\", fontsize=10];\n  edge [fontname=\"Helvetica\", fontsize=9];\n";
  List.iter
    (fun nt ->
      let is_start = String.equal nt g.Grammar.start in
      let hl = hl_nt nt in
      Buffer.add_string buf
        (Printf.sprintf
           "  %s [label=\"%s\", shape=%s%s%s];\n"
           (nt_id nt) (escape nt)
           (if is_start then "doublecircle" else "ellipse")
           (if hl then ", color=red, penwidth=2.5, fontcolor=red" else "")
           (if is_start then ", style=bold" else "")))
    (Grammar.nonterminals g);
  List.iter
    (fun t ->
      Buffer.add_string buf
        (Printf.sprintf
           "  tm_%s [label=\"%s\", shape=plaintext, fontcolor=\"#666666\"];\n"
           (sanitize t) (escape t)))
    (Grammar.terminals g);
  List.iteri
    (fun i p ->
      let pr = prob_of probs p.Grammar.lhs p.Grammar.rhs in
      let hl = List.mem (prod_key p) hl_prods in
      let rhs_text =
        String.concat " " (List.map Grammar.symbol_to_string p.Grammar.rhs)
      in
      Buffer.add_string buf
        (Printf.sprintf
           "  pr_%d [label=\"%s\\np=%.3f\", shape=box%s];\n"
           i (escape rhs_text) pr
           (if hl then ", color=red, penwidth=2.5, fontcolor=red" else ""));
      Buffer.add_string buf
        (Printf.sprintf "  %s -> pr_%d;\n" (nt_id p.Grammar.lhs) i);
      List.iteri
        (fun k s ->
          let target =
            match s with
            | Grammar.Nonterm x -> nt_id x
            | Grammar.Term t -> "tm_" ^ sanitize t
          in
          Buffer.add_string buf
            (Printf.sprintf "  pr_%d -> %s [label=\"%d\"];\n" i target (k + 1)))
        p.Grammar.rhs)
    g.Grammar.productions;
  Buffer.add_string buf "}\n";
  Buffer.contents buf

let dot_tree ?(title = "parse") tree =
  let buf = Buffer.create 1024 in
  Buffer.add_string buf "digraph parse {\n";
  Buffer.add_string buf "  rankdir=TB;\n";
  Buffer.add_string buf (Printf.sprintf "  label=\"%s\";\n" (escape title));
  Buffer.add_string buf
    "  node [fontname=\"Helvetica\", fontsize=10, shape=ellipse];\n";
  let counter = ref 0 in
  let rec go tree =
    let id = !counter in
    incr counter;
    match tree with
    | Parse.Leaf s ->
        Buffer.add_string buf
          (Printf.sprintf "  n%d [label=\"%s\", shape=plaintext, fontcolor=\"#666666\"];\n"
             id (escape s));
        id
    | Parse.Node (nt, children) ->
        Buffer.add_string buf
          (Printf.sprintf "  n%d [label=\"%s\"];\n" id (escape nt));
        List.iteri
          (fun k child ->
            let cid = go child in
            Buffer.add_string buf
              (Printf.sprintf "  n%d -> n%d [label=\"%d\"];\n" id cid (k + 1)))
          children;
        id
  in
  ignore (go tree);
  Buffer.add_string buf "}\n";
  Buffer.contents buf

let write_file path contents =
  let oc = open_out path in
  output_string oc contents;
  close_out oc

let render ?out_svg ~out_dot dot_string =
  write_file out_dot dot_string;
  match out_svg with
  | None -> None
  | Some svg ->
      let cmd =
        Printf.sprintf "dot -Tsvg %s -o %s 2>/dev/null"
          (Filename.quote out_dot) (Filename.quote svg)
      in
      if Sys.command cmd = 0 then Some svg else None
