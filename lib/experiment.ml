type target = {
  name : string;
  group : string;
  desc : string;
  grammar : Grammar.t;
}

let tm x = Grammar.Term x
let nt x = Grammar.Nonterm x

let tg group name desc start prods =
  let productions =
    List.map
      (fun (lhs, rhs, count) -> { Grammar.lhs; rhs; count })
      prods
  in
  { group; name; desc; grammar = Grammar.make ~start productions }

let targets =
  [
    tg "paper" "parens" "balanced parentheses  S -> () | (S) | SS" "S"
      [ ("S", [ tm "("; tm ")" ], 1.0); ("S", [ tm "("; nt "S"; tm ")" ], 1.0);
        ("S", [ nt "S"; nt "S" ], 1.0) ];
    tg "paper" "a2n" "even-length strings of a  S -> aa | SS" "S"
      [ ("S", [ tm "a"; tm "a" ], 1.0); ("S", [ nt "S"; nt "S" ], 1.0) ];
    tg "paper" "abn" "(ab)^n  S -> ab | SS" "S"
      [ ("S", [ tm "a"; tm "b" ], 1.0); ("S", [ nt "S"; nt "S" ], 1.0) ];
    tg "paper" "anbn" "a^n b^n  S -> ab | aSb" "S"
      [ ("S", [ tm "a"; tm "b" ], 1.0); ("S", [ tm "a"; nt "S"; tm "b" ], 1.0) ];
    tg "paper" "palindrome" "w c w^R  S -> c | aSa | bSb" "S"
      [ ("S", [ tm "c" ], 1.0); ("S", [ tm "a"; nt "S"; tm "a" ], 1.0);
        ("S", [ tm "b"; nt "S"; tm "b" ], 1.0) ];
    tg "paper" "addition" "addition strings  S -> a | b | (S) | S+S" "S"
      [ ("S", [ tm "a" ], 1.0); ("S", [ tm "b" ], 1.0);
        ("S", [ tm "("; nt "S"; tm ")" ], 1.0);
        ("S", [ nt "S"; tm "+"; nt "S" ], 1.0) ];
    tg "paper" "shape" "shape grammar  S -> dY | bYS ; Y -> a | cY" "S"
      [ ("S", [ tm "d"; nt "Y" ], 1.0);
        ("S", [ tm "b"; nt "Y"; nt "S" ], 1.0); ("Y", [ tm "a" ], 1.0);
        ("Y", [ tm "c"; nt "Y" ], 1.0) ];
    tg "paper" "basic_english"
      "basic English  S -> I am A | he T | ..." "S"
      [ ("S", [ tm "I"; tm "am"; nt "A" ], 1.0);
        ("S", [ tm "he"; nt "T" ], 1.0);
        ("S", [ tm "she"; nt "T" ], 1.0);
        ("S", [ tm "it"; nt "T" ], 1.0);
        ("S", [ tm "they"; nt "V" ], 1.0);
        ("S", [ tm "you"; nt "V" ], 1.0);
        ("S", [ tm "we"; nt "V" ], 1.0);
        ("S", [ tm "this"; nt "C" ], 1.0);
        ("S", [ tm "that"; nt "C" ], 1.0);
        ("T", [ tm "is"; nt "A" ], 1.0);
        ("V", [ tm "are"; nt "A" ], 1.0);
        ("A", [ tm "there" ], 1.0); ("A", [ tm "here" ], 1.0);
        ("C", [ tm "is"; tm "a"; nt "Z" ], 1.0);
        ("C", [ nt "Z"; nt "T" ], 1.0);
        ("Z", [ tm "man" ], 1.0); ("Z", [ tm "woman" ], 1.0) ];
    tg "synthetic" "nested_parens" "nested + concatenation  S -> a | (S) | SS" "S"
      [ ("S", [ tm "a" ], 1.0); ("S", [ tm "("; nt "S"; tm ")" ], 1.0);
        ("S", [ nt "S"; nt "S" ], 1.0) ];
    tg "synthetic" "nested_anbn" "nested a^n b^n  S -> aSb | c" "S"
      [ ("S", [ tm "a"; nt "S"; tm "b" ], 1.0); ("S", [ tm "c" ], 1.0) ];
    tg "synthetic" "expr" "arithmetic  S -> S+T | T ; T -> T*F | F ; F -> (S) | a | b" "S"
      [ ("S", [ nt "S"; tm "+"; nt "T" ], 1.0); ("S", [ nt "T" ], 1.0);
        ("T", [ nt "T"; tm "*"; nt "F" ], 1.0); ("T", [ nt "F" ], 1.0);
        ("F", [ tm "("; nt "S"; tm ")" ], 1.0); ("F", [ tm "a" ], 1.0);
        ("F", [ tm "b" ], 1.0) ];
  ]

let sample_one probs start rng max_pending =
  let rec derive pending =
    if List.length pending > max_pending then None
    else
      match pending with
      | [] -> Some []
      | Grammar.Term a :: rest -> (
          match derive rest with Some l -> Some (a :: l) | None -> None)
      | Grammar.Nonterm x :: rest -> (
          match Hashtbl.find_opt probs x with
          | None | Some [] -> None
          | Some ps ->
              let tot = List.fold_left (fun s (_, p) -> s +. p) 0.0 ps in
              let r = Random.State.float rng tot in
              let rec pick acc = function
                | [] -> fst (List.hd ps)
                | (p, pr) :: tl -> if acc +. pr >= r then p else pick (acc +. pr) tl
              in
              let p = pick 0.0 ps in
              derive (p.Grammar.rhs @ rest))
  in
  derive [ Grammar.Nonterm start ]

let sample_counts target draws seed max_len =
  let probs = Grammar.probabilities target.grammar in
  let rng = Random.State.make [| seed |] in
  let tbl = Hashtbl.create 256 in
  let order = ref [] in
  let rec go i tries =
    if i >= draws || tries > draws * 100 then ()
    else
      match sample_one probs target.grammar.Grammar.start rng (2 * max_len) with
      | Some s when List.length s >= 1 && List.length s <= max_len ->
          (match Hashtbl.find_opt tbl s with
          | Some c -> Hashtbl.replace tbl s (c + 1)
          | None ->
              Hashtbl.replace tbl s 1;
              order := s :: !order);
          go (i + 1) (tries + 1)
      | _ -> go i (tries + 1)
  in
  go 0 0;
  List.rev !order
  |> List.map (fun s ->
         { Corpus.tokens = s; count = float_of_int (Hashtbl.find tbl s) })

let initial_of_corpus corpus =
  Grammar.initial_grammar
    (List.map (fun s -> (s.Corpus.tokens, s.Corpus.count)) corpus)

type result = {
  target : target;
  train : Corpus.t;
  test : Corpus.t;
  initial : Grammar.t;
  final_grammar : Grammar.t;
  initial_score : float;
  final_score : float;
  nts : int;
  prods : int;
  heldout_avg : float;
  heldout_coverage : float;
  unparseable_test : float;
  covers_target : bool;
  precision : float;
  exact_language : bool;
  recursive : bool;
  steps : int;
  iterations : int;
}

let language_sets target final_grammar max_len =
  let tlang = Grammar.language_up_to target.grammar max_len in
  let flang = Grammar.language_up_to final_grammar max_len in
  let covers = List.for_all (fun s -> List.mem s flang) tlang in
  let precision =
    if flang = [] then 0.0
    else
      float_of_int
        (List.length (List.filter (fun s -> List.mem s tlang) flang))
      /. float_of_int (List.length flang)
  in
  let exact =
    List.length tlang = List.length flang
    && List.for_all (fun s -> List.mem s flang) tlang
  in
  (covers, precision, exact)

let run_one ?(export_dir = None) ~config ~beam_width ~max_steps ~patience ~seed
    ~max_len target =
  let train = sample_counts target 50 seed max_len in
  let test = sample_counts target 200 (seed + 1000) max_len in
  let initial = initial_of_corpus train in
  let search =
    Search.run ~config ~beam_width ~max_steps ~patience initial train
  in
  (match export_dir with
  | None -> ()
  | Some dir ->
      List.iteri
        (fun i (s : Search.step) ->
          let ch =
            Transform.change_of s.Search.parent s.Search.transform s.Search.result
          in
          let base =
            Filename.concat dir
              (Printf.sprintf "%s-step%03d" target.name (i + 1))
          in
          ignore
            (Dot.render ~out_svg:(base ^ "-before.svg")
               ~out_dot:(base ^ "-before.dot")
               (Dot.dot_grammar ~highlight_nts:ch.Transform.before_nts
                  ~highlight_prods:ch.Transform.before_prods
                  ~title:
                    (Printf.sprintf "%s step %d before: %s" target.name (i + 1)
                       (Transform.describe s.Search.transform))
                  s.Search.parent));
          ignore
            (Dot.render ~out_svg:(base ^ "-after.svg")
               ~out_dot:(base ^ "-after.dot")
               (Dot.dot_grammar ~highlight_nts:ch.Transform.after_nts
                  ~highlight_prods:ch.Transform.after_prods
                  ~title:
                    (Printf.sprintf "%s step %d after: %s" target.name (i + 1)
                       (Transform.describe s.Search.transform))
                  s.Search.result)))
        search.Search.steps);

  let final_grammar = search.Search.best in
  let heldout = Scoring.heldout ~config final_grammar ~train test in
  let heldout_coverage =
    if heldout.Scoring.total_mass > 0.0 then
      heldout.Scoring.parsed_mass /. heldout.Scoring.total_mass
    else 0.0
  in
  let covers, precision, exact = language_sets target final_grammar max_len in
  {
    target;
    train;
    test;
    initial;
    final_grammar;
    initial_score = search.Search.initial_score;
    final_score = search.Search.best_score;
    nts = List.length (Grammar.nonterminals final_grammar);
    prods = List.length final_grammar.Grammar.productions;
    heldout_avg = heldout.Scoring.average_loglik_per_token;
    heldout_coverage;
    unparseable_test = heldout.Scoring.unparseable_mass;
    covers_target = covers;
    precision;
    exact_language = exact;
    recursive = Grammar.is_recursive final_grammar;
    steps = List.length search.Search.steps;
    iterations = search.Search.iterations;
  }

let header () =
  "group\tname\tnts\tprods\tinitial_score\tfinal_score\theldout_avg\t\
   heldout_coverage\tunparseable_mass\tcovers\tprecision\texact\trecursive\tsteps\t\
   iterations"

let row r =
  Printf.sprintf
    "%s\t%s\t%d\t%d\t%.6g\t%.6g\t%.6g\t%.6g\t%.6g\t%b\t%.4f\t%b\t%b\t%d\t%d"
    r.target.group r.target.name r.nts r.prods r.initial_score r.final_score
    r.heldout_avg r.heldout_coverage r.unparseable_test r.covers_target r.precision
    r.exact_language r.recursive r.steps r.iterations

let to_tsv results = String.concat "\n" (header () :: List.map row results)

let report_markdown results =
  let b = Buffer.create 4096 in
  Buffer.add_string b "# SCFG induction experiments\n\n";
  Buffer.add_string b
    "Scores are natural-log posterior values. `heldout_avg` is the average \
     log-likelihood per token on a held-out sample drawn from the same target \
     grammar. It is negative infinity when any weighted test mass is \
     unparseable. `coverage` is the fraction of weighted held-out mass that the \
     grammar parses. `precision` is the fraction of strings the induced grammar \
     generates up to the comparison length that belong to the target language.\n\n";
  Buffer.add_string b
    "| group | target | nonterms | prods | initial | final | heldout/token | \
     coverage | covers | precision | exact | recursive | steps |\n";
  Buffer.add_string b
    "|---|---|---|---|---|---|---|---|---|---|---|---|---|\n";
  List.iter
    (fun r ->
      Buffer.add_string b
        (Printf.sprintf
           "| %s | %s | %d | %d | %.2f | %.2f | %.3f | %.3f | %b | %.3f | %b | %b | %d |\n"
           r.target.group r.target.name r.nts r.prods r.initial_score
           r.final_score r.heldout_avg r.heldout_coverage r.covers_target r.precision
           r.exact_language r.recursive r.steps))
    results;
  Buffer.add_string b "\n## Final grammars\n\n";
  List.iter
    (fun r ->
      Buffer.add_string b
        (Printf.sprintf "### %s (%s)\n\n%s\n\n" r.target.name r.target.group
           r.target.desc);
      Buffer.add_string b "```\n";
      Buffer.add_string b (Grammar.to_string r.final_grammar);
      Buffer.add_string b "\n```\n\n")
    results;
  Buffer.contents b

let run_all ?(export_dir = None) ?(config = Scoring.default_config)
    ?(beam_width = 3) ?(max_steps = 25) ?(patience = 5) ?(seed = 1)
    ?(max_len = 8) ?(only = None) () =
  let ts =
    match only with
    | None -> targets
    | Some name -> List.filter (fun t -> String.equal t.name name) targets
  in
  List.map
    (fun target ->
      run_one ~export_dir ~config ~beam_width ~max_steps ~patience ~seed ~max_len
        target)
    ts
