type score_mode = Approximate_evidence | Maximum_likelihood | Variational

type chunk_occurrence = All_occurrences | First_occurrence

type config = {
  alpha : float;
  prior_weight : float;
  em_iters : int;
  em_tol : float;
  max_chunk : int;
  mode : score_mode;
  chunk_occurrence : chunk_occurrence;
}

let default_config =
  {
    alpha = 0.1;
    prior_weight = 1.0;
    em_iters = 8;
    em_tol = 1e-7;
    max_chunk = 2;
    mode = Approximate_evidence;
    chunk_occurrence = All_occurrences;
  }

let rec log_gamma x =
  let g = 7.0 in
  let c =
    [|
      0.99999999999980993;
      676.5203681218851;
      -1259.1392167224028;
      771.32342877765313;
      -176.61502916214059;
      12.507343278686905;
      -0.13857109526572012;
      9.9843695780195716e-6;
      1.5056327351493116e-7;
    |]
  in
  if x < 0.5 then
    log (Float.pi /. sin (Float.pi *. x)) -. log_gamma (1.0 -. x)
  else begin
    let x = x -. 1.0 in
    let a = ref c.(0) in
    for i = 1 to 8 do
      a := !a +. (c.(i) /. (x +. float_of_int i))
    done;
    let t = x +. g +. 0.5 in
    (0.5 *. log (2.0 *. Float.pi))
    +. ((x +. 0.5) *. log t)
    -. t +. log !a
  end

let rec digamma x =
  if x < 6.0 then digamma (x +. 1.0) -. (1.0 /. x)
  else
    let inv = 1.0 /. x in
    let inv2 = inv *. inv in
    log x -. (0.5 *. inv)
    -. (inv2
       *. (1.0 /. 12.0
          -. (inv2
             *. (1.0 /. 120.0
                -. (inv2
                   *. (1.0 /. 252.0
                      -. (inv2 *. (1.0 /. 240.0 -. (inv2 *. (1.0 /. 132.0))))))))))

let bits x = if x <= 0.0 then 0.0 else log x /. log 2.0

let description_length_bits (g : Grammar.t) =
  let nts = Grammar.nonterminals g in
  let ts = Grammar.terminals g in
  let nn = float_of_int (List.length nts) in
  let nt_terms = float_of_int (List.length ts) in
  let bnt = bits nn in
  let bt = bits nt_terms in
  let total = ref (bits nn +. bits nt_terms) in
  List.iter
    (fun a ->
      let ps = Grammar.productions_of g a in
      total := !total +. bits (float_of_int (List.length ps));
      List.iter
        (fun (p : Grammar.production) ->
          total := !total +. bits (float_of_int (List.length p.rhs));
          List.iter
            (function
              | Grammar.Nonterm _ -> total := !total +. bnt
              | Grammar.Term _ -> total := !total +. bt)
            p.rhs)
        ps)
    nts;
  !total

let structural_logprior ?(config = default_config) (g : Grammar.t) =
  -.config.prior_weight *. description_length_bits g *. log 2.0

let approximate_log_evidence ?(config = default_config) (g : Grammar.t)
    (corpus : Corpus.t) =

  let uprep = Parse.prepare_uniform g in
  if
    not
      (List.for_all
         (fun s -> Option.is_some (Parse.log_inside uprep s.Corpus.tokens))
         corpus)
  then (neg_infinity, neg_infinity, [||], Parse.prepare g)
  else begin
  let prep = Parse.prepare g in
  ignore (Parse.fit_em prep corpus ~iters:config.em_iters ~tol:config.em_tol);

  let ll =
    List.fold_left
      (fun acc s ->
        match Parse.log_inside prep s.Corpus.tokens with
        | Some probability -> acc +. (s.Corpus.count *. probability)
        | None -> neg_infinity)
      0.0 corpus
  in
  if ll = neg_infinity then (neg_infinity, neg_infinity, [||], prep)
  else begin

    let np = Array.length prep.prods in
    let counts = Array.make np 0.0 in
    List.iter
      (fun s ->
        let cnt, _ = Parse.expected_counts prep s.Corpus.tokens in
        Array.iteri
          (fun i x -> counts.(i) <- counts.(i) +. (s.Corpus.count *. x))
          cnt)
      corpus;
    let dirichlet = ref 0.0 and ml_count = ref 0.0 in
    Hashtbl.iter
      (fun _ idxs ->
        let k = List.length idxs in
        if k > 0 then begin
          let n = List.fold_left (fun s i -> s +. counts.(i)) 0.0 idxs in
          if n > 0.0 then begin
            dirichlet :=
              !dirichlet
              +. (log_gamma (float_of_int k *. config.alpha)
                 -. log_gamma (n +. (float_of_int k *. config.alpha)));
            List.iter
              (fun i ->
                if counts.(i) > 0.0 then begin
                  dirichlet :=
                    !dirichlet
                    +. (log_gamma (counts.(i) +. config.alpha)
                       -. log_gamma config.alpha);
                  ml_count :=
                    !ml_count +. (counts.(i) *. log (counts.(i) /. n))
                end)
              idxs
          end
        end)
      prep.by_lhs;
    let occam = !dirichlet -. !ml_count in
    (ll +. occam, ll, counts, prep)
  end
  end

let variational_loglik ?(config = default_config) (g : Grammar.t) (corpus : Corpus.t) =
  let prep = Parse.prepare g in
  let np = Array.length prep.prods in
  let beta = Array.make np config.alpha in
  let kl = ref 0.0 in
  let set_theta_bar () =
    Hashtbl.iter
      (fun _ idxs ->
        let bsum = List.fold_left (fun s i -> s +. beta.(i)) 0.0 idxs in
        let bpsi = digamma bsum in
        List.iter (fun i -> prep.probs.(i) <- exp (digamma beta.(i) -. bpsi)) idxs)
      prep.by_lhs
  in
  let update_kl () =
    kl := 0.0;
    Hashtbl.iter
      (fun _ idxs ->
        let k = List.length idxs in
        if k > 0 then begin
          let bsum = List.fold_left (fun s i -> s +. beta.(i)) 0.0 idxs in
          let bpsi = digamma bsum in
          let log_b_alpha =
            (float_of_int k *. log_gamma config.alpha)
            -. log_gamma (float_of_int k *. config.alpha)
          in
          let log_b_beta =
            List.fold_left (fun s i -> s +. log_gamma beta.(i)) 0.0 idxs
            -. log_gamma bsum
          in
          let term =
            List.fold_left
              (fun s i ->
                s +. ((beta.(i) -. config.alpha) *. (digamma beta.(i) -. bpsi)))
              0.0 idxs
          in
          kl := !kl +. (log_b_alpha -. log_b_beta +. term)
        end)
      prep.by_lhs
  in
  for _ = 1 to max 1 config.em_iters do
    set_theta_bar ();
    let acc = Array.make np 0.0 in
    List.iter
      (fun s ->
        let cnt, _ = Parse.expected_counts prep s.Corpus.tokens in
        Array.iteri
          (fun i x -> acc.(i) <- acc.(i) +. (s.Corpus.count *. x))
          cnt)
      corpus;
    for i = 0 to np - 1 do
      beta.(i) <- config.alpha +. acc.(i)
    done;
    update_kl ()
  done;
  set_theta_bar ();
  let log_z = ref 0.0 in
  List.iter
    (fun s ->
      match Parse.log_inside prep s.Corpus.tokens with
      | Some probability ->
          log_z := !log_z +. (s.Corpus.count *. probability)
      | None -> log_z := neg_infinity)
    corpus;
  !log_z -. !kl

let posterior ?(config = default_config) g corpus =
  let prior = structural_logprior ~config g in
  match config.mode with
  | Variational -> prior +. variational_loglik ~config g corpus
  | _ ->
      let evidence, ll, _, _ = approximate_log_evidence ~config g corpus in
      (match config.mode with
      | Approximate_evidence -> prior +. evidence
      | Maximum_likelihood -> prior +. ll
      | Variational -> assert false)

type details = {
  prior : float;
  objective : float;
  approximate_evidence : float;
  ml_loglik : float;
  posterior : float;
  dl_bits : float;
  num_nonterminals : int;
  num_productions : int;
}

let details ?(config = default_config) (g : Grammar.t) (corpus : Corpus.t) =
  let evidence, ll, _, _ = approximate_log_evidence ~config g corpus in
  let prior = structural_logprior ~config g in
  let dl = description_length_bits g in
  let likelihood =
    match config.mode with
    | Approximate_evidence -> evidence
    | Maximum_likelihood -> ll
    | Variational -> variational_loglik ~config g corpus
  in
  {
    prior;
    objective = likelihood;
    approximate_evidence = evidence;
    ml_loglik = ll;
    posterior = prior +. likelihood;
    dl_bits = dl;
    num_nonterminals = List.length (Grammar.nonterminals g);
    num_productions = List.length g.Grammar.productions;
  }

type heldout_result = {
  average_loglik_per_token : float;
  total_loglik : float;
  total_tokens : float;
  parsed_mass : float;
  total_mass : float;
  unparseable_mass : float;
}

let heldout ?(config = default_config) (g : Grammar.t) ~train
    (test : Corpus.t) =
  let prep = Parse.prepare g in
  ignore (Parse.fit_em prep train ~iters:config.em_iters ~tol:config.em_tol);
  let total = ref 0.0 in
  let ntok = ref 0.0 in
  let parsed_mass = ref 0.0 in
  let total_mass = ref 0.0 in
  let unparseable_mass = ref 0.0 in
  List.iter
    (fun s ->
      total_mass := !total_mass +. s.Corpus.count;
      ntok :=
        !ntok +. (s.Corpus.count *. float_of_int (List.length s.Corpus.tokens));
      match Parse.log_inside prep s.Corpus.tokens with
      | Some probability ->
          parsed_mass := !parsed_mass +. s.Corpus.count;
          total := !total +. (s.Corpus.count *. probability)
      | None -> unparseable_mass := !unparseable_mass +. s.Corpus.count)
    test;
  let total_loglik =
    if !unparseable_mass > 0.0 then neg_infinity else !total
  in
  {
    average_loglik_per_token =
      (if !ntok > 0.0 then total_loglik /. !ntok else neg_infinity);
    total_loglik;
    total_tokens = !ntok;
    parsed_mass = !parsed_mass;
    total_mass = !total_mass;
    unparseable_mass = !unparseable_mass;
  }
