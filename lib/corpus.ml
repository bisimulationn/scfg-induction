type sample = { tokens : string list; count : float }

type t = sample list

let total_count corpus =
  List.fold_left (fun s x -> s +. x.count) 0.0 corpus

let distinct corpus = List.length corpus

let tokens_to_string toks = String.concat " " toks

let of_string text =
  let lines = String.split_on_char '\n' text in
  let acc = Hashtbl.create 64 in
  let order = ref [] in
  List.iter
    (fun line ->
      let line = String.trim line in
      if String.length line > 0 && line.[0] <> '#' then begin

        let count, rest =
          match String.index_opt line ':' with
          | Some i -> (
              match
                float_of_string_opt (String.trim (String.sub line 0 i))
              with
              | Some c ->
                  ( c,
                    String.sub line (i + 1) (String.length line - i - 1) )
              | None -> (1.0, line))
          | None -> (1.0, line)
        in
        let toks =
          String.split_on_char ' ' (String.trim rest)
          |> List.filter (fun s -> String.length s > 0)
        in
        if toks <> [] then begin
          let key = tokens_to_string toks in
          (match Hashtbl.find_opt acc key with
          | Some c -> Hashtbl.replace acc key (c +. count)
          | None ->
              Hashtbl.replace acc key count;
              order := key :: !order);
          ()
        end
      end)
    lines;
  List.rev !order
  |> List.map (fun key ->
         let toks =
           String.split_on_char ' ' key
           |> List.filter (fun s -> String.length s > 0)
         in
         { tokens = toks; count = Hashtbl.find acc key })

let to_string corpus =
  List.map
    (fun s -> Printf.sprintf "%g: %s" s.count (tokens_to_string s.tokens))
    corpus
  |> String.concat "\n"

let of_file path =
  let ic = open_in path in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  of_string s
