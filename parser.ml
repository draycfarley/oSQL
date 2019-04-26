(** regular expressions for checking if a command contains the right keywords
    in the right order, with a following clause that has the right "shape" (i.e. 
    following a SET keyword there should be a series of column_name = value) *)
let gen_select = Str.regexp_case_fold "^\\(SELECT +.+\\)\\( +FROM +.+\\)\\( +INNER JOIN +.+ +\\(ON +.+\\)\\)?\\( +WHERE +.+\\)?\\( +GROUP BY +.+ \\(+HAVING+.+\\)?\\)?\\( +ORDER BY +[A-Z0-9_]+ +\\(ASC\\|DESC\\)\\)? *$";;
(** All recognized keywords, used for lexing and parsing*)
let keywords_lex = Str.regexp_case_fold "^\\(SELECT\\|FROM\\|WHERE\\|GROUP BY\\|HAVING\\|ORDER BY\\|CREATE TABLE\\|DROP TABLE\\|INSERT INTO\\|VALUES\\|ALTER TABLE\\|RENAME TO\\|ADD\\|DROP\\|MODIFY\\|DELETE FROM\\|UPDATE\\|SET\\|LOAD\\|SAVE\\|AVG\\|COUNT\\|MIN\\|MAX\\|SUM\\|NOT IN\\|IN\\|\\INNER JOIN\\|ON\\)$"
let table_create = Str.regexp_case_fold "^CREATE TABLE +[A-Z0-9_]+ *( *\\([A-Z0-9_]+ +[A-Z0-9_]+, *\\)*\\([A-Z0-9_]+ +[A-Z0-9_]+\\) *) *$"
let table_drop = Str.regexp_case_fold "^DROP TABLE +[A-Z0-9_]+ *$"
let table_insert = Str.regexp_case_fold "^INSERT INTO +[A-Z0-9_]+ *( *\\([A-Z0-9_]+, *\\)*\\([A-Z0-9_]+ *\\)) +VALUES *( *\\(.+, *\\)*\\(.+\\) *) *$"
let table_alter_rename = Str.regexp_case_fold "^ALTER TABLE +[A-Z0-9_]+ +RENAME TO +[A-Z0-9_]+ *$"
let table_alter = Str.regexp_case_fold "^ALTER TABLE +[A-Z0-9_]+ +\\(ADD\\( +[A-Z0-9_]+ +[A-Z0-9_]+\\)\\|DROP\\( +[A-Z0-9_]+\\)\\|MODIFY\\( +[A-Z0-9_]+ +[A-Z0-9_]+\\)\\) *$"
let table_delete_from = Str.regexp_case_fold "^DELETE FROM +[A-Z0-9_]+\\( +WHERE +.+\\)? *$"
let table_update = Str.regexp_case_fold "^UPDATE +[A-Z0-9_]+ +SET +\\([A-Z0-9_]+ *= *.+, *\\)*\\([A-Z0-9_]+ *= *.+\\)\\( +WHERE +.+\\)? *$"
let rename = Str.regexp_case_fold "RENAME TO"
(** all boolean operators that we support, currently only comparison ops*)
let bool_ops_lex = Str.regexp "\\(=\\|>\\|<\\|<=\\|>=\\|<>\\)"
let and_or_not = Str.regexp_case_fold "^\\(AND\\|OR\\|NOT\\)$"
let lt_or_gt = Str.regexp_case_fold "^\\(<\\|>\\)$"
let lt = Str.regexp_case_fold "^<$"
let bool_ops = Str.regexp_case_fold "^\\(=\\|>\\|<\\|<=\\|>=\\|<>\\|AND\\|OR\\|NOT\\)$"
let load_save = Str.regexp_case_fold "^\\(LOAD\\|SAVE\\) +[A-Z0-9_\\./]+ *$"

(** exception raised when clause following keyword SET is invalid*)
exception Malformed_set
(** exception raised when clause following keyword CREATE TABLE is invalid*)
exception Malformed_create


(** [is_well_formed r s] returns true is string [s] matches regular expression
    [r] and false otherwise*)
let is_well_formed (r:Str.regexp) (s:string) : bool = 
  Str.string_match r s 0

(** [clean_up_word s] partitions [s] around internal commas and internal 
    open parentheses, substrings before and after internal commas and internal
    open parentheses are stored as seperate elements in the returned string list
    [lst], traling closed parentheses and trailing commas are removed*)
let clean_up_word (s:string) = 
  let sep_str = String.map (fun x -> if (x = ','|| x = '(') then ';' else x) s in 
  let cleaner = if String.get sep_str (String.length sep_str -1) = ')' then String.sub sep_str 0 (String.length sep_str -1)
    else sep_str in 
  String.split_on_char ';' cleaner

(** [op_lexer s] returns a string list of [s] partitioned around the boolean 
    operators supported (operators in bool_ops_lex). Expects [s] to be of the 
    form "beforeOP"^"OP"^"afterOP" or some substring of this. This method is 
    only called as part of the lexing process and as such assumes all whitespaces
    have been removed. Returns:
    - "noOP" -> ["noOP"]
    - "beforeOP"^"OP" -> ["beforeOP"; "OP"]
    - "beforeOP"^"OP"^"afterOP" -> ["beforeOP"; "OP"; "afterOP"]
    - "OP"^"afterOP" -> ["OP"; "afterOP"]
    - "OP" -> ["OP"]*)
let op_lexer (s:string) = 
  try 
    let pointer = Str.search_forward bool_ops_lex s 0 in 
    let op_helper (word:string) acc = 
      match word with
      | "" -> acc
      | t -> 
        let lPointer = Str.search_backward bool_ops_lex t (String.length t -1) in 
        let rPointer = Str.group_end 1 in 
        let op = String.sub t lPointer (rPointer - lPointer) in
        if lPointer = 0 then op::(String.sub t rPointer (String.length t - rPointer))::acc
        else
          match op with
          | "=" ->
            let charBefore = String.make 1 (String.get t (lPointer-1)) in 
            let prepend = (if (Str.string_match lt_or_gt charBefore 0) then 
                             ((charBefore^op),lPointer-1)
                           else
                             (op,lPointer)) in 
            if rPointer = String.length t then
              (String.sub t 0 (snd prepend))::(fst prepend)::acc
            else
              (String.sub t 0 (snd prepend))::(fst prepend)::(String.sub t rPointer (String.length t - rPointer))::acc
          | ">" ->  let charBefore = String.make 1 (String.get t (lPointer-1)) in 
            let prepend = (if (Str.string_match lt charBefore 0) then 
                             ((charBefore^op),lPointer-1)
                           else
                             (op,lPointer)) in 
            if rPointer = String.length t then
              (String.sub t 0 (snd prepend))::(fst prepend)::acc
            else
              (String.sub t 0 (snd prepend))::(fst prepend)::(String.sub t rPointer (String.length t - rPointer))::acc
          | h -> if rPointer = String.length t then
              (String.sub t 0 lPointer)::h::acc
            else
              (String.sub t 0 lPointer)::h::(String.sub t rPointer (String.length t - rPointer))::acc in 
    if pointer = 0 && String.length s = 1 then [s] else
      op_helper s []
  with
    Not_found -> [s]

(** [lexer s] returns a string list of the tokens in [s]. Tokens are keywords
    (words in keywords_lex), boolean/comparison operators (operators in 
    bool_ops_lex) and any other substrings of [s] not containing whitespaces,
    with parentheses and commas removed. NOTE: any substring of [s] with no 
    whitespaces but containing internal commas and open parens (meaning anywhere
    in the substring except the front or head of the substring) are partitioned
    and separated into distinct tokens around the commas or open parens according 
    to [clean_up_word] *)
let lexer (s:string) = 
  let lst = List.filter (fun x -> not(x = ""||x="("||x=")"||x=",")) (String.split_on_char ' ' s) in 
  let lst' = List.filter (fun x -> x <> "") (List.flatten (List.map (fun x -> clean_up_word x) lst)) in 
  let l = List.filter (fun x -> x <> "") (List.flatten (List.map op_lexer lst')) in
  let rec lexer_helper lst acc = 
    match lst with
    | [] -> List.rev acc
    | h::t -> match String.uppercase_ascii h with
      | "CREATE" 
      | "DROP"  
      | "ALTER" -> (match t with
          | [] -> lexer_helper t (h::acc)
          | x::xs -> if (String.uppercase_ascii x = "TABLE") then 
              lexer_helper xs ((h^" TABLE")::acc)
            else lexer_helper xs (x::h::acc))
      | "INSERT" -> (match t with
          | [] -> lexer_helper t (h::acc)
          | x::xs -> if (String.uppercase_ascii x = "INTO") then 
              lexer_helper xs ((h^" INTO")::acc)
            else lexer_helper xs (x::h::acc))
      | "RENAME" -> (match t with
          | [] -> lexer_helper t (h::acc)
          | x::xs -> if (String.uppercase_ascii x = "TO") then 
              lexer_helper xs ((h^" TO")::acc)
            else lexer_helper xs (x::h::acc))
      | "DELETE" -> (match t with
          | [] -> lexer_helper t (h::acc)
          | x::xs -> if (String.uppercase_ascii x = "FROM") then 
              lexer_helper xs ((h^" FROM")::acc)
            else lexer_helper xs (x::h::acc))
      | "ORDER" | "GROUP" -> (match t with
          | [] -> lexer_helper t (h::acc)
          | x::xs -> if (String.uppercase_ascii x = "BY") then 
              lexer_helper xs ((h^" BY")::acc)
            else lexer_helper xs (x::h::acc))
      | "NOT" -> (match t with
          | [] -> lexer_helper t (h::acc)
          | x::xs -> if (String.uppercase_ascii x = "IN") then 
              lexer_helper xs (((String.uppercase_ascii h)^" IN")::acc)
            else lexer_helper xs (x::h::acc))
      | "INNER" -> (match t with
          | [] -> lexer_helper t (h::acc)
          | x::xs -> if (String.uppercase_ascii x = "JOIN") then 
              lexer_helper xs (((String.uppercase_ascii h)^" JOIN")::acc)
            else lexer_helper xs (x::h::acc))
      | "IN" | "AS" -> lexer_helper t ((String.uppercase_ascii h)::acc)
      | s -> lexer_helper t (h::acc) in 
  lexer_helper l []

(** [get_remainder lst] helper for compactify that returns a tuple of the 
    list of the all the tokens from the head of list until the first key word and 
    the remainder of the list (in that order) *)
let get_remainder lst = 
  let rec get_remainder_helper lst acc = 
    match lst with
    | [] -> (List.rev acc,[])
    | h::t -> if is_well_formed keywords_lex h then (List.rev acc, h::t)
      else if is_well_formed and_or_not h then get_remainder_helper t ((String.uppercase_ascii h)::acc)
      else get_remainder_helper t (h::acc) in 
  get_remainder_helper lst []

(** [get_rem_create lst] functions like [get_remainder] but all the tokens
    from the element after the head of the list up to the keyword are combined with
    the token directly following it seperated by a whitespace, so that 
    ["hd"; "t1"; "t2"; "t3"; "t4"; ...] becomes ["hd"; "t1 t2"; "t3 t4"; ...],
    returns (["hd"; "t1 t2"; "t3 t4"; ... ; "tn-1 tn"], [rest_of_lst])
    Helper for parsing, assumes that we have already checked that the create
    clause is valid so that it will contain a table name token and then column
    name-datatype token pairs. Raises Malformed_create if lst is empty or if 
    every token after Head and up to the next keyword can't be paired up as above*)
let get_rem_create lst = 
  let (rem_clause, rem_query) = get_remainder lst in 
  let rec combine_pairs l acc = 
    match l with
    | [] -> List.rev acc
    | h::m::t -> combine_pairs t ((h ^ " " ^ m)::acc) 
    | h::[] -> raise Malformed_create in 
  match rem_clause with
  | [] -> raise Malformed_create
  | h::t -> (h::(combine_pairs t []),rem_query)

(** [get_rem_set lst] functions like [get_remainder] except that it expects
    that [lst] is of the form:
    ["t1"; "="; "t2"; "t3"; "="; "t4"; ... ; "tn-1"; "=";"tn"; "keyword"||end of list ...]
    and returns (["t1=t2"; "t3=t4"; ... ; "tn-1=tn"],[rest_of_lst]). Raises
    Malformed_set if [lst] is in a different form than what [get_rem_set] expects. *)
let get_rem_set lst = 
  let (rem_clause, rem_query) = get_remainder lst in 
  let rec combine_equals l acc = 
    match l with
    | [] -> List.rev acc
    | h::eq::s::t -> combine_equals t ((h^eq^s)::acc)
    | _::_::[] 
    | _::[] -> raise Malformed_set in 
  (combine_equals rem_clause [], rem_query)

(** [compactify lst acc] converts the list of tokens [lst] to a (string * string list) list
    and stores result in [acc]. the first element of each tuple is a keyword (word matched by keywords_lex),
    and the second element is a string list of all the tokens between the 
    keyword and the next keyword or end of lst (whichever comes first). Note: 
    Create table and set are special cases where just putting the tokens that follow
    them into a list is not enough. For create table the remaining tokens are
    stored as [table_name; "colname1 dtype1"; "colname2 dtype2"; ... ; "colnameN dtypeN"]
    and for Set the remaining tokens are stored as ["col1=val1"; "col2=val2"; ... ; "coln=valn"]
    will raise Malformed_create or Malformed_set if create table or set are tokens
    in [lst] and storing the tokens following them and before the next keyword or end
    of list in this manner is not possible. NOTE: matching keywords is case 
    insensitive but keywords are converted to uppercase ascii when stored in 
    the first element of a tuple element of the returned list. *)
let rec compactify lst acc = 
  match lst with
  | [] -> List.rev acc
  | h::t -> if is_well_formed keywords_lex h then 
      let keyword = String.uppercase_ascii h in 
      match keyword with
      | "" -> failwith "how even"
      | "CREATE TABLE" -> let (rem_clause, rem_query) = get_rem_create t in 
        compactify rem_query ((keyword,rem_clause)::acc)
      | "SET" -> let (rem_clause, rem_query) = get_rem_set t in 
        compactify rem_query ((keyword,rem_clause)::acc)
      | "ORDER BY" -> let (rem_clause, rem_query) = get_remainder t in 
        compactify rem_query ((keyword, List.rev rem_clause)::acc)
      | s ->  let (rem_clause, rem_query) = get_remainder t in 
        compactify rem_query ((keyword,rem_clause)::acc)
    else
      compactify t acc

let parse (s:string) = 
  let lst = lexer s in 
  try 
    match lst with
    | [] -> []
    | h::t -> match String.uppercase_ascii h with
      | "SELECT" -> if not(is_well_formed gen_select s) then [] else
          compactify lst []
      | "CREATE TABLE" -> if not(is_well_formed table_create s) then [] else
          compactify lst []
      | "DROP TABLE" -> if not(is_well_formed table_drop s) then [] else
          compactify lst []
      | "DELETE FROM" -> if not(is_well_formed table_delete_from s) then [] else
          compactify lst []
      | "UPDATE" -> if not(is_well_formed table_update s) then [] else
          compactify lst []
      | "INSERT INTO" -> if not(is_well_formed table_insert s) then [] else
          compactify lst []
      | "ALTER TABLE" -> (match List.find_opt (fun x -> String.uppercase_ascii x = "RENAME TO") lst with
          | Some _ -> if not(is_well_formed table_alter_rename s) then []
            else compactify lst []
          | None -> if not(is_well_formed table_alter s) then []
            else compactify lst [])
      | "LOAD" | "SAVE" -> if not(is_well_formed load_save s) then [] else
          compactify lst []
      | _ -> []
  with 
  | Malformed_create 
  | Malformed_set -> []
