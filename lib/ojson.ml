open! Core
open Option.Let_syntax

module Json = struct
  type t =
    [ `String of string
    | `Number of float
    | `Object of (string * t) list
    | `Array of t list
    | `True
    | `False
    | `Null
    ]
  [@@deriving sexp_of, variants]
end

module type Dfa = sig
  type t [@@deriving sexp_of]

  val parse : t -> Json.t option
  val transition : t -> char -> t option
  val start : char -> t option
  val start_state : unit -> t
end

module rec Value : Dfa = struct
  type t =
    | Start
    | String of String.t
    | Number of Number.t
    | Object of Object.t
    | Array of Array.t
    | True of True.t
    | False of False.t
    | Null of Null.t
    | Whitespace_after_start
    | Whitespace_after_value of Json.t
  [@@deriving variants, sexp_of]

  let parse = function
    | Start | Whitespace_after_start -> None
    | Whitespace_after_value t -> Some t
    | String s -> String.parse s
    | Number n -> Number.parse n
    | Object o -> Object.parse o
    | Array a -> Array.parse a
    | True t -> True.parse t
    | False f -> False.parse f
    | Null n -> Null.parse n
  ;;

  let start_aux (type a) (module Dfa : Dfa with type t = a) variant c prev =
    Option.value_map prev ~f:Option.return ~default:(Dfa.start c >>| variant)
  ;;

  let transition_aux (type a) (module Dfa : Dfa with type t = a) variant v c =
    match Dfa.transition v c with
    | Some v -> Some (variant v)
    | None ->
      Whitespace.start c
      >>| (fun _ -> Dfa.parse v >>| whitespace_after_value)
      |> Option.join
  ;;

  let transition = function
    | Start | Whitespace_after_start ->
      fun c ->
        None
        |> start_aux (module Whitespace) (fun _ -> whitespace_after_start) c
        |> start_aux (module String) string c
        |> start_aux (module Number) number c
        |> start_aux (module Object) object_ c
        |> start_aux (module Array) array c
        |> start_aux (module True) true_ c
        |> start_aux (module False) false_ c
        |> start_aux (module Null) null c
    | String s -> transition_aux (module String) string s
    | Number n -> transition_aux (module Number) number n
    | Object o -> transition_aux (module Object) object_ o
    | Array a -> transition_aux (module Array) array a
    | True t -> transition_aux (module True) true_ t
    | False f -> transition_aux (module False) false_ f
    | Null n -> transition_aux (module Null) null n
    | Whitespace_after_value t ->
      fun c -> Whitespace.start c >>| fun _ -> whitespace_after_value t
  ;;

  let start = transition Start
  let start_state () = Start
end

and Object : Dfa = struct
  module State = struct
    type t = (string * Json.t option) list [@@deriving sexp_of]

    let empty = []
    let add_key t k = (k, None) :: t

    let add_value t v =
      match t with
      | [] -> Error.raise_s [%message "Missing key"]
      | (k, Some v') :: _ ->
        Error.raise_s [%message "Duplicate key" (k : string) (v : Json.t) (v' : Json.t)]
      | (k, None) :: t -> (k, Some v) :: t
    ;;

    let parse_exn t =
      List.map t ~f:(fun (k, v) ->
        match v with
        | None -> Error.raise_s [%message "Missing value" (k : string)]
        | Some v -> k, v)
      |> List.rev
      |> Json.object_
    ;;
  end

  type t =
    | Start of State.t
    | Open_brace of State.t
    | Close_brace of State.t
    | Comma of State.t
    | Colon of State.t
    | Key of State.t * String.t
    | Value of State.t * Value.t
    | Whitespace_after_open_brace of State.t
    | Whitespace_before_key of State.t
    | Whitespace_after_key of State.t
  [@@deriving variants, sexp_of]

  let parse = function
    | Close_brace s -> Some (State.parse_exn s)
    | _ -> None
  ;;

  let add_key s = function
    | `String k -> State.add_key s k
    | k -> Error.raise_s [%message "Non-string key" (k : Json.t)]
  ;;

  let whitespace_or_start_string variant s c =
    match Whitespace.start c with
    | Some _ -> Some (variant s)
    | None -> String.start c >>| key s
  ;;

  let transition = function
    | Start s ->
      (function
       | '{' -> Some (Open_brace s)
       | _ -> None)
    | Open_brace s ->
      (function
       | '}' -> Some (Close_brace s)
       | c -> whitespace_or_start_string whitespace_after_open_brace s c)
    | Whitespace_after_open_brace s ->
      (function
       | '}' -> Some (Close_brace s)
       | c -> whitespace_or_start_string whitespace_after_open_brace s c)
    | Key (s, k) ->
      (function
       | ':' -> String.parse k >>| add_key s >>| colon
       | c ->
         (match Whitespace.start c with
          | Some _ -> String.parse k >>| add_key s >>| whitespace_after_key
          | None -> String.transition k c >>| key s))
    | Whitespace_after_key s ->
      (function
       | ':' -> Some (Colon s)
       | c -> Whitespace.start c >>| fun _ -> whitespace_after_key s)
    | Colon s -> fun c -> Value.start c >>| value s
    | Value (s, v) ->
      fun c ->
        (match Value.transition v c with
         | Some v -> Some (Value (s, v))
         | None ->
           (match c with
            | ',' -> Value.parse v >>| State.add_value s >>| comma
            | '}' -> Value.parse v >>| State.add_value s >>| close_brace
            | _ -> None))
    | Comma s -> whitespace_or_start_string whitespace_before_key s
    | Whitespace_before_key s -> whitespace_or_start_string whitespace_before_key s
    | Close_brace _ ->
      (function
       | _ -> None)
  ;;

  let start = transition (Start State.empty)
  let start_state () = Start State.empty
end

and Array : Dfa = struct
  module State = struct
    type t = Json.t list [@@deriving sexp_of]

    let empty = []
    let add t v = v :: t
    let parse_exn = Json.array
  end

  type t =
    | Start of State.t
    | Open_bracket of State.t
    | Close_bracket of State.t
    | Comma of State.t
    | Value of State.t * Value.t
    | Whitespace of State.t
  [@@deriving variants, sexp_of]

  let parse = function
    | Close_bracket s -> Some (State.parse_exn s)
    | _ -> None
  ;;

  let transition = function
    | Start s ->
      (function
       | '[' -> Some (Open_bracket s)
       | _ -> None)
    | Open_bracket s | Whitespace s ->
      (function
       | ']' -> Some (Close_bracket s)
       | c ->
         (match Whitespace.start c with
          | Some _ -> Some (Whitespace s)
          | None -> Value.start c >>| value s))
    | Value (s, v) ->
      fun c ->
        (match Value.transition v c with
         | Some v -> Some (Value (s, v))
         | None ->
           (match c with
            | ',' -> Value.parse v >>| State.add s >>| comma
            | ']' -> Value.parse v >>| State.add s >>| close_bracket
            | _ -> None))
    | Comma s -> fun c -> Value.start c >>| value s
    | Close_bracket _ ->
      (function
       | _ -> None)
  ;;

  let start = transition (Start State.empty)
  let start_state () = Start State.empty
end

and Whitespace : Dfa = struct
  type t = unit [@@deriving sexp_of]

  let parse _ = None

  let transition (_ : t) = function
    | ' ' | '\n' | '\r' | '\t' -> Some ()
    | _ -> None
  ;;

  let start = transition ()
  let start_state () = ()
end

and String : Dfa = struct
  module State = struct
    type t = char list [@@deriving sexp_of]

    let empty = []
    let add t c = c :: t

    let unicode_to_utf8 code =
      if code < 0x80
      then [ Char.of_int_exn code ]
      else if code < 0x800
      then (
        let byte1 = 0xC0 lor (code lsr 6) in
        let byte2 = 0x80 lor (code land 0x3F) in
        [ Char.of_int_exn byte1; Char.of_int_exn byte2 ])
      else (
        let byte1 = 0xE0 lor (code lsr 12) in
        let byte2 = 0x80 lor ((code lsr 6) land 0x3F) in
        let byte3 = 0x80 lor (code land 0x3F) in
        [ Char.of_int_exn byte1; Char.of_int_exn byte2; Char.of_int_exn byte3 ])
    ;;

    let hex_to_int = function
      | '0' .. '9' as c -> Char.to_int c - 0x30
      | 'a' .. 'f' as c -> Char.to_int c - 0x57
      | 'A' .. 'F' as c -> Char.to_int c - 0x37
      | _ -> Error.raise_s [%message "Invalid hex digit"]
    ;;

    let add_unicode_hex4 t hex4 =
      match t with
      | hex3 :: hex2 :: hex1 :: tl ->
        let code =
          (hex_to_int hex1 lsl 12)
          lor (hex_to_int hex2 lsl 8)
          lor (hex_to_int hex3 lsl 4)
          lor hex_to_int hex4
        in
        let utf8_encoded = unicode_to_utf8 code in
        List.rev_append utf8_encoded tl
      | _ -> Error.raise_s [%message "Invalid unicode escape sequence"]
    ;;

    let parse_exn t = List.rev t |> Core.String.of_char_list |> Json.string
  end

  type t =
    | Start of State.t
    | Open_quote of State.t
    | Close_quote of State.t
    | Backslash of State.t
    | Escape of State.t
    | Code_point of State.t
    | Unicode of State.t
    | Hex1 of State.t
    | Hex2 of State.t
    | Hex3 of State.t
    | Hex4 of State.t
  [@@deriving sexp_of]

  let parse = function
    | Close_quote s -> Some (State.parse_exn s)
    | _ -> None
  ;;

  let transition = function
    | Start s ->
      (function
       | '"' -> Some (Open_quote s)
       | _ -> None)
    | Open_quote s | Escape s | Code_point s | Hex4 s ->
      (function
       | '"' -> Some (Close_quote s)
       | '\\' -> Some (Backslash s)
       | '\b' | '\n' | '\r' | '\t' | '\000' .. '\031' ->
         (* Control characters *)
         None
       | c -> Some (Code_point (State.add s c)))
    | Backslash s ->
      (function
       | '"' -> Some (Escape (State.add s '"'))
       | '\\' -> Some (Escape (State.add s '\\'))
       | '/' -> Some (Escape (State.add s '/'))
       | 'b' -> Some (Escape (State.add s '\b'))
       | 'f' -> Some (Escape (State.add s '\012'))
       | 'n' -> Some (Escape (State.add s '\n'))
       | 'r' -> Some (Escape (State.add s '\r'))
       | 't' -> Some (Escape (State.add s '\t'))
       | 'u' -> Some (Unicode s)
       | _ -> None)
    | Unicode s ->
      (function
       | ('0' .. '9' | 'a' .. 'f' | 'A' .. 'F') as c -> Some (Hex1 (State.add s c))
       | _ -> None)
    | Hex1 s ->
      (function
       | ('0' .. '9' | 'a' .. 'f' | 'A' .. 'F') as c -> Some (Hex2 (State.add s c))
       | _ -> None)
    | Hex2 s ->
      (function
       | ('0' .. '9' | 'a' .. 'f' | 'A' .. 'F') as c -> Some (Hex3 (State.add s c))
       | _ -> None)
    | Hex3 s ->
      (function
       | ('0' .. '9' | 'a' .. 'f' | 'A' .. 'F') as c ->
         Some (Hex4 (State.add_unicode_hex4 s c))
       | _ -> None)
    | Close_quote _ ->
      (function
       | _ -> None)
  ;;

  let start = transition (Start State.empty)
  let start_state () = Start State.empty
end

and Number : Dfa = struct
  module State = struct
    type t = string [@@deriving sexp_of]

    let empty = ""
    let add t c = t ^ Char.to_string c
    let parse_exn = Fn.compose Json.number Float.of_string
  end

  type t =
    | Start of State.t
    | Minus of State.t
    | Leading_zero of State.t
    | Leading_non_zero_digit of State.t
    | Integer_digit of State.t
    | Decimal_point of State.t
    | Fraction_digit of State.t
    | Exponent of State.t
    | Exponent_sign of State.t
    | Exponent_digit of State.t
  [@@deriving sexp_of, variants]

  let parse = function
    | Leading_zero s
    | Leading_non_zero_digit s
    | Integer_digit s
    | Fraction_digit s
    | Exponent_digit s -> Some (State.parse_exn s)
    | _ -> None
  ;;

  let transition_aux variant s c = Some (variant (State.add s c))

  let transition = function
    | Start s ->
      (function
       | '-' as c -> transition_aux minus s c
       | '0' as c -> transition_aux leading_zero s c
       | '1' .. '9' as c -> transition_aux leading_non_zero_digit s c
       | _ -> None)
    | Minus s ->
      (function
       | '0' as c -> transition_aux leading_zero s c
       | '1' .. '9' as c -> transition_aux leading_non_zero_digit s c
       | _ -> None)
    | Leading_zero s ->
      (function
       | '.' as c -> transition_aux decimal_point s c
       | ('e' | 'E') as c -> transition_aux exponent s c
       | _ -> None)
    | Leading_non_zero_digit s ->
      (function
       | '.' as c -> transition_aux decimal_point s c
       | '0' .. '9' as c -> transition_aux integer_digit s c
       | ('e' | 'E') as c -> transition_aux exponent s c
       | _ -> None)
    | Integer_digit s ->
      (function
       | '.' as c -> transition_aux decimal_point s c
       | '0' .. '9' as c -> transition_aux integer_digit s c
       | ('e' | 'E') as c -> transition_aux exponent s c
       | _ -> None)
    | Decimal_point s ->
      (function
       | '0' .. '9' as c -> transition_aux fraction_digit s c
       | _ -> None)
    | Fraction_digit s ->
      (function
       | '0' .. '9' as c -> transition_aux fraction_digit s c
       | ('e' | 'E') as c -> transition_aux exponent s c
       | _ -> None)
    | Exponent s ->
      (function
       | ('+' | '-') as c -> transition_aux exponent_sign s c
       | '0' .. '9' as c -> transition_aux exponent_digit s c
       | _ -> None)
    | Exponent_sign s ->
      (function
       | '0' .. '9' as c -> transition_aux exponent_digit s c
       | _ -> None)
    | Exponent_digit s ->
      (function
       | '0' .. '9' as c -> transition_aux exponent_digit s c
       | _ -> None)
  ;;

  let start = transition (Start State.empty)
  let start_state () = Start State.empty
end

and True : Dfa = struct
  type t =
    | Start
    | T
    | R
    | U
    | E
  [@@deriving sexp_of]

  let parse = function
    | E -> Some `True
    | _ -> None
  ;;

  let transition = function
    | Start ->
      (function
       | 't' -> Some T
       | _ -> None)
    | T ->
      (function
       | 'r' -> Some R
       | _ -> None)
    | R ->
      (function
       | 'u' -> Some U
       | _ -> None)
    | U ->
      (function
       | 'e' -> Some E
       | _ -> None)
    | E ->
      (function
       | _ -> None)
  ;;

  let start = transition Start
  let start_state () = Start
end

and False : Dfa = struct
  type t =
    | Start
    | F
    | A
    | L
    | S
    | E
  [@@deriving sexp_of]

  let parse = function
    | E -> Some `False
    | _ -> None
  ;;

  let transition = function
    | Start ->
      (function
       | 'f' -> Some F
       | _ -> None)
    | F ->
      (function
       | 'a' -> Some A
       | _ -> None)
    | A ->
      (function
       | 'l' -> Some L
       | _ -> None)
    | L ->
      (function
       | 's' -> Some S
       | _ -> None)
    | S ->
      (function
       | 'e' -> Some E
       | _ -> None)
    | E ->
      (function
       | _ -> None)
  ;;

  let start = transition Start
  let start_state () = Start
end

and Null : Dfa = struct
  type t =
    | Start
    | N
    | U
    | L
    | L2
  [@@deriving sexp_of]

  let parse = function
    | L2 -> Some `Null
    | _ -> None
  ;;

  let transition = function
    | Start ->
      (function
       | 'n' -> Some N
       | _ -> None)
    | N ->
      (function
       | 'u' -> Some U
       | _ -> None)
    | U ->
      (function
       | 'l' -> Some L
       | _ -> None)
    | L ->
      (function
       | 'l' -> Some L2
       | _ -> None)
    | L2 ->
      (function
       | _ -> None)
  ;;

  let start = transition Start
  let start_state () = Start
end

open! Core

let parse_exn input =
  input
  |> String.to_list
  |> List.fold ~init:(Value.start_state ()) ~f:(fun state c ->
       Value.transition state c
       |> Option.value_exn
            ~error:
              (Error.create_s
                 [%message "Cannot parse charactor" ~char:(c : char) (state : Value.t)]))
  |> Value.parse
  |> Option.value_exn
       ~error:(Error.create_s [%message "Cannot parse input" (input : string)])
;;

let parse input = Or_error.try_with (fun () -> parse_exn input)

let%expect_test "parse" =
  let input =
    {|
      {
        "glossary": {
          "title": "example glossary",
          "GlossDiv": {
            "title": "S",
            "GlossList": {
              "GlossEntry": {
                "ID": "🐫 ガ \u30AC",
                "SortAs": "SGML",
                "Size": 123.456e+12,
                "GlossTerm": "Standard Generalized Markup Language",
                "Acronym": "SGML",
                "T": true,
                "F": false,
                "N": null,
                "Abbrev": "ISO 8879:1986",
                "GlossDef": {
                  "para": "A meta-markup language, used to create markup languages such as DocBook.",
                  "GlossSeeAlso": ["GML", "XML"]
                },
                "GlossSee": "markup"
              }      
            }
          }
        }
      }
    |}
  in
  print_s [%sexp (parse_exn input : Json.t)];
  [%expect
    {|
    (Object
     ((glossary
       (Object
        ((title (String "example glossary"))
         (GlossDiv
          (Object
           ((title (String S))
            (GlossList
             (Object
              ((GlossEntry
                (Object
                 ((ID (String "\240\159\144\171 \227\130\172 \227\130\172"))
                  (SortAs (String SGML)) (Size (Number 123456000000000))
                  (GlossTerm (String "Standard Generalized Markup Language"))
                  (Acronym (String SGML)) (T True) (F False) (N Null)
                  (Abbrev (String "ISO 8879:1986"))
                  (GlossDef
                   (Object
                    ((para
                      (String
                       "A meta-markup language, used to create markup languages such as DocBook."))
                     (GlossSeeAlso (Array ((String XML) (String GML)))))))
                  (GlossSee (String markup)))))))))))))))) |}]
;;
