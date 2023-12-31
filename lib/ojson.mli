open! Core

module Json : sig
  type t =
    [ `String of string
    | `Number of float
    | `Object of (string * t) list
    | `Array of t list
    | `True
    | `False
    | `Null
    ]
  [@@deriving sexp_of]
end

val parse_exn : string -> Json.t
val parse : string -> Json.t Or_error.t
