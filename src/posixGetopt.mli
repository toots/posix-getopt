type short = char
type long = (string*char)

type arg = [
  | `Unit     of (unit -> unit)
  | `Optional of (string option -> unit)
  | `Required of (string -> unit)
]

type 'a opt = {
  name: 'a;
  arg:  arg
} 

exception Unknown_option   of char
exception Missing_argument of char

val print_error      : bool -> unit
val getopt           : string array -> short opt list -> string array
val getopt_long      : string array -> long opt list -> string array
val getopt_long_only : string array -> long opt list -> string array
