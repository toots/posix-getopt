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

exception Unknown_option of char
exception Missing_argument of char

let () =
  Printexc.register_printer (function
    | Unknown_option c ->
        Some (Printf.sprintf "Unknown getopt option: %c" c)
    | Missing_argument c ->
        Some (Printf.sprintf "Missing argument for getopt option: %c" c)
    | _ -> None)

open Ctypes
open Foreign

let _opterr = foreign_value "opterr" bool
let _optopt = foreign_value "optopt" char
let _optind = foreign_value "optind" int
let _optarg = foreign_value "optarg" string

let print_error flag =
  _opterr <-@ flag

let remaining_argv argv =
  let optind = !@ _optind in
  Array.sub argv optind ((Array.length argv)-optind)

let apply_opt c = function
  | `Unit callback -> callback ()
  | `Optional callback ->
     if c = ':' then
      callback None
     else
       callback (Some (!@ _optarg))
  | `Required callback ->
     callback (!@ _optarg)

let check_result c opts select =
  if c = '?' then
    raise (Unknown_option (!@ _optopt));
  let opt =
    List.find select opts
  in
  if c = ':' then
   begin
    match opt.arg with
      | `Unit _ -> assert false
      | `Optional _ -> ()
      | `Required _ -> raise (Missing_argument (!@ _optopt)) 
   end;
  opt

let string_of_short_opt {name;arg} =
  let arg = match arg with
    | `Unit _ -> ""
    | _ -> ":"
  in
  Printf.sprintf "%c%s" name arg

let _getopt = foreign "getopt" ~check_errno:true
  (int @-> ptr string @-> string @-> returning int)

let getopt argv opts =
  let _argc = Array.length argv in
  let _argv =
    CArray.of_list string (Array.to_list argv)
  in
  let _short_opts =
    String.concat ""
      (List.map string_of_short_opt opts)
  in
  let rec f () =
    let ret =
      _getopt _argc (CArray.start _argv) _short_opts
    in
    if ret = -1 then
      remaining_argv argv
    else
     begin
      let c = Char.chr ret in
      let {arg} =
        check_result c opts (fun {name} -> name = c)
      in
      apply_opt c arg;
      f ()
     end
  in
  f ()

let string_of_long_opt {name;arg} =
  let arg = match arg with
    | `Unit _ -> ""
    | _ -> ":"
  in
  Printf.sprintf "%c%s" (snd name) arg

type _long_opt

let long_opt : _long_opt structure typ  = structure "long_opt"
let _name = field long_opt "name" string
let _has_args = field long_opt "has_args" bool
let _flag = field long_opt "flag" (ptr int)
let _value = field long_opt "value" int
let () = seal long_opt

let long_opt_of_opt {name;arg} =
  let long_name, short_name = name in
  let _opt = make long_opt in
  setf _opt _name long_name;
  let has_args =
    match arg with
      | `Unit _ -> false
      | _ -> true
  in
  setf _opt _has_args has_args;
  setf _opt _flag (from_voidp int null);
  setf _opt _value (Char.code short_name);
  _opt

let _getopt_long = foreign "getopt_long" ~check_errno:true
  (int @-> ptr string @-> string @-> ptr long_opt @-> ptr int @-> returning int)

let _getopt_long_only = foreign "getopt_long" ~check_errno:true
  (int @-> ptr string @-> string @-> ptr long_opt @-> ptr int @-> returning int)

let getopt_long_generic fn argv opts =
  let _argc = Array.length argv in
  let _argv =
    CArray.of_list string (Array.to_list argv)
  in
  let _short_opts =
    String.concat ""
      (List.map string_of_long_opt opts)
  in
  let _long_opts =
    List.map long_opt_of_opt opts
  in
  let _long_opts =
    CArray.of_list long_opt _long_opts
  in
  let index = allocate int 0 in
  let rec f () =
    let ret =
      fn _argc (CArray.start _argv) _short_opts
         (CArray.start _long_opts) index
    in
    if ret = -1 then
      remaining_argv argv
    else
     begin
      let c = Char.chr ret in
      let {arg} =
        check_result c opts (fun {name} -> (snd name) = c)
      in
      apply_opt c arg;
      f ()
     end
  in
  f ()

let getopt_long = getopt_long_generic _getopt_long
let getopt_long_only = getopt_long_generic _getopt_long_only
