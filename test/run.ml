open OUnit2
open PosixGetopt

let test_short_flag _ =
  reset ();
  let argv = [|"progname";"-b"|] in
  let test = ref false in
  let opt = {
    name = 'b';
    arg = `None (fun () -> test := true)
  } in
  let ret =
    getopt argv [opt]
  in
  assert !test;
  assert (Array.length ret = 0)

let test_short_required _ =
  reset ();
  let argv = [|"progname";"-b";"arg"|] in
  let test = ref "" in
  let opt = {
    name = 'b';
    arg = `Required (fun arg -> test := arg)
  } in
  let ret =
    getopt argv [opt]
  in
  assert_equal "arg" !test;
  assert (Array.length ret = 0)

let test_short_required_no_arg _ =
  reset ();
  let argv = [|"progname";"-b"|] in
  let opt = {
    name = 'b';
    arg = `Required (fun _ -> ())
  } in
  let test = ref false in
  begin
    try
      ignore(getopt argv [opt])
    with Missing_argument 'b' ->
      test := true
  end;
  assert !test

let test_short_optional_no_arg _ =
  reset ();
  let argv = [|"progname";"-b"|] in
  let test = ref false in
  let opt = {
    name = 'b';
    arg = `Optional (fun arg -> test := arg = None)
  } in
  let ret =
    getopt argv [opt]
  in
  assert !test;
  assert (Array.length ret = 0)

let test_short_optional_arg _ =
  reset ();
  let argv = [|"progname";"-b";"yup"|] in
  let test = ref false in
  let opt = {
    name = 'b';
    arg = `Optional (fun arg -> test := arg = Some "yup")
  } in
  let ret =
    getopt argv [opt]
  in
  assert !test;
  assert (Array.length ret = 0)

let suite =
  "getopt tests">:::[
    "test_short_flag">::test_short_flag;
    "test_short_required">::test_short_required;
    "test_short_required_no_arg">::test_short_required_no_arg;
    "test_short_optional_no_arg">::test_short_optional_no_arg;
    "test_short_optional_arg">::test_short_optional_arg]

let () =
  run_test_tt_main suite
