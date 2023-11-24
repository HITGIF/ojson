open! Core

let () =
  (* Test cases from https://github.com/nst/JSONTestSuite/tree/master/test_parsing *)
  let skip_because_too_big =
    [ "n_structure_100000_opening_arrays.json"; "n_structure_open_array_object.json" ]
  in
  let test_cases =
    Core.Sys.ls_dir "./test_cases"
    |> List.filter ~f:(fun test_case ->
         not (List.mem skip_because_too_big test_case ~equal:String.equal))
  in
  List.iter test_cases ~f:(fun test_case ->
    let file = Core.In_channel.read_all ("./test_cases/" ^ test_case) in
    let json = Ojson.parse file in
    match String.get test_case 0 with
    | 'i' -> (* parsers are free to accept or reject content *) ()
    | 'y' ->
      (* content must be accepted by parsers *)
      (match json with
       | Ok _ -> ()
       | Error error ->
         Error.raise_s
           [%message
             "Expected to succeed" (test_case : string) (file : string) (error : Error.t)])
    | 'n' ->
      (* content must be rejected by parsers *)
      (match json with
       | Ok json ->
         Error.raise_s
           [%message
             "Expected to fail" (test_case : string) (file : string) (json : Ojson.Json.t)]
       | Error _ -> ())
    | prefix -> Error.raise_s [%message "Unknown test case prefix" (prefix : char)]);
  print_endline [%string "All %{(List.length test_cases)#Int} test cases passed"]
;;
