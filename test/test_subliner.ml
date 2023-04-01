let () =
  Alcotest.run "ppx_subliner"
    [ ("term", Test_term.test_set); ("group-cmd", Test_group_cmd.test_set) ]
