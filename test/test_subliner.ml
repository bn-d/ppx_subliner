let () =
  Alcotest.run "ppx_subliner"
    [
      ("term_attr", Test_attr.term_test_set);
      ("term", Test_term.test_set);
      ("group-cmd", Test_group_cmd.test_set);
    ]
