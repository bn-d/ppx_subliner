let () =
  Alcotest.run "ppx_subliner"
    [
      ("attr", Test_attr.Common.test_set);
      ("attr.term", Test_attr.Term.test_set);
      ("term.conv", Test_term.Conv.test_set);
      ("term.info", Test_term.Info.test_set);
      ("term", Test_term.test_set);
      ("group-cmd", Test_group_cmd.test_set);
    ]
