let () =
  Alcotest.run "ppx_subliner"
    [
      ("attr", Test_attr.Common.test_set);
      ("attr.term", Test_attr.Term.test_set);
      ("attr.cmd_info", Test_attr.Cmd_info.test_set);
      ("term.conv", Test_term_conv.test_set);
      ("term.info", Test_term.Info.test_set);
      ("term.as-term", Test_term_as_term.test_set);
      ("term.postional", Test_term.Positional.test_set);
      ("group-cmd", Test_group_cmd.test_set);
    ]
