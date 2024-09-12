let usage_msg = "adjika <file> -o <output>"

module Arguments = struct
  let input_file_ref = ref ""
  let output_file_ref = ref ""

  type t = { input_file : string; output_file : string }

  let finalize () =
    { input_file = !input_file_ref; output_file = !output_file_ref }

  let input_file arg = arg.input_file
  let output_file arg = arg.output_file
end

let speclist =
  [ ("-o", Arg.Set_string Arguments.output_file_ref, "Set output file name") ]

let cli_parse () =
  Arg.parse speclist (fun file -> Arguments.input_file_ref := file) usage_msg;
  Arguments.finalize ()
