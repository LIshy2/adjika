module Arguments : sig
  type t

  val input_file : t -> string
  val output_file : t -> string
end

val cli_parse : unit -> Arguments.t
