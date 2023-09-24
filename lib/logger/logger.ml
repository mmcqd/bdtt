module ErrorCode = struct
  type t = [
    | `NotInScope
    | `NotInferable
    | `Elaboration
    | `Conversion
    | `InternalError
  ]
  let default_severity _ = Asai.Diagnostic.Error

  let to_string : t -> string = function
    | `NotInScope -> "Unbound Variable"
    | `NotInferable-> "Failed to Infer"
    | `Elaboration -> "Elaboration Error"
    | `Conversion -> "Conversion Error"
    | `InternalError -> "Internal Error"
  end

module Locate = Algaeff.Reader.Make(struct type env = Asai.Span.t option end)
let () = Locate.register_printer @@ function
  `Read -> Some "Unhandled Location Reader Effect"

include Asai.Logger.Make(ErrorCode)


type pos = Asai.Span.position =  {
  file_path : string;
  offset : int;
  start_of_line : int;
  line_num : int;
}
  [@@deriving show]

type pos_pair_opt = (pos * pos) option [@@deriving show]

let show_span_opt sp = show_pos_pair_opt (Option.map Asai.Span.to_positions sp)

let fatalf ?severity ?backtrace ?additional_messages code =
  fatalf ?loc:(Locate.read ()) ?severity ?backtrace ?additional_messages code

let tracef x =
  tracef ?loc:(Locate.read ()) x