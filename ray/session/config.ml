type command =
  | Build
  | Fmt
  | Test
  | Nan

type t = {
  mutable input : string;
  mutable output : string;
  mutable opt_level : opt_level;
  mutable output_type : output_type;
  mutable command : command;
}

and opt_level =
  (* no opt *)
  | Default
  (* -03 *)
  | Agressive

and output_type =
  | Exe
  | Asm
  | Object
  | LlvmIr

let get_opt_level = function
  | "0" -> Some Default
  | "3" -> Some Agressive
  | _ -> None

let get_output_type = function
  | "exe" -> Some Exe
  | "asm" -> Some Asm
  | "obj" -> Some Object
  | "llvm-ir" -> Some LlvmIr
  | _ -> None

let config input : t =
  {
    input;
    output = List.hd (String.split_on_char '.' input);
    opt_level = Default;
    output_type = Exe;
    command = Nan;
  }
