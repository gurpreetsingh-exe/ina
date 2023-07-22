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
  mutable print_ir : bool;
  mutable display_time : bool;
  mutable display_type_vars : bool;
  mutable ui_testing : bool;
  mutable dot_cfg : bool;
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

let config () : t =
  {
    input = "";
    output = "";
    opt_level = Default;
    output_type = Exe;
    command = Nan;
    print_ir = false;
    display_time = false;
    display_type_vars = false;
    ui_testing = false;
    dot_cfg = false;
  }
