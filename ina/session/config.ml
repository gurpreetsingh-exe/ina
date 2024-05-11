type command =
  | Build
  | Check
  | Fmt
  | Test
  | Nan

type t = {
    mutable input: string
  ; mutable output: string
  ; mutable opt_level: opt_level
  ; mutable output_type: output_type
  ; mutable command: command
  ; mutable backend: backend
  ; mutable print_ir: bool
  ; mutable print_ast: bool
  ; mutable print_module_graph: bool
  ; mutable display_time: bool
  ; mutable display_type_vars: bool
  ; mutable ui_testing: bool
  ; mutable dot_cfg: bool
  ; mutable build_stdlib: bool
  ; mutable skip_codegen: bool
  ; mutable skip_passes: string array option
  ; mutable list_passes: bool
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
  | ExtMod

and backend =
  | C
  | Llvm

let get_opt_level = function
  | "0" -> Some Default
  | "3" -> Some Agressive
  | _ -> None
;;

let get_output_type = function
  | "exe" -> Some Exe
  | "asm" -> Some Asm
  | "obj" -> Some Object
  | "extmod" -> Some ExtMod
  | _ -> None
;;

let config () : t =
  {
    input = ""
  ; output = ""
  ; opt_level = Default
  ; output_type = Exe
  ; command = Nan
  ; backend = C
  ; print_ir = false
  ; print_ast = false
  ; print_module_graph = false
  ; display_time = false
  ; display_type_vars = false
  ; ui_testing = false
  ; dot_cfg = false
  ; build_stdlib = false
  ; skip_codegen = false
  ; skip_passes = None
  ; list_passes = false
  }
;;
