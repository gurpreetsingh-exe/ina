open Format

let ( $ ) f g h = f (g h)

module Colors = struct
  type t =
    | Black
    | Red
    | Green
    | Yellow
    | Blue
    | Magenta
    | Cyan
    | White

  let to_int = function
    | Black -> 30
    | Red -> 31
    | Green -> 32
    | Yellow -> 33
    | Blue -> 34
    | Magenta -> 35
    | Cyan -> 36
    | White -> 37
  ;;

  let fg' ?(bright = false) = if bright then ( + ) 60 $ to_int else to_int
  let bg' ?(bright = false) = ( + ) 10 $ fg' ~bright
  let fg ?(bright = false) = string_of_int $ fg' ~bright
  let bg ?(bright = false) = string_of_int $ bg' ~bright
end

type t =
  | Normal
  | Bold
  | Faint
  | Italic
  | Underline
  | Fg of Colors.t
  | BrightFg of Colors.t
  | Bg of Colors.t
  | BrightBg of Colors.t
  | F'rgb of (int * int * int)
  | B'rgb of (int * int * int)
  | Fixed of int

let prefix = "\x1b["
let reset = prefix ^ "0m"

let to_int = function
  | Normal -> 0
  | Bold -> 1
  | Faint -> 2
  | Italic -> 3
  | Underline -> 4
  | Fg c -> Colors.fg' c
  | BrightFg c -> Colors.fg' ~bright:true c
  | Bg c -> Colors.bg' c
  | BrightBg c -> Colors.bg' ~bright:true c
  | Fixed i -> i
  | F'rgb _ -> assert false
  | B'rgb _ -> assert false
;;

let color = function
  | F'rgb (r, g, b) -> sprintf "38;2;%d;%d;%d" r g b
  | B'rgb (r, g, b) -> sprintf "48;2;%d;%d;%d" r g b
  | Fixed i -> sprintf "38;5;%d" i
  | c -> to_int c |> string_of_int
;;

let state = ref [30000; 15000; 35000]
let magic = List.rev [5.; 30.; 180.]
let bits = 0xffff
let b = 0.5

let next () =
  state :=
    List.mapi (fun i s -> (s + (40503 * ((i * 4) + 1130))) land bits) !state;
  let c =
    16
    + (int_of_float
         (List.map2
            (fun s m ->
              ((float_of_int s /. float_of_int bits *. (1. -. b)) +. b) *. m)
            !state
            magic
          |> List.fold_left ( +. ) 0.)
       land 0xff)
  in
  Fixed c
;;

let format segments =
  let rec loop = function
    | [] -> []
    | (c, s) :: rest -> sprintf "%s%sm%s" prefix (color c) s :: loop rest
  in
  let fmt = loop segments in
  sprintf "%s%s" (String.concat "" fmt) reset
;;

let grey = F'rgb (150, 150, 150)
let red = F'rgb (255, 70, 70)
let margin = Fixed 246
let () = ()
