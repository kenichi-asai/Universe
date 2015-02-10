open Color

(* constants *)
let radius =  30.               (* initial radius of balls *)
let width  = 500.               (* window width *)
let height = 400.               (* window height *)

let table = [|                  (* color table *)
  make_color 0xff 0x00 0x00;
  make_color 0x00 0xff 0x00;
  make_color 0x00 0x00 0xff;
  make_color 0xff 0xff 0x00;
  make_color 0x00 0xff 0xff;
  make_color 0xff 0x00 0xff;
|]

let num_of_color = Array.length table

(* next_color : unit -> Image.color_t *)
let counter = ref (-1)
let next_color () =
  counter := (!counter + 1) mod num_of_color;
  table.(!counter)
