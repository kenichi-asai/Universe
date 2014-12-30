(* filename : image.ml *)

open Color

(* 四角形 *)
type gnorect = {
  color : Color.t;              (* 色 *)
  x1 : int; y1 : int;           (* 左上の座標 *)
  x2 : int; y2 : int;           (* 横幅、高さ *)
}

(* 円 *)
type gnocircle = {
  color : Color.t;              (* 色 *)
  x1 : int; y1 : int;           (* 中心の座標 *)
  x2 : int; y2 : int;           (* 横幅、高さ *)
}

type gnoline = {
  color : Color.t;              (* 色 *)
  points : int list;            (* 点の座標のリスト *)
}

(* テキスト *)
type gnotext = {
  color : Color.t;              (* 色 *)
  text : string;                (* テキスト *)
  x : int; y : int;             (* 左上の座標 *)
  size : int;                   (* フォントのサイズ *)
}

(* 画像 *)
type gnopixbuf = {
  pixbuf : GdkPixbuf.pixbuf;    (* 画像 *)
  x : int; y : int;             (* 左上の座標 *)
  width : int; height : int;    (* 横幅、高さ *)
}

type t = 
    RECT of gnorect
  | CIRCLE of gnocircle
  | LINE of gnoline
  | TEXT of gnotext
  | PIXBUF of gnopixbuf
  | IMAGE of t list

(* empty_scene : int -> int -> Image.t *)
let empty_scene w h =
  RECT {color = white; x1 = 0; y1 = 0; x2 = w; y2 = h}
  
(* rectangle : int -> int -> Color.t -> Image.t *)
let rectangle w h c =
  RECT {color = c; x1 = 0; y1 = 0; x2 = w; y2 = h}

(* circle : int -> Color.t -> Image.t *)
let circle radious c =
  CIRCLE {color = c; x1 = 0; y1 = 0; x2 = 2 * radious; y2 = 2 * radious }

(* line : (int*int) list -> Color.t -> Image.t *)
let line lst c =
  let pointlst= List.flatten (List.map (fun (x,y) -> [x;y]) lst) in
  LINE {color =c; points = pointlst}

(* text : string -> int -> Color.t -> Image.t *)
let text str tsize c =
  TEXT {color = c; text = str; x = 0; y = 0; size = tsize}

(* read_image : string -> Image.t *)
let read_image name = 
  let buf = GdkPixbuf.from_file name in
  PIXBUF {pixbuf = buf; x = 0; y = 0;
          width = GdkPixbuf.get_width buf;
          height = GdkPixbuf.get_height buf}

(* move_one *)
let rec move_one dx dy pic = match pic with
    RECT {color = c; x1 = x; y1 = y; x2 = w; y2 = h} ->
      RECT {color = c; x1 = x + dx; y1 = y + dy; x2 = w; y2 = h}
  | CIRCLE {color = c; x1 = x; y1 = y; x2 = w; y2 = h} ->
      CIRCLE {color = c; x1 = x + dx; y1 = y + dy; x2 = w; y2 = h}
  | LINE {color = c; points = lst} ->
      LINE {color = c; points = lst}
           (* 何もかわりません！ *)
  | TEXT {color = c; text = t; x = x; y = y; size = s} ->
      TEXT {color = c; text = t; x = x + dx; y = y + dy; size = s}
  | PIXBUF {pixbuf = p; x = x; y = y; width = w; height = h} ->
      PIXBUF {pixbuf = p; x = x + dx; y = y + dy; width = w; height = h}
  | IMAGE lst -> IMAGE (List.map (move_one dx dy) lst)

(* place_images : Image.t list -> (int * int) list -> Image.t -> Image.t *)
let rec place_images pics posns background = match (pics, posns) with
    ([], []) -> background
  | ([], _ :: _) -> failwith "place_images: too many posns."
  | (_ :: _, []) -> failwith "place_images: too many pics."
  | (pic :: pics, (dx, dy) :: posns) -> 
      IMAGE (move_one dx dy pic ::
             let background = place_images pics posns background in
             begin match background with
                 IMAGE lst -> lst
               | _ -> [background]
             end)

(* place_image : Image.t -> int * int -> Image.t -> Image.t *)
let rec place_image pic posn background =
  place_images [pic] [posn] background

(* rev_iter *)
let rec rev_iter f lst = match lst with
    [] -> ()
  | first :: rest -> rev_iter f rest; f first

(* image を canvas に描画する *)
(* draw : #GnoCanvas.group -> Image.t -> unit *)
let rec draw canvas image = match image with
    RECT {color = c; x1 = x; y1 = y; x2 = w; y2 = h} ->
      ignore (GnoCanvas.rect ~props:[`FILL_COLOR (Color.to_string c);
                                     `X1 (float_of_int x);
                                     `Y1 (float_of_int y);
                                     `X2 (float_of_int (x + w));
                                     `Y2 (float_of_int (y + h))]
                             canvas)
  | CIRCLE {color = c; x1 = x; y1 = y; x2 = w; y2 = h} ->
      ignore (GnoCanvas.ellipse ~props:[`FILL_COLOR (Color.to_string c);
                                        `X1 (float_of_int x);
                                        `Y1 (float_of_int y);
                                        `X2 (float_of_int (x + w));
                                        `Y2 (float_of_int (y + h))]
                                canvas)
  | LINE {color = c; points =lst} ->
     let p = Array.of_list (List.map float_of_int lst) in
     ignore (GnoCanvas.line ~props:[`FILL_COLOR (Color.to_string c);
                                    `POINTS p]
                                canvas)
  | TEXT {color = c; text = t; x = x; y = y; size = s} ->
      ignore (GnoCanvas.text ~props:[`FILL_COLOR (Color.to_string c);
                                     `SIZE_POINTS (float_of_int s);
                                     `TEXT t;
                                     `X (float_of_int x);
                                     `Y (float_of_int y)]
                             canvas)
  | PIXBUF {pixbuf = p; x = x; y = y; width = w; height = h} ->
      ignore (GnoCanvas.pixbuf ~x:(float_of_int x)
                               ~y:(float_of_int y)
                               ~pixbuf:p
                               canvas)
  | IMAGE lst -> (* 後ろから描画 *)
      rev_iter (draw canvas) lst
