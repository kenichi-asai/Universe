(**     Image module
    @ja 画像モジュール
    @see <http://docs.racket-lang.org/teachpack/2htdpimage.html> Racket Documentation *)

type t
(** @en [Image.t] : the type of images
    @ja [Image.t] : 画像の型 *)

val empty_scene : float -> float -> t
(** @en [empty_scene width height] : creates a plain background of size
        [width] x [height]
    @ja [empty_scene width height] : 横 [width]、縦 [height] の無地の
        背景を作る *)

val rectangle : float -> float ->
                ?fill:bool -> ?outline_size:float -> Color.t -> t
(** @en [rectangle width height color] : creates a rectangle of size
        [width] x [height] with outline [color].
        The outline width is [outline_size] point.
        Filled with [color] if [fill] is true.
    @ja [rectangle width height color] : 横 [width]、縦 [height]、
        色 [color] の長方形を作る。
        外枠の線のサイズは [outline_size] ポイント、
        [fill] が true の場合塗りつぶされる *)

val polygon : (float * float) list ->
              ?fill:bool -> ?outline_size:float -> Color.t -> t
(** @en [polygon [(x, y);...] color] : creates a polygon with [color]
        connecting [(x, y)]'s.
        The outline width is [outline_size] point.
        Filled with [color] if [fill] is true.
    @ja [polygon [(x, y);...] color] : [(x, y)] を繋げた
        色 [color] の多角形を作る。
        外枠の線のサイズは [outline_size] ポイント、
        [fill] が true の場合塗りつぶされる *)

val circle : float -> ?fill:bool -> ?outline_size:float -> Color.t -> t
(** @en [circle radius color] : creates a circle of [radius] with [color].
        The outline width is [outline_size] point.
        Filled with [color] if [fill] is true.
    @ja [circle radius color] : 半径 [radius]、色 [color] の円を作る。
        外枠の線のサイズは [outline_size] ポイント、
        [fill] が true の場合塗りつぶされる *)

val line : (float * float) list -> ?size:float -> Color.t -> t
(** @en [line [(x, y);...] color] : creates a line with [color]
        starting from [(0, 0)] connecting [(x, y)]'s.
        The line width is [size] point.
    @ja [line [(x, y);...] color] : [(0, 0)] から初めて [(x, y)] を
        繋げた色 [color] の線を作る。
        線のサイズは [size] ポイント *)

val text : string -> ?size:float -> Color.t -> t
(** @en [text str color] : creates a string [str] with [color].
        The font size is [size] point.
    @ja [text str color] : 色 [color] の文字列 [str] を作る。
        サイズは [size] ポイント *)

val read_image : string -> t
(** @en [read_image file] : reads an image from [file] (png)
    @ja [read_image file] : ファイル名 [file] の画像ファイル (png) を読み込む *)

val place_image : t -> float * float -> t -> t
(** @en [place_image image (x, y) scene] : places an [image] at
        coordinates [(x, y)] on [scene]
    @ja [place_image image (x, y) scene] : 画像 [scene] の上に
        画像 [image] を [(x, y)] の位置に置く *)

val place_images : t list -> (float * float) list -> t -> t
(** @en [place_images images posns scene] : places [images] respectively at
        coordinates [posns] on [scene]
    @ja [place_images images posns scene] : 画像 [scene] の上に
        画像の列 [images] をそれぞれ [posns] の位置に置く *)

val to_bitmap : t -> Cairo.Image.data32
(** @en [to_bitmap image] : converts [image] to bitmap of type
        {{:http://cairo.forge.ocamlcore.org/tutorial/Cairo.Image.html#TYPEdata32} data32}.
    @ja [to_bitmap image] : 画像 [image] を {{:http://cairo.forge.ocamlcore.org/tutorial/Cairo.Image.html#TYPEdata32} data32} 型のビットマップに
        出力する *)

val from_bitmap : Cairo.Image.data32 -> t
(** @en [from_bitmap data] : converts [data] of type
        {{:http://cairo.forge.ocamlcore.org/tutorial/Cairo.Image.html#TYPEdata32} data32} to an image
    @ja [from_bitmap data] : {{:http://cairo.forge.ocamlcore.org/tutorial/Cairo.Image.html#TYPEdata32} data32} 型のビットマップ [data] を
        画像として取り込む *)

val freeze : t -> t
(** @en [freeze image] : freezes [image] (as bitmap)
    @ja [freeze image] : 画像 [image] を（bitmap に）固める *)

(**/**)
val draw : Cairo.context -> t -> unit
