(**     Image module 
    @ja 画像モジュール *)

type t
(** @en [Image.t] : the type of images 
    @ja [Image.t] : 画像の型 *)

val empty_scene : int -> int -> t
(** @en [empty_scene width height] : creates a plain background of size
        [width] x [height] 
    @ja [empty_scene width height] : 横 [width]、縦 [height] の無地の
        背景を作る *)

val rectangle : int -> int -> Color.t -> t
(** @en [rectangle width height color] : creates a rectangle of size
        [width] x [height] with [color] 
    @ja [rectangle width height color] : 横 [width]、縦 [height]、
        色 [color] の長方形を作る *)

val circle : int -> Color.t -> t
(** @en [circle radius color] : creates a circle of [radius] with [color] 
    @ja [circle radius color] : 半径 [radius]、色 [color] の円を作る *)

val line : (int * int) list -> Color.t -> t
(** @en [line [(x, y);...] color] : creates a line with [color]
        connecting [(x, y)]'s
    @ja [line [(x, y);...] color] : [(x, y)] を繋げた、色 [color] の線を作る *)

val text : string -> int -> Color.t -> t
(** @en [text str size color] : creates a string [str] of [size] point
        with [color] 
    @ja [text str size color] : サイズ [size] ポイント、色 [color] の
        文字 [str] 列を作る *)

val read_image : string -> t
(** @en [read_image file] : reads an image from [file] (png, jpg, bmp, or gif)
    @ja [read_image file] : ファイル名 [file] の画像ファイル (png, jpg, bmp, または gif) を読み込む *)

val place_image : t -> int * int -> t -> t
(** @en [place_image image (x, y) scene] : places an [image] at
        coordinates [(x, y)] on [scene] 
    @ja [place_image image (x, y) scene] : 画像 [scene] の上に
        画像 [image] を [(x, y)] の位置に置く *)

val place_images : t list -> (int * int) list -> t -> t
(** @en [place_images images posns scene] : places [images] respectively at
        coordinates [posns] on [scene] 
    @ja [place_images images posns scene] : 画像 [scene] の上に
        画像の列 [images] をそれぞれ [posns] の位置に置く *)

(**/**)
val draw : #GnoCanvas.group -> t -> unit
