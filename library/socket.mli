(**     Socket module 
    @ja ソケットモジュール *)

(**     used in both universe module and world module 
    @ja universe モジュールと world モジュールの両方で使うもの *)

val close : Unix.file_descr -> unit
(** @en close file_descr
    @ja file_descrを閉じる *)

val send : 'a -> Unix.file_descr -> unit
(** @en send a message 
    @ja メッセージを送る *)

val receive : Unix.file_descr -> 'a
(** @en receive a message 
    @ja メッセージを受け取る *)

val tcflush : Unix.file_descr -> unit
(** @en destroy unprocessed data in sockfd
    @ja sockfd にたまっている未処理のデータを破棄 *)

val make_sockfd : unit -> Unix.file_descr
(** @en make a socket  
    @ja ソケットを作成する *)

(**     used in universe module 
    @ja universe モジュールで使うもの *)

val select_read : Unix.file_descr list -> Unix.file_descr list
(** @en return a list of file_descr which got messages
    @ja メッセージが来ていた file_descr のリストを返す  *)

val new_client_sockfd : Unix.file_descr -> Unix.file_descr 
(** @en return file_descr which has connection request
    @ja 接続要求のあったクライアントの file_descr を返す *)

val portnum : Unix.file_descr -> int
(** @en receive Unix.file_descr of a server and return the port number
        of the server 
    @ja サーバの Unix.file_descr をもらってサーバのポート番号を返す *)

val listen : Unix.file_descr -> unit
(** @en setup to receive connection requests to Unix.file_descr
    @ja Unix.file_descr への connection requests を受け取るセットアップをする *)

(**     used in world module 
    @ja world モジュールで使うもの *)

val connect : (string * string) -> Unix.file_descr -> unit
(** @en receive a pair of an ip address and a port number, and client_sockfd;
        then connect to a server
    @ja ip アドレスとポート番号の組と client_sockfd を受け取ったら
        それとサーバを接続 *)
