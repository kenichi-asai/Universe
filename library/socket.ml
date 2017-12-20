(* filename : socket.ml *)

(* world と universe の両方で使うもの *)

(* sockfdにmessageを送る *)
let send message sockfd =
  try
    let out_channel = Unix.out_channel_of_descr sockfd in
    Marshal.to_channel out_channel message [];
    flush out_channel with _ -> ()
(* もし送り先が通信から抜けていたら何もしない *)

(* in_channelに対応するsockfdからmessageを受け取って返す *)
let receive sockfd =
  let in_channel = Unix.in_channel_of_descr sockfd in
  let message = Marshal.from_channel in_channel in
  message

(* sockfdにたまっている未処理のデータを破棄 *)
let rec tcflush sockfd =
  try begin
      Unix.set_nonblock sockfd;
      Unix.tcflush sockfd Unix.TCIOFLUSH;
      tcflush sockfd
    end
  with _ -> ()

(* ソケットを作成するための関数 *)
(* make_sockfd : unit -> file_descr *)
let make_sockfd () = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0

(* world と universe の両方が使うものここまで *)

(* universe で使うもの *)
let servermax = 10 (* 受け入れるクライアントの数 *)

(* sockfdをもらってきたらそれをcloseする *)
let close sockfd = Unix.close sockfd

(* msg_event関数の中で使用 *)
(* メッセージが来ていた file_descr のリストを返す *)
let select_read clientlst =
  let (readlst, _, _) = Unix.select clientlst [] [] (-1.) in
  (* 送られてきたデータの受信 *)
  (* メッセージを受け取ったクライアントのリストが作られる *)
  (* マイナスを指定するとタイムアウトがなくなる *)
  readlst

(* new_event 関数の中で使用 *)
(* 接続要求のあったクライアントの file_descr を返す *)
let new_client_sockfd server_sockfd =
  let (client_sockfd, _) = Unix.accept server_sockfd in
  (*接続要求のあったクライアントを受け入れ、それの（file_descr * sockaddr）を
      作る*)
  client_sockfd

let port_of_sockname s = match s with
    Unix.ADDR_UNIX str -> 0 (*ここには入らないから適当な値*)
  | Unix.ADDR_INET (a, b) -> b
    (*ソケットをINETで設定しているので確実にこっちにくる*)

let listen server_sockfd = Unix.listen server_sockfd servermax

(*サーバのポート番号を返す*)
let portnum server_sockfd =
  port_of_sockname (Unix.getsockname server_sockfd)

(* universe で使うものここまで *)

(* world で使うもの *)

let rec try_connection sock_addr_list client_sockfd = match sock_addr_list with
    [] -> failwith "No available connection"
  | {Unix.ai_addr = sock_addr; Unix.ai_canonname = name} :: rest ->
      try
        Unix.connect client_sockfd sock_addr
      with Unix.Unix_error (_, _, _) -> try_connection rest client_sockfd

(*ipアドレスとポート番号の組とclient_sockfdを受け取ったらそれとサーバを接続*)
let connect (ipaddress, portnumber) client_sockfd =
  let sock_addr_list = Unix.getaddrinfo ipaddress portnumber [] in
  try_connection sock_addr_list client_sockfd

(* world で使うものここまで *)
