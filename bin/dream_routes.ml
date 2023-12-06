(* let () =
  Dream.run ~interface:"0.0.0.0" ~port:3000 (fun _ ->
    Dream.html "Hello world!") *)




type data = {
  message: string;
}
[@@deriving yojson]

type data_list = data list [@@deriving yojson]

let create_data_list : data_list =
  [
    {message = "Hello"};
    {message = "World"};
  ]

let post_handler _ =
  (* let%lwt body = Dream.body request in *)
  let headers = [
    "Access-Control-Allow-Origin", "*";
    "Access-Control-Allow-Methods", "GET, POST, OPTIONS";
    "Access-Control-Allow-Headers", "Content-Type";
  ] in
  Dream.json ~headers (Yojson.Safe.to_string (data_list_to_yojson create_data_list))
  (* (Printf.sprintf "Received POST request with body: %s" body) *)

let api_handler _ = 
  let headers = [
    "Access-Control-Allow-Origin", "*";
    "Access-Control-Allow-Methods", "GET, POST, OPTIONS";
    "Access-Control-Allow-Headers", "Content-Type";
  ] in
  let data = { message = "Hello, Dream API!" } in
  Dream.json ~headers (Yojson.Safe.to_string (data_to_yojson data))


let () =
  Dream.run 
  @@ Dream.logger
  
  @@ Dream.router [
    Dream.get "/api" api_handler;
  
    (* adding parametrized url instead *)
    Dream.get "/api/:word"
      (fun request ->
        let content = (Dream.param request "word") in 
          match content with 
          | "2_2" -> let data = { message = content } in Dream.json ((Yojson.Safe.to_string (data_to_yojson data)))
          | _ -> Dream.html "world"
      );
    
    Dream.post "/api/p" post_handler;

  ]