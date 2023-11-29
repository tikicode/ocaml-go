(* let () =
  Dream.run ~interface:"0.0.0.0" ~port:3000 (fun _ ->
    Dream.html "Hello world!") *)


type data = {
  message: string;
}
[@@deriving yojson]

(* type input_data = {
  input: string;
}
[@@deriving yojson] *)

let api_handler _req =
  let data = { message = "Hello, Dream API!" } in
  Dream.json (Yojson.Safe.to_string (data_to_yojson data))

let post_handler request =
  let%lwt body = Dream.body request in
  Dream.html (Printf.sprintf "Received POST request with body: %s" body)


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