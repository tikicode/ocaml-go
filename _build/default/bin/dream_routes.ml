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

let () =
  Dream.run 
  @@ Dream.logger
  @@ Dream.router [
    Dream.get "/api" api_handler;

    Dream.get "/api/:word"
      (fun request ->
        let content = (Dream.param request "word") in 
          match content with 
          | "2 2" -> Dream.html "hello"
          | _ -> Dream.html "world"
      );
  ]
(* 
  let () =
  Dream.run
  @@ Dream.logger
  @@ Dream.router [

    Dream.get "/"
      (fun _ ->
        Dream.html "Good morning, world!");

    Dream.get "/echo/:word"
      (fun request ->
        Dream.html (Dream.param request "word"));

  ] *)