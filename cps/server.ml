(* Implement the server operations *)

exception BadCmd
    
module DBMox = Map.Make(struct
  type t = string
  let compare = Stdlib.compare
end)
    
let room_assn : (string DBMox.t) ref = ref DBMox.empty
let _ =
  room_assn := DBMox.add "Kevin" "Winthrop 41" !room_assn;
  room_assn := DBMox.add "Ye Joo" "Winthrop 41" !room_assn;
  room_assn := DBMox.add "Diego" "Winthrop 42" !room_assn;
  room_assn := DBMox.add "Alonzo" "Winthrop 43" !room_assn;
  room_assn := DBMox.add "Grace" "Currier 8" !room_assn;
  room_assn := DBMox.add "Alan" "Currier 16" !room_assn;
  room_assn := DBMox.add "George" "Currier 32" !room_assn;
  room_assn := DBMox.add "Ada" "Currier 32" !room_assn;
  room_assn := DBMox.add "Evelyn" "Currier 64" !room_assn;
  room_assn := DBMox.add "Carol" "Currier 64" !room_assn
      
let secret_pass : (string DBMox.t) ref = ref DBMox.empty
let _ =
  secret_pass := DBMox.add "Currier 8" "Currier 16" !secret_pass;
  secret_pass := DBMox.add "Currier 16" "Currier 32" !secret_pass;
  secret_pass := DBMox.add "Currier 32" "Currier 64" !secret_pass;
  secret_pass := DBMox.add "Winthrop 41" "Winthrop 42" !secret_pass

let num : int ref = ref 0
      
let do_server_op (op : string) (arg : string) : string =
  match op with
  | "echo" -> arg
  | "double" -> arg ^ arg
  | "GreetingService" -> "Hello " ^ arg
  | "getRoom" -> 
      (try DBMox.find arg !room_assn
      with Not_found -> "nonexistent entry")
  | "getSecretPassage" -> 
      (try DBMox.find arg !secret_pass
      with Not_found -> "nonexistent entry")
  | "getInt" ->
      let ans = !num in
      num := ans + 1;
      string_of_int ans
  | _ -> raise BadCmd
	
