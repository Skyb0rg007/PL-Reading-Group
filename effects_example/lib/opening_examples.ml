type user = { username : string ;
              password : string }

let print_user_info { username ; password } =
  Printf.printf "Username: %s\nPassword: %s\n" username password

let safe_print info = 
  if info.username = "matt"
  then print_user_info
         { info with password = "XXXXX" }
  else print_user_info info
