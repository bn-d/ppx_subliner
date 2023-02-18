let english ~night name =
  if night then
    Printf.printf "\nGood night, %s\n" name
  else
    Printf.printf "\nGood morning, %s\n" name

let chinese ~night name =
  if night then
    Printf.printf "\n晚上好, %s\n" name
  else
    Printf.printf "\n早上好, %s\n" name

let programmer () = Printf.printf "\nHello world!\n"
