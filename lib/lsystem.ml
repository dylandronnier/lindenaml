(***
      L - S Y S T E M   M O D U L E   D E F I N I T I O N
 ***)

let explode s = List.init (String.length s) (String.get s)
;;

let charList_to_string chars = 
  let buf = Buffer.create 16 in
  List.iter (Buffer.add_char buf) chars;
  Buffer.contents buf

module CharMap = Map.Make(Char);;

(*** L-System Module ***)
module LSystem =
  struct
    type rules = (char List.t) CharMap.t

    let empty_rules = CharMap.empty

    let print_association ch ch_l =
      Printf.printf "%c -> %s \n" ch (charList_to_string ch_l)
      
    let print_rules r =
      CharMap.iter print_association r

    let new_rule  ch s r =
      CharMap.add  ch (explode s) r

    let find_rule ch r =
      charList_to_string (CharMap.find ch r) 

    let rec apply_rules s r = match s with
      | [] -> []
      | t :: q -> (CharMap.find t r) @ (apply_rules q r)

    let rec grow_bis ax r n =
      if n == 0 then ax
      else grow_bis (apply_rules ax r) r (n-1) 
    
    let grow axiom r n = charList_to_string (grow_bis (explode axiom) r n)
    
end

