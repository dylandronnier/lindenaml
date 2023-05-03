(**
   EXPORTED SIGNATURE OF LSystem MODULE
 **)

module LSystem :
sig
  type rules
  val empty_rules : rules
  val print_rules : rules -> unit
  val new_rule : char -> string -> rules -> rules
  val find_rule : char -> rules -> string
  val grow : string -> rules -> int -> string
end
