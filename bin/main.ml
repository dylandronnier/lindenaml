(***

       M A I N   P R O G R A M  H I L B E R T   C U R V E

 **)

open Lindenaml.Svg_generator
open Lindenaml.Lsystem

let pi = Float.pi

let _usage_msg = "lindenaml [-style <file1>] -rules <file2> -output <output>"
let output_file = ref ""
let style_file = ref ""
let rules_file = ref ""


let _speclist = [
  ("-style", Arg.Set_string style_file, "Set style file name");
  ("-rules", Arg.Set_string rules_file, "Set rules file name (JSON)");
  ("-output", Arg.Set_string output_file, "Set output file name");
]

let joliecouleur = Color.color_of_ints 200 150 90 

let line_width = 2
let line_length = 15.
let generation = 6       (* Generations number *)
  
let origin = SVG_draw.init_pen 10.0 10.0 (pi /. 2.) line_width joliecouleur

(**  EXAMPLE OF IMPLEMENTATION
La courbe de Hilbert peut aussi être construite par un L-système3 :
    Alphabet : L, R
    Constantes : F, +, −
    Axiome : L
    Règles :
    L → –RF+LFL+FR−
    R → +LF−RFR−FL+
Ici, F signifie « avance », + signifie « à gauche 90° », et − signifie « à droite 90° ». 
 **)

let myRules = ref LSystem.empty_rules;;
(* List.iter (fun (c,s) -> myRules := LSystem.new_rule c s !myRules) my_rules_list;; *)

let blabla = LSystem.grow "L" !myRules generation

(** Turtle Interpretation following Houdini **)
let houdini_interpretation ch = match ch with
  | '+' -> Turtle.Turn (pi /. 2.)                                      (* Turn 90° *)
  | '-' -> Turtle.Turn (-. pi /. 2.)                                     (* Turn -90° *)
  | 'F' -> Turtle.MoveForward                                           (* Move forward *)
  | 'f' -> Turtle.MoveForward
  | '[' -> Turtle.PushState
  | ']' -> Turtle.PopState
  | _ -> Turtle.DoNothing                                               (* Do nothing *)

let () =
  (* Arg.parse speclist (fun file -> input_file := file) usage_msg *)

  (* Your program code goes here *)


  let channel = open_in "config.txt" in
  let _content = really_input_string channel (in_channel_length channel) in
  if (Array.length Sys.argv) < 2 then print_endline "Donnez un nom de fichier"
  else
    let input_file = Sys.argv.(1) in
    let oc = open_out input_file in
    SVG_draw.init_svg oc 1000 1000;
    SVG_draw.draw_path oc origin (Turtle.interprete blabla houdini_interpretation) line_length;
    SVG_draw.end_svg oc;
    close_out oc
