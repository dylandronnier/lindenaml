(***
       M O D U L E S    D E F I N I T I O N
 ***)

open List

(*** Apply f function to the first element of the list ***)
let apply_to_head f l = match l with
  | [] -> []
  | t :: q -> (f t) :: q

let explode s = List.init (String.length s) (String.get s)

(** Color management Module **)
module Color =
struct
  type color = { r : int; g : int; b : int }

  let red =  {r = 255 ; g = 0 ; b = 0 }
  let green =  {r = 0 ; g = 255 ; b = 0 }
  let blue = {r = 0 ; g = 0 ; b = 255 }
  let white = {r = 0 ; g = 0 ; b = 0 }
  let black = {r = 255 ; g = 255 ; b = 255 }
    
  let string_of_color c =
    "rgb(" ^ string_of_int c.r ^ "," ^ string_of_int c.g ^ "," ^ string_of_int c.b ^ ")"
    
  let color_of_ints r g b =
    {r = r ; g = g ; b = b }
end

(** Turtle management Module **)
module Turtle =
struct
  type order =
    | DoNothing
    | MoveForward
    | MoveForwardandDraw
    | Turn of float
    | ChangeColor of Color.color
    | Resize of int
    | PushState
    | PopState

  type interpretation = char -> order
  type path = order List.t
    
  let interprete s myinterpretation =
    map myinterpretation (explode s)

  let rec count acc l = match l with
    | [] -> acc
    | PushState :: q -> count (acc+1) q
    | PopState :: q -> count (acc-1) q
    | _ :: q -> count acc q

  let isCorrect p = (count 0 p) >= 0
end

(**
Management Module:
    - for SVG file creation
    - for the tree tracing
**)
module SVG_draw =
struct
  type pen =
    {
      x: float;
      y: float;
      angle: float;
      width: int;
      col: Color.color
    }

  let init_pen x y a w c =
    {
      x = x;
      y = y;
      angle = a;
      width = w;
      col = c
    }

  let line1 = "<?xml version=\"1.0\" encoding=\"utf-8\"?>"
  let line2 = "<svg xmlns=\"http://www.w3.org/2000/svg\" version=\"1.1\" "
  
  let init_svg oc w h =
    Printf.fprintf oc "%s\n" line1;
    Printf.fprintf oc "%s width='%d' height='%d' >\n" line2 w h
  
  let end_svg oc = Printf.fprintf oc "</svg>\n"

  let write_svg_line x1 y1 x2 y2 width c =
    Printf.sprintf " <line x1='%f' y1='%f' x2='%f' y2='%f' stroke='%s' stroke-width='%d' />\n"
      x1 y1 x2 y2 (Color.string_of_color c) width

  let draw_forward oc pen z = 
    let nx = pen.x +. z *. cos pen.angle
    and ny = pen.y +. z *. sin pen.angle in
    Printf.fprintf oc "%s" (write_svg_line pen.x pen.y nx ny pen.width pen.col);
    init_pen nx ny pen.angle pen.width pen.col

  let move_forward pen z =
    let nx = pen.x +. z *. cos pen.angle
    and ny = pen.y +. z *. sin pen.angle in
    init_pen nx ny pen.angle pen.width pen.col

  let resize pen w =
    if w + pen.width > 0 then
      init_pen pen.x pen.y pen.angle (pen.width + w) pen.col
    else
      pen

  let turn pen a =
    init_pen pen.x pen.y (pen.angle +. a) pen.width pen.col

  let change_color pen c =
    init_pen pen.x pen.y pen.angle pen.width c

  let draw_list oc pen_list z =
    let draw_bis pen = draw_forward oc pen z in
    apply_to_head draw_bis pen_list

  let turn_list pen_list a =
    let turn_bis pen = turn pen a in
    apply_to_head turn_bis pen_list

  let change_color_list pen_list c =
    let change_color_bis pen = change_color pen c in
    apply_to_head change_color_bis pen_list

  let resize_list pen_list w =
    let resize pen = resize pen w in
    apply_to_head resize pen_list
  
  let rec draw_path_bis oc pen_stack path len =
    if path == [] then ()
    else match hd path with
       | Turtle.DoNothing -> draw_path_bis oc pen_stack (tl path) len
       | Turtle.MoveForwardandDraw -> draw_path_bis oc (draw_list oc pen_stack len) (tl path) len
       | Turtle.MoveForward -> draw_path_bis oc (draw_list oc pen_stack len) (tl path) len
       | Turtle.Turn a -> draw_path_bis oc (turn_list pen_stack a) (tl path) len
       | Turtle.ChangeColor c -> draw_path_bis oc (change_color_list pen_stack c) (tl path) len 
       | Turtle.Resize w -> draw_path_bis oc (resize_list pen_stack w) (tl path) len
       | Turtle.PushState -> draw_path_bis oc ((hd pen_stack) :: pen_stack) (tl path) len
       | Turtle.PopState -> draw_path_bis oc (tl pen_stack) (tl path) len

  let draw_path oc pen chemin len =
    if not (Turtle.isCorrect chemin) then failwith "Incorrect path"
    else draw_path_bis oc [pen] chemin len
end
