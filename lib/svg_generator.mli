(**
   EXPORTED SIGNATURE OF 
            Color
            Turtle
            SVG_draw
   MODULES
 **)

module Color :
sig
  type color = private { r : int; g : int; b : int }
  val red: color
  val green: color
  val blue: color
  val white: color
  val black: color
  val string_of_color: color -> string
  val color_of_ints: int -> int -> int -> color
end


module Turtle :
sig
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
  type path
  val interprete : string -> interpretation -> path
  val isCorrect : path -> bool
end


module SVG_draw :
sig
  type pen
  val init_pen : float -> float -> float -> int -> Color.color -> pen
  val init_svg : out_channel -> int -> int -> unit
  val end_svg : out_channel -> unit
  val draw_forward : out_channel -> pen -> float -> pen
  val turn : pen -> float -> pen
  val resize : pen -> int -> pen
  val change_color : pen -> Color.color -> pen
  val draw_path : out_channel -> pen -> Turtle.path -> float -> unit
end
  
