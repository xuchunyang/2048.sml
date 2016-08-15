(* 2048.sml --- Console version of the game "2048"
 *
 * Copyright (C) 2016  Chunyang Xu
 *
 * Author: Chunyang Xu <xuchunyang.me@gmail.com>
 * Homepage: https://github.com/xuchunyang/2048.sml
 * Version: 0.0
 * Created: Sat Aug 13 10:50:11 UTC 2016
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *)

val board = [0, 2, 0, 0,
             0, 0, 2, 0,
             0, 0, 0, 0,
             0, 0, 0, 0]

fun makeString length init =
  if length = 0
  then ""
  else (Char.toString init) ^ makeString (length - 1) init

fun getColor value =
  case value of
      0    => "\^[[100m"
    | 2    => "\^[[41m"
    | 4    => "\^[[42m"
    | 8    => "\^[[43m"
    | 16   => "\^[[44m"
    | 32   => "\^[[45m"
    | 64   => "\^[[46m"
    | 128  => "\^[[47m"
    | 256  => "\^[[101m"
    | 512  => "\^[[102m"
    | 1024 => "\^[[103m"
    | 2048 => "\^[[104m"
    | _    => ""

fun colorizeString value s =
  (getColor value) ^ s ^ "\^[[0m"

fun centerString length s =
  let
      val extra = length - String.size (s)
      val half_extra = (Real.fromInt extra) / 2.0;
  in
      (makeString (Real.ceil half_extra) #" ") ^ s ^
      (makeString (Real.floor half_extra) #" ")
  end

fun format n =
  centerString 7 (if n = 0 then "*" else Int.toString n)

fun printRow row =
  (List.app (fn n => print (colorizeString n "       ")) row;
   print "\n";
   List.app (fn n => print (colorizeString n (format n))) row;
   print "\n";
   List.app (fn n => print (colorizeString n "       ")) row;
   print "\n")

fun calcScore board =
  let
      val sum = foldl (fn (elt, acc) => elt + acc) 0
      fun times x = if x < 4
                    then 0
                    else if x = 4
                    then 1
                    else 1 + times (x div 2)
  in
      sum (map (fn x => x * (times x)) board)
  end

fun printBoardHeader board =
  let
      val score = Int.toString (calcScore board)
      val whitespaces = makeString (16 - String.size (score)) #" "
  in
      if length board = 16
      then print ("2048.sml" ^ whitespaces ^ score ^ " pts\n\n")
      else ()
  end

exception Unmatch

fun printBoard board =
  case board of
      [] => print "\n        a,w,d,s or q        \n"
    | a::b::c::d::board' => (printBoardHeader board;
                             printRow [a, b, c, d];
                             printBoard board')
    | _ => raise Unmatch

fun zerop x = x = 0
fun countZero xs = List.length (List.filter zerop xs)
val seed = Random.rand (1,1)
fun rand i j = Random.randRange (i, j) seed

fun addRandom board =
  let val number_of_zeros = countZero board
      val random_pos = rand 1 number_of_zeros
      val random_value = if (rand 1 6) = 6 then 4 else 2
      fun loop board pos =
        case board of
            [] => []
          | elt::board' => if elt = 0 andalso pos = 1
                           then random_value :: loop board' 0
                           else if elt <> 0
                           then elt::(loop board' pos)
                           else elt::(loop board' (pos - 1))
  in
      loop board random_pos
  end

fun notZerop x = x <> 0
fun makeList length init =
  if length = 0
  then []
  else init :: makeList (length - 1) init

fun moveLeft board =
  let
      fun rotate row =
        let
            val row_no_zero = List.filter notZerop row
            val padding = makeList (4 - (List.length row_no_zero)) 0
            val new_row = row_no_zero @ padding
        in
            new_row
        end
      fun merge row =
        case row of
            [] => []
          | a::[] => [a]
          | a::b::row' => if a <> 0 andalso a = b
                          then (a + b) :: merge (row' @ [0])
                          else a :: merge (b::row')
      fun moveLeftRow row =
        let
            val new_row = merge (rotate row)
            val successp = new_row <> row
        in
            (successp, new_row)
        end
  in
      case board of
          [] => (false, [])
        | a::b::c::d::board' =>
          let val (successp1, row1) = moveLeft board'
              val (successp2, row2) = moveLeftRow [a, b, c, d]
          in
              (successp1 orelse successp2, row2 @ row1)
          end
        | _ => raise Unmatch
  end

fun rotateBoard board =
  let val [a, b, c, d,
           e, f, g, h,
           i, j, k, l,
           m, n, O, p] = board
  in
      [m, i, e, a,
       n, j, f, b,
       O, k, g, c,
       p, l, h, d]
  end

fun moveUp board =
  let
      val board_tmp = (rotateBoard o rotateBoard o rotateBoard) board
      val (successp, board_new) = moveLeft board_tmp
  in
      (successp, rotateBoard board_new)
  end

fun moveRight board =
  let
      val board_tmp = (rotateBoard o rotateBoard) board
      val (successp, board_new) = moveLeft board_tmp
  in
      (successp, (rotateBoard o rotateBoard) board_new)
  end

fun moveDown board =
  let val board_tmp = rotateBoard board
      val (successp, board_new) = moveLeft board_tmp
  in
      (successp, (rotateBoard o rotateBoard o rotateBoard) board_new)
  end

fun move f board =
  let
      val (succeep, board_new) = f board
  in
      if succeep
      then loop (addRandom board_new)
      else loop board
  end
and loop board =
  let val key = (print "\^[[H"; (* Move cursor home *)
                 printBoard board;
                 TextIO.input1 TextIO.stdIn)
  in
      case key of
          SOME #"q" => ()
        | SOME #"a" => move moveLeft board
        | SOME #"w" => move moveUp board
        | SOME #"d" => move moveRight board
        | SOME #"s" => move moveDown board
        | _ => loop board
  end

val _ = OS.Process.system "clear"
val _ = loop board
