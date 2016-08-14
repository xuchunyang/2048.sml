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

val scores = [[0, 0, 0, 0],
              [0, 0, 0, 0],
              [8, 0, 0, 2],
              [64, 16, 4, 0]]

fun makeString length init =
  if length = 0
  then ""
  else (Char.toString init) ^ makeString (length - 1) init

fun getColor value =
  case value of
      0   => "\^[[100m"
    | 2   => "\^[[41m"
    | 4   => "\^[[42m"
    | 8   => "\^[[43m"
    | 16  => "\^[[44m"
    | 32  => "\^[[45m"
    | 64  => "\^[[46m"
    | 128 => "\^[[47m"
    | 256 => "\^[[101m"
    | 512 => "\^[[102m"
    | _   => ""

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

fun printBoard scores =
  case scores of
      [] => print "\n        a,w,d,s or q        \n"
    | row::scores' => ((if length scores = 4
                        then print "2048.sml               0 pts\n\n"
                        else ());
                       printRow row;
                       printBoard scores')

fun loop scores =
  let val key = (print "\^[[H";
                 printBoard scores;
                 TextIO.input1 TextIO.stdIn)
  in
      case key of
          SOME #"q" => ()
        | SOME #"a" => loop [[0, 0, 0, 0],
                             [0, 0, 0, 2],
                             [8, 0, 2, 2],
                             [64, 16, 4, 0]]
        | SOME #"w" => loop [[0, 0, 0, 0],
                             [8, 0, 0, 2],
                             [64, 16, 4, 0],
                             [0, 0, 2, 0]]
        | SOME #"d" => loop [[0, 0, 0, 0],
                             [2, 0, 0, 0],
                             [8, 0, 0, 2],
                             [64, 16, 4, 0]]
        | SOME #"s" => loop [[2, 0, 0, 0],
                             [0, 0, 0, 0],
                             [8, 0, 0, 4],
                             [64, 16, 4, 0]]
        | _ => loop scores
  end

val _ = OS.Process.system "clear"
val _ = loop scores
