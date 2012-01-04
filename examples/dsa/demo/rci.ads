------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                                  R C I                                   --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2002-2012, Free Software Foundation, Inc.          --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.                                               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

with Matrices; use Matrices;
with RT;

package RCI is

   pragma Remote_Call_Interface;
   --  pragma All_Calls_Remote;

   type Color is (Red, Green, Blue);

   type Hue is new Color;
   --  Expanded to: type hueB is new Color; subtype hue is hueB;

   subtype Tint is Color;

   type Predicate is new Boolean;
   subtype Predicate2 is Predicate;

   type Predicate3 is new Predicate;
   subtype Predicate4 is Predicate3;

   type Trit is range 0 .. 2;

   procedure My_Proc (X : Integer; Y : in out Predicate; Z : out Trit);

   function My_Func (S : String) return Color;

   function Get_Obj (Name : String) return RT.RACW;

   type Vector is array (Integer range <>) of Integer;
   function echoVector (V : Vector) return Vector;

   function echoTranspose (M : Matrix) return Matrix;

   function echoString (S : String) return String;
   type echo_RAS is access function (S : String) return String;

   function getRAS return echo_RAS;
   procedure Check_Back_RAS (Func : echo_RAS; S : String);

   function echoString_Delayed (S : String; Seconds : Integer) return String;

   type Complex is record
      Re, Im : Float;
   end record;

   function Modulus2 (Z : Complex) return Float;

   type C_4_5 is array (0 .. 3, 0 .. 4) of Complex;

   function echoC_4_5 (X : C_4_5) return C_4_5;

   procedure Add (X : Integer; To : in out RT.Limited_Data);

   --  type Parameterless_RAS is access procedure;

   function Get_Cookie return Integer;
   procedure Delayed_Set_Cookie (Cookie : Integer);
   pragma Asynchronous (Delayed_Set_Cookie);

   Visible : exception;

   procedure Raise_Program_Error;
   procedure Raise_Visible;
   procedure Raise_Invisible;

end RCI;
