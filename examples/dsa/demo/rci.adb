------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                                  R C I                                   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2002-2009, Free Software Foundation, Inc.          --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
-- not, write to the Free Software Foundation, 51 Franklin Street, Fifth    --
-- Floor, Boston, MA 02111-1301, USA.                                       --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Text_IO; use Ada.Text_IO;
with System.Address_Image;
with Ada.Real_Time; use Ada.Real_Time;

package body RCI is

   type String_Ptr is access all String;
   type Real_Obj is new RT.Obj with record
      Name : String_Ptr;
   end record;
   type Real_Obj_Ptr is access Real_Obj;
   procedure Method (Self : Real_Obj);
   procedure Method2 (Self : Real_Obj; N : Integer);
   procedure Method3 (Self : Real_Obj; Other : Real_Obj);
   function Tekitoa (Self : Real_Obj) return String;

   procedure Method (Self : Real_Obj) is
   begin
      Put_Line ("Method1 called on " & System.Address_Image (Self'Address));
   end Method;

   procedure Method2 (Self : Real_Obj; N : Integer) is
   begin
      Put_Line ("Method2 (" & N'Img & ") called on "
        & System.Address_Image (Self'Address));
   end Method2;

   procedure Method3 (Self : Real_Obj; Other : Real_Obj) is
   begin
      Put_Line ("Method2 (" & System.Address_Image (Other'Address)
         & ") called on " & System.Address_Image (Self'Address));
   end Method3;

   function Tekitoa (Self : Real_Obj) return String is
   begin
      Put_Line ("I am " & Self.Name.all);
      return Self.Name.all;
   end Tekitoa;

   procedure My_Proc (X : Integer; Y : in out Predicate; Z : out Trit) is
   begin
      Y := Y and then Predicate (X = 0);
      Z := 1;
   end My_Proc;

   function My_Func (S : String) return Color is
   begin
      return Color'Value (S);
   end My_Func;

   function Get_Obj (Name : String) return RT.RACW is
      P : Real_Obj_Ptr;
   begin
      if Name = "" then
         return null;
      else
         P := new Real_Obj;
         P.Name := new String'(Name);
         return RT.RACW (P);
      end if;
   end Get_Obj;

   function echoVector (V : Vector) return Vector is
   begin
      Put_Line ("Got vector" & Integer'Image (V'First)
                & " .." & Integer'Image (V'Last) & ":");
      for J in V'Range loop
         Put (Integer'Image (V (J)));
      end loop;
      New_Line;
      return V;
   end echoVector;

   function echoTranspose (M : Matrix) return Matrix is
   begin
      Put_Line ("Got matrix:");
      Put_Line ("Ranges of M : (" & Integer'Image (M'First (1))
                           & ".." & Integer'Image (M'Last (1))
                           & ", " & Integer'Image (M'First (2))
                           & ".." & Integer'Image (M'Last (2)) & ")");
      for J in M'Range (1) loop
         for K in M'Range (2) loop
            Put (" " & Float'Image (M (J, K)));
         end loop;
         New_Line;
      end loop;
      return Matrices.Transpose (M);
   end echoTranspose;

   function echoString (S : String) return String is
   begin
      Put_Line ("Thus spake my client unto me: «" & S & "».");
      return S;
   end echoString;

   function getRAS return echo_RAS is
      Func : constant echo_RAS := echoString'Access;
   begin
      Put_Line (Func.all ("Checking local (bypass) RAS call"));
      return Func;
   end getRAS;

   procedure Check_Back_RAS (Func : echo_RAS; S : String) is
   begin
      Put_Line (Func.all ("Cheking RAS sent back by client: " & S));
   end Check_Back_RAS;

   function echoString_Delayed (S : String; Seconds : Integer) return String is
   begin
      delay until Clock + To_Time_Span (Duration (Seconds));
      return echoString (S);
   end echoString_Delayed;

   function Modulus2 (Z : Complex) return Float is
   begin
      return Z.Re * Z.Re + Z.Im * Z.Im;
   end Modulus2;

   Cookie : Integer := 0;

   function echoC_4_5 (X : C_4_5) return C_4_5 is
   begin
      return X;
   end echoC_4_5;

   function Get_Cookie return Integer is
   begin
      return Cookie;
   end Get_Cookie;

   procedure Delayed_Set_Cookie (Cookie : Integer) is
   begin
      delay until Clock + Milliseconds (2_000);
      RCI.Cookie := Cookie;
   end Delayed_Set_Cookie;

   procedure Raise_Program_Error is
   begin
      raise Program_Error;
   end Raise_Program_Error;

   procedure Raise_Visible is
   begin
      raise Visible;
   end Raise_Visible;

   procedure Raise_Invisible is
      Invisible : exception;
   begin
      raise Invisible;
   end Raise_Invisible;

   procedure Add (X : Integer; To : in out RT.Limited_Data) is
      To_Val : Integer;
      for To_Val'Address use To'Address;
      pragma Import (Ada, To_Val);
   begin
      To_Val := To_Val + X;
   end Add;

end RCI;
