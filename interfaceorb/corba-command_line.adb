-----------------------------------------------------------------------
-----------------------------------------------------------------------
----                                                               ----
----                         AdaBroker                             ----
----                                                               ----
----                   package Corba.Command_Line                  ----
----                                                               ----
----                                                               ----
----   Copyright (C) 1999 ENST                                     ----
----                                                               ----
----   This file is part of the AdaBroker library                  ----
----                                                               ----
----   The AdaBroker library is free software; you can             ----
----   redistribute it and/or modify it under the terms of the     ----
----   GNU Library General Public License as published by the      ----
----   Free Software Foundation; either version 2 of the License,  ----
----   or (at your option) any later version.                      ----
----                                                               ----
----   This library is distributed in the hope that it will be     ----
----   useful, but WITHOUT ANY WARRANTY; without even the implied  ----
----   warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR     ----
----   PURPOSE.  See the GNU Library General Public License for    ----
----   more details.                                               ----
----                                                               ----
----   You should have received a copy of the GNU Library General  ----
----   Public License along with this library; if not, write to    ----
----   the Free Software Foundation, Inc., 59 Temple Place -       ----
----   Suite 330, Boston, MA 02111-1307, USA                       ----
----                                                               ----
----                                                               ----
----                                                               ----
----   Description                                                 ----
----   -----------                                                 ----
----                                                               ----
----      This package allows to transforms the Ada command        ----
----      line arguments into C's argc and argv                    ----
----                                                               ----
----                                                               ----
----   authors : Sebastien Ponce, Fabien Azavant                   ----
----   date    : 02/28/99                                          ----
----                                                               ----
-----------------------------------------------------------------------
-----------------------------------------------------------------------


with Ada.Command_Line ;  use Ada.Command_Line ;
with Interfaces.C ; use Interfaces.C ;
with Interfaces.C.Strings ; use Interfaces.C.Strings ;
with System.Address_To_Access_Conversions ;


package body Corba.Command_Line is

   -- Argv
   -------
   function Argv return System.Address is
   begin
      return Pd_Argv ;
   end ;


   -- Get_Command_Line
   -------------------
   -- This function is used to build the Pd_Argv
   function Get_Command_Line return System.Address is

      type Array_Ptr is access Chars_Ptr_Array ;

      Argv_Ptr : Array_Ptr := new Chars_Ptr_Array(0..Size_T(Argument_Count)) ;

      package A2a  is new System.Address_To_Access_Conversions(Chars_Ptr_Array) ;

   begin
      Argv_Ptr.all(0) := New_String("name_of_the_program") ;
      for I in 1..Argument_Count loop
         declare
            S : Size_T := Size_T(I) ;
         begin
            Argv_Ptr.all(S) := New_String(Argument(I)) ;
         end ;
      end loop ;
      return A2a.To_address(A2a.Object_Pointer(Argv_Ptr)) ;
   end ;


begin

   Pd_Argv := Get_Command_Line ;

end Corba.Command_Line ;
