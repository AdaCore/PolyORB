------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUNTIME COMPONENTS                          --
--                                                                          --
--                     A D A . C O M M A N D _ L I N E                      --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision$                              --
--                                                                          --
--        Copyright (C) 1992,1993,1994 Free Software Foundation, Inc.       --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- It is now maintained by Ada Core Technologies Inc (http://www.gnat.com). --
--                                                                          --
------------------------------------------------------------------------------

with System;
package body Ada.Command_Line is

   function Arg_Count return Natural;
   pragma Import (C, Arg_Count, "arg_count");

   procedure Fill_Arg (A : System.Address; Arg_Num : Integer);
   pragma Interface (C, Fill_Arg);

   function Len_Arg (Arg_Num : Integer) return Integer;
   pragma Interface (C, Len_Arg);

   --------------------
   -- Argument_Count --
   --------------------

   function Argument_Count return Natural is
   begin
      return Arg_Count - 1;
   end Argument_Count;

   --------------
   -- Argument --
   --------------

   function Argument (Number : in Positive) return String is
   begin
      if Number > Argument_Count then
         raise Constraint_Error;
      end if;

      declare
         Arg : aliased String (1 .. Len_Arg (Number));
      begin
         Fill_Arg (Arg'Address, Number);
         return Arg;
      end;
   end Argument;

   ------------------
   -- Command_Name --
   ------------------

   function Command_Name return String is
      Arg : aliased String (1 .. Len_Arg (0));
   begin
      Fill_Arg (Arg'Address, 0);
      return Arg;
   end Command_Name;

end Ada.Command_Line;
