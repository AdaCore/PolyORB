------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                    T E S T _ S U I T E . O U T P U T                     --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2003-2004 Free Software Foundation, Inc.           --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
-- not, write to the Free Software Foundation, 59 Temple Place - Suite 330, --
-- Boston, MA 02111-1307, USA.                                              --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
--                PolyORB is maintained by ACT Europe.                      --
--                    (email: sales@act-europe.fr)                          --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Unchecked_Deallocation;

package body Test_Suite.Output is

   Output_Mode : TSO_Access := null;

   -----------------------
   -- Initialize_Filter --
   -----------------------

   procedure Initialize_Filter
     (Output : Test_Suite_Output'Class)
   is
      procedure Free is new
        Ada.Unchecked_Deallocation (Test_Suite_Output'Class, TSO_Access);

   begin
      if Output_Mode /= null then
         Free (Output_Mode);
      end if;

      Output_Mode := new Test_Suite_Output'Class'(Output);
   end Initialize_Filter;

   -------------------
   -- Output_Filter --
   -------------------

   procedure Output_Filter
     (Descriptor : GNAT.Expect.Process_Descriptor'Class;
      Str        : String;
      User_Data  : System.Address := System.Null_Address)
   is
      pragma Warnings (Off); -- WAG:3.14
      pragma Unreferenced (Descriptor);
      pragma Unreferenced (User_Data);
      pragma Warnings (On); -- WAG:3.14

   begin
      Test_Execution (Output_Mode.all, Str);
   end Output_Filter;

end Test_Suite.Output;
