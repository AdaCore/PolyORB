------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--                   C O R B A . C O M M A N D _ L I N E                    --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.7 $
--                                                                          --
--         Copyright (C) 1999-2000 ENST Paris University, France.           --
--                                                                          --
-- AdaBroker is free software; you  can  redistribute  it and/or modify it  --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. AdaBroker  is distributed  in the hope that it will be  useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with AdaBroker; see file COPYING. If  --
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
--             AdaBroker is maintained by ENST Paris University.            --
--                     (email: broker@inf.enst.fr)                          --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Command_Line;     use Ada.Command_Line;
with Interfaces.C;         use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with System.Address_To_Access_Conversions;

package body CORBA.Command_Line is

   ----------
   -- Argv --
   ----------

   function Argv return System.Address is
   begin
      return Pd_Argv;
   end Argv;

   ----------------------
   -- Get_Command_Line --
   ----------------------

   function Get_Command_Line
     return System.Address
   is

      type Array_Ptr is access chars_ptr_array;

      Argv_Ptr : Array_Ptr
        := new chars_ptr_array (0 .. size_t (Argument_Count));

      package A2A  is
        new System.Address_To_Access_Conversions (chars_ptr_array);

   begin
      Argv_Ptr.all (0) := New_String ("name_of_the_program");
      for I in 1 .. Argument_Count loop
         declare
            S : size_t := size_t (I);
         begin
            Argv_Ptr.all (S) := New_String (Argument (I));
         end;
      end loop;
      return A2A.To_Address (A2A.Object_Pointer (Argv_Ptr));
   end Get_Command_Line;

begin
   Pd_Argv := Get_Command_Line;
end CORBA.Command_Line;
