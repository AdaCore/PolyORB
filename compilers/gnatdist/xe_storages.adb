------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                          X E _ S T O R A G E S                           --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 1995-2008, Free Software Foundation, Inc.          --
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

with XE_Utils; use XE_Utils;
with XE_Names; use XE_Names;

package body XE_Storages is

   -----------
   -- Equal --
   -----------

   function Equal (N1, N2 : Name_Id) return Boolean is
   begin
      return N1 = N2;
   end Equal;

   ----------
   -- Hash --
   ----------

   function Hash (N : Name_Id) return Hash_Header is
      Name : constant String  := Get_Name_String (N);
      H    : Natural          := 0;

   begin
      for J in Name'Range loop
         H := (H + Character'Pos (Name (J))) mod (Hash_Header'Last + 1);
      end loop;
      return H;
   end Hash;

   ----------------------
   -- Register_Storage --
   ----------------------

   procedure Register_Storage
     (Storage_Name     : String;
      Allow_Passive    : Boolean;
      Allow_Local_Term : Boolean;
      Need_Tasking     : Boolean)
   is
   begin
      Storage_Supports.Set
        (Id (Storage_Name),
         Storage_Support_Type'
           (Allow_Passive, Allow_Local_Term, Need_Tasking));
   end Register_Storage;

end XE_Storages;
