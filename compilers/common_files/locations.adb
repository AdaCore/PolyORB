------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                            L O C A T I O N S                             --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2004-2008, Free Software Foundation, Inc.          --
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

with Namet; use Namet;
with Utils; use Utils;

package body Locations is

   ---------
   -- "<" --
   ---------

   function "<" (Op1, Op2 : Location) return Boolean is
   begin
      return Op1.File = Op2.File
        and then Op1.Dir = Op2.Dir
        and then Op1.Scan < Op2.Scan;
   end "<";

   -----------
   -- Image --
   -----------

   function Image (Loc : Location) return String
   is
   begin
      if Loc.File = No_Name then
         return No_Str;
      end if;
      declare
         Column : constant Nat := Nat (Loc.Last - Loc.First + 1);

         function Column_Image return String;
         --  Return the image of Column, with a leading blank if necessary to
         --  make it at least 2 characters.

         function Column_Image return String is
            Im : constant String := Image (Column);
         begin
            if Column < 10 then
               return "0" & Im;
            else
               return Im;
            end if;
         end Column_Image;

      begin
         return Get_Name_String (Loc.File) & ":" & Image (Loc.Line)
           & ":" & Column_Image;
      end;
   end Image;

   ----------------------
   -- Set_New_Location --
   ----------------------

   procedure Set_New_Location
     (Loc  : in out Location;
      Name : Name_Id;
      Line : Int)
   is
      Len : Integer;
   begin
      Loc.Line := Line;
      Get_Name_String (Name);
      Len := Name_Len;
      for I in reverse 1 .. Name_Len loop
         if Is_Dir_Separator (Name_Buffer (I)) then
            Name_Len := I - 1;
            Loc.Dir  := Name_Find;
            Set_Str_To_Name_Buffer (Name_Buffer (I + 1 .. Len));
            Loc.File := Name_Find;
            return;
         end if;
      end loop;
      Loc.Dir  := No_Name;
      Loc.File := Name;
   end Set_New_Location;

end Locations;
