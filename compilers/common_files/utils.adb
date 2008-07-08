------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                                U T I L S                                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2004-2007, Free Software Foundation, Inc.          --
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

with Charset;   use Charset;
with Namet;     use Namet;

package body Utils is

   Up_To_Low : constant := Character'Pos ('A') - Character'Pos ('a');

   ----------------
   -- Capitalize --
   ----------------

   procedure Capitalize (S : in out String) is
      Up : Boolean := True;
   begin
      for I in S'Range loop
         if Up then
            Up := False;
            if S (I) in 'a' .. 'z' then
               S (I) := Character'Val (Character'Pos (S (I)) + Up_To_Low);
            end if;
         end if;
         if S (I) = '_' then
            Up := True;
         end if;
      end loop;
   end Capitalize;

   ------------
   -- Quoted --
   ------------

   function Quoted (S : String; D : Character := '"') return String is -- "
   begin
      return (1 => D) & S & (1 => D);
   end Quoted;

   ------------
   -- Quoted --
   ------------

   function Quoted (S : String; D : Character := '"') return Name_Id is -- "
   begin
      Set_Char_To_Name_Buffer (D);
      Add_Str_To_Name_Buffer (S);
      Add_Char_To_Name_Buffer (D);
      return Name_Find;
   end Quoted;

   ------------
   -- Quoted --
   ------------

   function Quoted (N : Name_Id; D : Character := '"') return String is -- "
   begin
      return Quoted (Get_Name_String (N), D);
   end Quoted;

   ------------
   -- Quoted --
   ------------

   function Quoted (N : Name_Id; D : Character := '"') return Name_Id is -- "
   begin
      return Quoted (Get_Name_String (N), D);
   end Quoted;

   --------------
   -- To_Lower --
   --------------

   function To_Lower (N : Name_Id) return Name_Id is
   begin
      if N = No_Name then
         return No_Name;
      end if;
      Get_Name_String (N);
      To_Lower (Name_Buffer (1 .. Name_Len));
      return Name_Find;
   end To_Lower;

end Utils;
