------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                              B A C K E N D                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2005-2010, Free Software Foundation, Inc.          --
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

with GNAT.OS_Lib;  use GNAT.OS_Lib;

with Errors;  use Errors;
with Output;  use Output;

package body Backend is

   type Backend_Record is record
      Language  : String_Access;
      Comments  : String_Access;
      Generate  : Generate_Procedure;
      Usage     : Usage_Procedure;
   end record;

   type Table_Index is range 1 .. 8;
   subtype Table_Count is Table_Index'Base range 0 .. Table_Index'Last;
   Table : array (Table_Index) of Backend_Record;
   First : constant Table_Index := Table'First;
   Last  : Table_Count := 0;

   Current : Table_Count := 0;
   --  Initialized to proper value in Backend.Config

   ----------------------
   -- Current_Language --
   ----------------------

   function Current_Language return String is
      pragma Assert (Current in Table_Index);
   begin
      return Table (Current).Language.all;
   end Current_Language;

   --------------
   -- Generate --
   --------------

   procedure Generate (IDL_Spec : Node_Id) is
   begin
      if Current /= 0 then
         Table (Current).Generate (IDL_Spec);
      end if;
   end Generate;

   -----------------------
   -- Is_Valid_Language --
   -----------------------

   function Is_Valid_Language (L : String) return Boolean is
   begin
      for N in First .. Last loop
         if Table (N).Language.all = L then
            return True;
         end if;
      end loop;
      return False;
   end Is_Valid_Language;

   --------------
   -- Register --
   --------------

   procedure Register_Language
     (Generate  : Generate_Procedure;
      Usage     : Usage_Procedure;
      Language  : String;
      Comments  : String)
   is
   begin
      if Last = Table'Last then
         DE ("too many target languages");
      end if;
      for I in First .. Last loop
         if Table (I).Language.all = Language then
            DE ("already declared target language");
            raise Internal_Error;
         end if;
      end loop;
      Last := Last + 1;
      Table (Last).Generate  := Generate;
      Table (Last).Usage     := Usage;
      Table (Last).Language  := new String'(Language);
      Table (Last).Comments  := new String'(Comments);
   end Register_Language;

   --------------------------
   -- Set_Current_Language --
   --------------------------

   procedure Set_Current_Language (Language : String) is
   begin
      Current := 0;
      for I in First .. Last loop
         if Table (I).Language.all = Language then
            Current := I;
            exit;
         end if;
      end loop;
      if Current = 0 then
         DE ("unknown target language");
         raise Fatal_Error;
      end if;
   end Set_Current_Language;

   ---------------------
   -- Write_Languages --
   ---------------------

   procedure Write_Languages
     (L, C : Natural)
   is
      S : String (1 .. 64);
   begin
      for I in reverse First .. Last loop
         S := (others => ' ');
         declare
            Language : constant String := Table (I).Language.all;
            Comments : constant String := Table (I).Comments.all;
            Usage    : constant Usage_Procedure := Table (I).Usage;
         begin
            S (L .. L + Language'Length - 1) := Language;
            S (C .. C + Comments'Length - 1) := Comments;
            Write_Line (S (1 .. C + Comments'Length - 1));
            Usage (C);
            Write_Eol;
         end;
      end loop;
   end Write_Languages;

end Backend;
