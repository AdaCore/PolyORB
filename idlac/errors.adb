------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--                        I D L _ F E . E R R O R S                         --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1999-2000 ENST Paris University, France.          --
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
--             AdaBroker is maintained by ENST Paris University.            --
--                     (email: broker@inf.enst.fr)                          --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Text_IO; use Ada.Text_IO;
with GNAT.OS_Lib;

package body Errors is

   -------------------------
   --  Location handling  --
   -------------------------

   --  nice display of a natural
   function Nat_To_String (Val : Natural) return String;

   --  display an error
   procedure Display_Error (Message : in String;
                            Level : in Error_Kind;
                            Loc : Location);

   ---------------------
   --  Nat_To_String  --
   ---------------------

   function Nat_To_String (Val : Natural) return String is
      Res : String := Natural'Image (Val);
   begin
      return Res (2 .. Res'Last);
   end Nat_To_String;

   ------------------------
   --  Display_Location  --
   ------------------------

   function Display_Location (Loc : in Location) return String is
   begin
      if Loc.Filename /= null then
         if Loc.Dirname /= null then
            return "line " &
              Nat_To_String (Loc.Line) &
              ", column " &
              Nat_To_String (Loc.Col) &
              " of file " &
              Loc.Dirname.all &
              GNAT.OS_Lib.Directory_Separator &
              Loc.Filename.all;
         else
            return "line " &
              Nat_To_String (Loc.Line) &
              ", column " &
              Nat_To_String (Loc.Col) &
              " of file " &
              Loc.Filename.all;
         end if;
      else
         return "line " &
           Nat_To_String (Loc.Line) &
           ", column " &
           Nat_To_String (Loc.Col);
      end if;
   end Display_Location;



   ----------------------
   --  Error handling  --
   ----------------------

   --  counters for errors and warnings
   Error_Count : Natural := 0;
   Warning_Count : Natural := 0;

   ---------------------
   --  Display_Error  --
   ---------------------
   procedure Display_Error (Message : in String;
                            Level : in Error_Kind;
                            Loc : Location) is
   begin
      case Level is
         when Fatal =>
            Put (Current_Error, "FATAL ERROR occured");
         when Error =>
            Put (Current_Error, "ERROR occured");
         when Warning =>
            Put (Current_Error, "WARNING occured");
      end case;
      if Loc.Line > 0 then
         Put (Current_Error, " at " & Display_Location (Loc));
      end if;
      New_Line (Current_Error);
      Put (Current_Error, "    ");
      Put_Line (Current_Error, Message);
      New_Line (Current_Error);
   end Display_Error;

   -----------
   -- Error --
   -----------

   procedure Error
     (Message : in String;
      Level : in Error_Kind;
      Loc : in Location) is
   begin
      case Level is
         when Fatal =>
            null;
         when Error =>
            Error_Count := Error_Count + 1;
         when Warning =>
            Warning_Count := Warning_Count + 1;
      end case;
      Display_Error (Message, Level, Loc);
      if Level = Fatal then
         raise Fatal_Error;
      end if;
   end Error;

   ----------------
   --  Is_Error  --
   ----------------

   function Is_Error return Boolean is
   begin
      return Error_Count > 0;
   end Is_Error;

   ------------------
   --  Is_Warning  --
   ------------------

   function Is_Warning return Boolean is
   begin
      return Warning_Count > 0;
   end Is_Warning;

   --------------------
   --  Error_Number  --
   --------------------

   function Error_Number return Natural is
   begin
      return Error_Count;
   end Error_Number;

   ----------------------
   --  Warning_Number  --
   ----------------------

   function Warning_Number return Natural is
   begin
      return Warning_Count;
   end Warning_Number;

end Errors;
