------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                               O U T P U T                                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision$                             --
--                                                                          --
--   Copyright (C) 1992,1993,1994,1995,1996 Free Software Foundation, Inc.  --
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

package body Output is

   Current_Column : Int := 1;
   --  Current column number

   Current_FD : File_Descriptor := Standout;
   --  File descriptor for current output

   Saved_FD : File_Descriptor;

   -------------
   --  Column --
   -------------

   function Column return Int is
   begin
      return Current_Column;
   end Column;

   -----------------------
   -- Restore_Output_FD --
   -----------------------

   procedure Restore_Output_FD is
   begin
      Current_FD := Saved_FD;
   end Restore_Output_FD;

   -------------------
   -- Set_Output_FD --
   -------------------

   procedure Set_Output_FD (FD : File_Descriptor) is
   begin
      Saved_FD   := Current_FD;
      Current_FD := FD;
   end Set_Output_FD;

   ------------------------
   -- Set_Standard_Error --
   ------------------------

   procedure Set_Standard_Error is
   begin
      Current_FD := Standerr;
   end Set_Standard_Error;

   -------------------------
   -- Set_Standard_Output --
   -------------------------

   procedure Set_Standard_Output is
   begin
      Current_FD := Standout;
   end Set_Standard_Output;

   ----------------
   -- Write_Char --
   ----------------

   procedure Write_Char (C : Character) is
   begin
      if 1 = Write (Current_FD, C'Address, 1) then
         Current_Column := Current_Column + 1;

      else
         Set_Standard_Error;
         Write_Str ("fatal error: disk full");
         OS_Exit (2);
      end if;
   end Write_Char;

   ---------------
   -- Write_Eol --
   ---------------

   procedure Write_Eol is
   begin
      Write_Char (Ascii.LF);
      Current_Column := 1;
   end Write_Eol;

   ---------------
   -- Write_Int --
   ---------------

   procedure Write_Int (Val : Int) is
   begin

      if Val < 0 then
         Write_Char ('-');
         Write_Int (-Val);

      else
         if Val > 9 then
            Write_Int (Val / 10);
         end if;

         Write_Char (Character'Val ((Val mod 10) + Character'Pos ('0')));
      end if;
   end Write_Int;

   ---------------
   -- Write_Str --
   ---------------

   procedure Write_Str (S : String) is
   begin
      if S'Length = Write (Current_FD, S'Address, S'Length) then
         Current_Column := Current_Column + S'Length;

      else
         Set_Standard_Error;
         Write_Str ("fatal error: disk full");
         OS_Exit (2);
      end if;

   end Write_Str;

   --------------------------
   -- Debugging Procedures --
   --------------------------

   procedure w (C : Character) is
   begin
      Write_Char (''');
      Write_Char (C);
      Write_Char (''');
      Write_Eol;
   end w;

   procedure w (S : String) is
   begin
      Write_Str (S);
      Write_Eol;
   end w;

   procedure w (V : Int) is
   begin
      Write_Int (V);
      Write_Eol;
   end w;

   procedure w (B : Boolean) is
   begin
      if B then
         w ("True");
      else
         w ("False");
      end if;
   end w;

   procedure w (L : String; C : Character) is
   begin
      Write_Str (L);
      Write_Char (' ');
      w (C);
   end w;

   procedure w (L : String; S : String) is
   begin
      Write_Str (L);
      Write_Char (' ');
      w (S);
   end w;

   procedure w (L : String; V : Int) is
   begin
      Write_Str (L);
      Write_Char (' ');
      w (V);
   end w;

   procedure w (L : String; B : Boolean) is
   begin
      Write_Str (L);
      Write_Char (' ');
      w (B);
   end w;

end Output;
