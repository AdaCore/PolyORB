------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--            POLYORB.GIOP_P.TAGGED_COMPONENTS.CODE_SETS.PRINT              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2004-2011, Free Software Foundation, Inc.         --
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
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Text_IO;

with Output;
with PolyORB.GIOP_P.Code_Sets;
with PolyORB.GIOP_P.Code_Sets.Description_Data;
with PolyORB.GIOP_P.Tagged_Components.Print;
with PolyORB.Initialization;
with PolyORB.Utils.Strings;

package body PolyORB.GIOP_P.Tagged_Components.Code_Sets.Print is

   use Output;
   use PolyORB.GIOP_P.Code_Sets;
   use PolyORB.GIOP_P.Code_Sets.Code_Set_Id_Lists;

   function C_Hex_Image (Value : Code_Set_Id) return String;
   --  Return 16-based C-style image of Code_Set_Id value.

   procedure Output
     (List : Code_Set_Id_List;
      Data : Character);

   procedure Output (Item : Tagged_Component'Class);

   procedure Output_TC (TC : TC_Code_Sets);

   function Description (Code_Set : Code_Set_Id) return String;
   --  Return code set description.

   procedure Initialize;

   -----------------
   -- C_Hex_Image --
   -----------------

   function C_Hex_Image (Value : Code_Set_Id) return String is
      package Code_Set_Id_IO is new Ada.Text_IO.Modular_IO (Code_Set_Id);
      use Code_Set_Id_IO;

      Buf : String (1 .. 13);
      Aux : Character;
   begin
      Put (Buf, Value, 16);

      for J in Buf'Range loop
         Aux := Buf (J);
         Buf (J) := '0';
         exit when Aux = '#';
      end loop;

      return "0x" & Buf (5 .. 12);
   end C_Hex_Image;

   -----------------
   -- Description --
   -----------------

   function Description (Code_Set : Code_Set_Id) return String is
      package PGSD renames PolyORB.GIOP_P.Code_Sets.Description_Data;
   begin
      for J in PGSD.Info'Range loop
         if PGSD.Info (J).Code_Set = Code_Set then
            return
              PGSD.Description (PGSD.Info (J).First .. PGSD.Info (J).Last);
         end if;
      end loop;

      return "Unknown code set";
   end Description;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      PolyORB.GIOP_P.Tagged_Components.Print.Register
       (Tag_Code_Sets, Output'Access);
   end Initialize;

   ------------
   -- Output --
   ------------

   procedure Output (Item : Tagged_Component'Class) is
   begin
      Output_TC (TC_Code_Sets (Item));
   end Output;

   ------------
   -- Output --
   ------------

   procedure Output
     (List : Code_Set_Id_List;
      Data : Character)
   is
      Iter : Code_Set_Id_Lists.Iterator;
   begin
      Inc_Indent;
      Iter := First (List);
      while not Last (Iter) loop
         declare
            Code_Set : constant Code_Set_Id := Value (Iter).all;
         begin
            Put_Line
              ("SCCS-" & Data,
               C_Hex_Image (Code_Set) & "; " & Description (Code_Set));
         end;

         Next (Iter);
      end loop;
      Dec_Indent;
   end Output;

   ---------------
   -- Output_TC --
   ---------------

   procedure Output_TC (TC : TC_Code_Sets) is
   begin
      Put_Line ("Type", "TAG_Code_Sets");

      Inc_Indent;

      Put_Line
       ("SNCS-C",
        C_Hex_Image (TC.For_Char_Data.Native_Code_Set)
        & "; " & Description (TC.For_Char_Data.Native_Code_Set));
      Output (TC.For_Char_Data.Conversion_Code_Sets, 'C');

      Put_Line
       ("SNCS-W",
        C_Hex_Image (TC.For_Wchar_Data.Native_Code_Set)
        & "; " & Description (TC.For_Wchar_Data.Native_Code_Set));
      Output (TC.For_Wchar_Data.Conversion_Code_Sets, 'W');

      Dec_Indent;
   end Output_TC;

   use PolyORB.Initialization;
   use PolyORB.Initialization.String_Lists;
   use PolyORB.Utils.Strings;

begin
   Register_Module
     (Module_Info'
      (Name      => +"tagged_components.code_sets.print",
       Conflicts => PolyORB.Initialization.String_Lists.Empty,
       Depends   => +"tagged_components.code_sets",
       Provides  => PolyORB.Initialization.String_Lists.Empty,
       Implicit  => False,
       Init      => Initialize'Access,
       Shutdown  => null));
end PolyORB.GIOP_P.Tagged_Components.Code_Sets.Print;
