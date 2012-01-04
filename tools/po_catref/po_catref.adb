------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                            P O _ C A T R E F                             --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2004-2012, Free Software Foundation, Inc.          --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.                                               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

--  Utility tool to display information held in a stringified
--  reference, e.g.  CORBA IOR, corbaloc or URI.

with Ada.Command_Line;
with Ada.Text_IO;

with Output;

with PolyORB.References;

with PolyORB.Setup.Client;
pragma Warnings (Off, PolyORB.Setup.Client);

with PolyORB.Initialization;

with PolyORB.Binding_Data.Print;

with PolyORB.Types; use PolyORB.Types;

with PO_CatRef_Setup;
pragma Warnings (Off, PO_CatRef_Setup);

procedure PO_CatRef is

   use Ada.Command_Line;

   use Output;

   use PolyORB.References;
   use PolyORB.Binding_Data;

   use PolyORB.Binding_Data.Print;

   Obj_Ref : Ref;

begin
   PolyORB.Initialization.Initialize_World;

   if Argument_Count /= 1 then
      Ada.Text_IO.Put_Line ("usage: po_catref <string_ref>");
      Ada.Text_IO.Put_Line (" <string_ref> is a stringified reference:"
                            & " CORBA IOR, corbaloc or URI");

      return;
   end if;

   Put_Line ("Parsing stringified reference", Ada.Command_Line.Argument (1));
   New_Line;

   begin
      String_To_Object (Ada.Command_Line.Argument (1), Obj_Ref);
   exception
      when others =>
         Put_Line ("Error", "Invalid reference !");
         Flush;
         return;
   end;

   if Is_Nil (Obj_Ref) then
      Put_Line ("Error", "Null reference !");

      Flush;
      return;
   end if;

   declare
      Profiles : constant Profile_Array := Profiles_Of (Obj_Ref);

   begin
      Put_Line ("Type Id", Type_Id_Of (Obj_Ref));
      New_Line;

      Put_Line ("Found", Profiles'Length'Img & " profiles in IOR");
      New_Line;

      for J in Profiles'Range loop
         Put_Line ("Profile number", Trimmed_Image (Long_Long (J)));
         Print_Profile (Profiles (J));
         New_Line;
      end loop;
   end;

   Flush;
end PO_CatRef;
