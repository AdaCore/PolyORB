------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                            P O _ C A T R E F                             --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2004 Free Software Foundation, Inc.             --
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

--  $Id$

--  Utility tool to display information held in a stringified
--  reference, e.g.  CORBA IOR, corbaloc or URI.

with Ada.Command_Line;
with Ada.Text_IO;

with Output;

with PolyORB.References;

with PolyORB.Setup.Client;
pragma Warnings (Off, PolyORB.Setup.Client);
pragma Elaborate_All (Polyorb.Setup.Client);

with PolyORB.Initialization;

with PolyORB.Binding_Data.Print;

with PolyORB.Utils;

with PO_CatRef_Setup;
pragma Warnings (Off, PO_CatRef_Setup);
pragma Elaborate_All (PO_CatRef_Setup);

procedure PO_CatRef is

   use Ada.Command_Line;

   use Output;

   use PolyORB.References;
   use PolyORB.Binding_Data;

   use PolyORB.Binding_Data.Print;
   use PolyORB.Utils;

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

   String_To_Object (Ada.Command_Line.Argument (1), Obj_Ref);

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
         Put_Line ("Profile number", Trimmed_Image (J));
         Print_Profile (Profiles (J));
         New_Line;
      end loop;
   end;

   Flush;
end PO_CatRef;
