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

with PolyORB.Setup.No_Tasking_Server;
pragma Warnings (Off, PolyORB.Setup.No_Tasking_Server);
pragma Elaborate_All (Polyorb.Setup.No_Tasking_Server);

with PolyORB.Initialization;

with PolyORB.Binding_Data.DIOP.Print;
with PolyORB.Binding_Data.IIOP.Print;
with PolyORB.Binding_Data.UIPMC.Print;
with PolyORB.Binding_Data.SOAP.Print;
with PolyORB.Binding_Data.SRP.Print;

with PolyORB.Types;
with PolyORB.Utils;

procedure PO_CatRef is

   use Ada.Command_Line;

   use Output;

   use PolyORB.References;
   use PolyORB.Binding_Data;

   use PolyORB.Binding_Data.DIOP.Print;
   use PolyORB.Binding_Data.IIOP.Print;
   use PolyORB.Binding_Data.UIPMC.Print;
   use PolyORB.Binding_Data.SOAP.Print;
   use PolyORB.Binding_Data.SRP.Print;

   use PolyORB.Utils;

   use type PolyORB.Types.Unsigned_Long;

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

   declare
      Profiles : constant Profile_Array := Profiles_Of (Obj_Ref);
      Tag : Profile_Tag;

   begin
      Put_Line ("Type Id", Type_Id_Of (Obj_Ref));
      New_Line;

      Put_Line ("Found", Profiles'Length'Img & " profiles in IOR");
      New_Line;

      for J in Profiles'Range loop
         Tag := Get_Profile_Tag (Profiles (J).all);

         if Tag = Tag_Internet_IOP then
            Put_Line ("Profile number #" & Trimmed_Image (J), " (IIOP)");
            Print_IIOP_Profile (Profiles (J));
            New_Line;

         elsif Tag = Tag_SOAP then
            Put_Line ("Profile number #" & Trimmed_Image (J), " (SOAP)");
            Print_SOAP_Profile (Profiles (J));
            New_Line;

         elsif Tag = Tag_SRP then
            Put_Line ("Profile number #" & Trimmed_Image (J), " (SRP)");
            Print_SRP_Profile (Profiles (J));
            New_Line;

         elsif Tag = Tag_DIOP then
            Put_Line ("Profile number #" & Trimmed_Image (J), " (DIOP)");
            Print_DIOP_Profile (Profiles (J));
            New_Line;

         elsif Tag = Tag_UIPMC then
            Put_Line ("Profile number #" & Trimmed_Image (J), " (UIPMC)");
            Print_UIPMC_Profile (Profiles (J));
            New_Line;

         else
            Put_Line ("Profile number #" & Trimmed_Image (J),
                      " (Tag UNKOWN" & Tag'Img & ")");
            New_Line;
         end if;
      end loop;
   end;

   Flush;
end PO_CatRef;
