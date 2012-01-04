------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                         P O _ C R E A T E R E F                          --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2007-2012, Free Software Foundation, Inc.          --
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

with Ada.Text_IO;

with PolyORB.References;
with PolyORB.References.IOR;

with PolyORB.Setup.Client;
pragma Elaborate_All (PolyORB.Setup.Client);
pragma Warnings (Off, PolyORB.Setup.Client);

with PolyORB.Initialization;
with PolyORB.Binding_Data.Create;

with PO_CreateRef_Setup;
pragma Elaborate_All (PO_CreateRef_Setup);
pragma Warnings (Off, PO_CreateRef_Setup);

with PO_CreateRef_Parse_Cmd;

procedure PO_CreateRef is
   use PO_CreateRef_Parse_Cmd;
   Params    : Parameter_Ref;

begin
   PolyORB.Initialization.Initialize_World;

   Parse_Command_Line (Params);

   declare
      use PolyORB.Binding_Data.Create;

      Reference : PolyORB.References.Ref;
      Error     : Boolean;
      Profiles  : PolyORB.References.Profile_Array
        (1 .. Params.Profiles.all'Length);
   begin

      for J in Profiles'Range loop
         Create_Profile (Params.Profiles (J), Profiles (J), Error);
         if Error then
            Ada.Text_IO.Put_Line ("Syntax error");
            return;
         end if;
      end loop;

      --  Build reference

      PolyORB.References.Create_Reference
        (Profiles,
         Params.Ref_Type.all,
         Reference);

      Free (Params);

      declare
         --  use PolyORB.Objects;

         Str : constant String :=
           PolyORB.References.IOR.Object_To_String (Reference);
      begin
         Ada.Text_IO.Put_Line (Str);
      end;

   end;
end PO_CreateRef;
