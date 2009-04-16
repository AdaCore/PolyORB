------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                         P O _ C R E A T E R E F                          --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2007-2008, Free Software Foundation, Inc.          --
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
