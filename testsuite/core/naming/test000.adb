------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                              T E S T 0 0 0                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2002-2012, Free Software Foundation, Inc.          --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.                                               --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
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

--   Testing naming client. Use the CORBA COS Naming API.

with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO;      use Ada.Text_IO;

with PolyORB.Setup.Client;
pragma Warnings (Off, PolyORB.Setup.Client);

with PolyORB.Initialization;
with PolyORB.References;

with PolyORB.Services.Naming.NamingContext.Client;
with PolyORB.Services.Naming.NamingContext.Helper;

procedure Test000 is

   use PolyORB.References;

   use PolyORB.Services.Naming;
   use PolyORB.Services.Naming.NamingContext;
   use PolyORB.Services.Naming.NamingContext.Client;
   use PolyORB.Services.Naming.NamingContext.Helper;

   Ref_Context : PolyORB.References.Ref;

begin

   --  Initialization

   PolyORB.Initialization.Initialize_World;

   if Argument_Count < 1 then
      Put_Line ("usage : test_naming_generic <IOR_string_from_server>");
      return;
   end if;

   String_To_Object (Ada.Command_Line.Argument (1), Ref_Context);

   --
   --  Test 1 : bind 1 object, lookup and then destroy
   --

   declare
      Obj_Name : PolyORB.Services.Naming.Name;
      Rcvd_Ref : PolyORB.References.Ref;
      --  pragma Unreferenced (Rcvd_Ref);
      pragma Warnings (Off, Rcvd_Ref); --  WAG:5.02 DB08-008
      --  Assigned but never read
      Root_Context : constant PolyORB.Services.Naming.NamingContext.Ref
        := To_Ref (Ref_Context);

   begin
      Append (Obj_Name, NameComponent'(id => To_PolyORB_String ("object1"),
                                       kind => To_PolyORB_String ("")));

      Bind (Root_Context, Obj_Name, Ref_Context);

      Rcvd_Ref := Resolve (Root_Context, Obj_Name);

      Unbind (Root_Context, Obj_Name);
      Rcvd_Ref := Resolve (Root_Context, Obj_Name);

   end;

end Test000;
