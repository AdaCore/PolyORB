------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                              T E S T 0 0 0                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2002-2003 Free Software Foundation, Inc.           --
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

--   Testing naming client. Use the CORBA COS Naming API.

--  $Id$

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
      Root_Context : PolyORB.Services.Naming.NamingContext.Ref
        := To_Ref (Ref_Context);

   begin
      Append (Obj_Name, NameComponent'(Id => To_PolyORB_String ("object1"),
                                       Kind => To_PolyORB_String ("")));

      Bind (Root_Context, Obj_Name, Ref_Context);

      Rcvd_Ref := Resolve (Root_Context, Obj_Name);

      Unbind (Root_Context, Obj_Name);
      Rcvd_Ref := Resolve (Root_Context, Obj_Name);

   end;

end Test000;
