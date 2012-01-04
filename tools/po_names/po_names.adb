------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                             P O _ N A M E S                              --
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
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

--  Naming server.

--  Provides an interface similar to CORBA COS Naming without dependencies
--  on the CORBA application personality.

with Ada.Text_IO;

with PolyORB.Errors;
with PolyORB.Initialization;
with PolyORB.Minimal_Servant.Tools;
with PolyORB.References;
with PolyORB.References.IOR;
with PolyORB.Types;

with PolyORB.Setup.No_Tasking_Server;
pragma Warnings (Off, PolyORB.Setup.No_Tasking_Server);

with PolyORB.Services.Naming.NamingContext.Servant;

procedure PO_Names is

   use PolyORB.Errors;
   use PolyORB.Minimal_Servant.Tools;
   use PolyORB.Types;

   package NC renames PolyORB.Services.Naming.NamingContext.Servant;

   NC_Ref  : PolyORB.References.Ref;
   Root_NC : NC.Object_Ptr;

   Error : Error_Container;

begin
   PolyORB.Initialization.Initialize_World;

   --  Initialize the Root Naming Context

   Root_NC := NC.Create;
   Initiate_Servant (Root_NC,
                     To_PolyORB_String ("NAMING"),
                     NC_Ref,
                     Error);

   if Found (Error) then
      raise Program_Error;
   end if;

   --  Output its reference

   Ada.Text_IO.Put_Line
     ("POLYORB_CORBA_NAME_SERVICE=" &
      PolyORB.References.IOR.Object_To_String (NC_Ref));

   --  Run node as a stand alone server

   Run_Server;
end PO_Names;
