------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                             P O _ N A M E S                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2002-2005 Free Software Foundation, Inc.           --
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
