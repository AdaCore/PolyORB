------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--             P O L Y O R B . S E T U P . T E S T _ C O R B A              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                Copyright (C) 2001 Free Software Fundation                --
--                                                                          --
-- AdaBroker is free software; you  can  redistribute  it and/or modify it  --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. AdaBroker  is distributed  in the hope that it will be  useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with AdaBroker; see file COPYING. If  --
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
--              PolyORB is maintained by ENST Paris University.             --
--                                                                          --
------------------------------------------------------------------------------

--  A variant of the test setup that uses a POA instead of an SOA.

--  $Id$

with Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with CORBA;
with CORBA.Test_Object; use CORBA.Test_Object;

with CORBA.ServerRequest;
pragma Warnings (Off, CORBA.ServerRequest);
--  XXX Not used for now.

with PortableServer.POA;
pragma Warnings (Off, PortableServer.POA);
--  XXX Not used for now.

with PolyORB.POA_Types;
with PolyORB.Obj_Adapters;
with PolyORB.Objects;
with PolyORB.ORB; use PolyORB.ORB;
with PolyORB.POA;
with PolyORB.POA.Basic_POA; use PolyORB.POA.Basic_POA;
with PolyORB.References;
with PolyORB.References.IOR;
with PolyORB.Setup.Test; use PolyORB.Setup.Test;
with PolyORB.Types;
with PolyORB.POA_Config;
with PolyORB.POA_Config.Minimum;
with PolyORB.POA_Manager;

package body PolyORB.Setup.Test_CORBA is

   My_Servant : PolyORB.POA_Types.Servant_Access;
   Obj_Adapter : PolyORB.POA_Types.Obj_Adapter_Access;

   procedure Initialize_CORBA_Test_Object is
   begin
      Put ("Initializing OA confiuration... ");
      PolyORB.POA_Config.Set_Configuration
        (new PolyORB.POA_Config.Minimum.Minimum_Configuration);
      Put ("Creating object adapter... ");
      Obj_Adapter := new POA.Basic_POA.Basic_Obj_Adapter;
      POA.Basic_POA.Create (Basic_Obj_Adapter (Obj_Adapter.all)'Access);
      --  Create object adapter

      Set_Object_Adapter
        (The_ORB, PolyORB.Obj_Adapters.Obj_Adapter_Access (Obj_Adapter));
      --  Link object adapter with ORB.

      My_Servant := new CORBA.Test_Object.My_Object;
      --  Create application server object.
      CORBA.Test_Object.Create (My_Object (My_Servant.all));

      PolyORB.POA_Manager.Activate
        (PolyORB.POA_Manager.POAManager_Access
         (PolyORB.POA_Manager.Entity_Of
          (POA.Obj_Adapter (Obj_Adapter.all).POA_Manager)));

      declare
         My_Id : aliased Objects.Object_Id
           := PolyORB.Obj_Adapters.Export
           (PolyORB.Obj_Adapters.Obj_Adapter_Access (Obj_Adapter),
            PolyORB.Objects.Servant_Access (My_Servant));
         --  Register it with the SOA.

      begin
         Put_Line ("Registered object: " & PolyORB.Objects.Image (My_Id));
         Create_Reference
           (The_ORB, My_Id'Access, "IDL:Echo:1.0", My_Ref);
         --  Obtain object reference.

         Put_Line ("Reference is     : " & References.Image (My_Ref));
         Put_Line ("URI is           : "
                   & PolyORB.Types.To_Standard_String
                   (PolyORB.POA_Types.Oid_To_Rel_URI
                    (Obj_Adapter, My_Id)));
         begin
            Put_Line ("IOR is           : "
                      & CORBA.To_Standard_String
                      (References.IOR.Object_To_String
                       ((Ref => My_Ref))));
         exception
            when E : others =>
               Put_Line ("Warning: Object_To_String raised:");
               Put_Line (Ada.Exceptions.Exception_Information (E));
         end;

      end;
      Put_Line (" done.");
   end Initialize_CORBA_Test_Object;

end PolyORB.Setup.Test_CORBA;
