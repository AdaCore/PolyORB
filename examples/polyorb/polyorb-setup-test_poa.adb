------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--               P O L Y O R B . S E T U P . T E S T _ P O A                --
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

--  A variant of the test setup that uses a POA instead of an SOA.

with Ada.Exceptions;
with Ada.Text_IO;

with PolyORB.Test_Object_POA;

with PolyORB.Errors;
with PolyORB.Obj_Adapters;
with PolyORB.Objects;
with PolyORB.Servants;
with PolyORB.ORB;
with PolyORB.POA;
with PolyORB.POA.Basic_POA;
with PolyORB.POA_Config;
with PolyORB.POA_Config.Root_POA;
with PolyORB.POA_Manager;
with PolyORB.POA_Types;
with PolyORB.References;
with PolyORB.References.IOR;
with PolyORB.Types;

with PolyORB.Setup.Test_SOA;

package body PolyORB.Setup.Test_POA is

   use Ada.Text_IO;

   use PolyORB.Errors;
   use PolyORB.ORB;
   use PolyORB.POA.Basic_POA;

   use PolyORB.Test_Object_POA;
   use PolyORB.Setup.Test_SOA;

   My_Servant : PolyORB.Servants.Servant_Access;
   Obj_Adapter : PolyORB.POA_Types.Obj_Adapter_Access;

   ----------------------------
   -- Initialize_Test_Object --
   ----------------------------

   procedure Initialize_Test_Object
   is
      My_Id  : Objects.Object_Id_Access;
      URI    : PolyORB.Types.String;
      Error  : Error_Container;
   begin
      Put_Line ("Initializing OA configuration... ");
      PolyORB.POA_Config.Set_Configuration
        (new PolyORB.POA_Config.Root_POA.Root_POA_Configuration);

      Put_Line ("Creating object adapter... ");
      Obj_Adapter := new POA.Basic_POA.Basic_Obj_Adapter;
      POA.Basic_POA.Create (Basic_Obj_Adapter (Obj_Adapter.all)'Access);
      --  Create object adapter

      Set_Object_Adapter
        (The_ORB, PolyORB.Obj_Adapters.Obj_Adapter_Access (Obj_Adapter));
      --  Link object adapter with ORB.

      My_Servant := new PolyORB.Test_Object_POA.My_Object;
      --  Create application server object.

      PolyORB.POA_Manager.Activate
        (PolyORB.POA_Manager.POAManager_Access
         (PolyORB.POA_Manager.Entity_Of
          (POA.Obj_Adapter (Obj_Adapter.all).POA_Manager)),
         Error);

      PolyORB.Obj_Adapters.Export
        (Obj_Adapters.Obj_Adapter_Access (Obj_Adapter),
         My_Servant,
         null,
         My_Id,
         Error);
      --  Register it with the POA.

      if Found (Error) then
         raise Program_Error;
      end if;

      Put_Line ("Registered object: " & PolyORB.Objects.Image (My_Id.all));

      Create_Reference
        (The_ORB, My_Id, "IDL:Echo:1.0", My_Ref);
      --  Obtain object reference.

      Put_Line ("Reference is     : " & References.Image (My_Ref));
      PolyORB.POA_Types.Oid_To_Rel_URI (Obj_Adapter, My_Id, URI, Error);
      if Found (Error) then
         raise Program_Error;
      end if;
      Put_Line ("URI is           : "
                & PolyORB.Types.To_Standard_String (URI));
      begin
         Put_Line ("IOR is           : "
                   & PolyORB.References.IOR.Object_To_String (My_Ref));
      exception
         when E : others =>
            Put_Line ("Warning: Object_To_String raised:");
            Put_Line (Ada.Exceptions.Exception_Information (E));
      end;

      Put_Line (" done.");
   end Initialize_Test_Object;

end PolyORB.Setup.Test_POA;
