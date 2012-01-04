------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                        S E R V E R _ C O M M O N                         --
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

with Ada.Exceptions;
with Ada.Text_IO;

with PolyORB.Errors;
with PolyORB.Obj_Adapters;
with PolyORB.Objects;
with PolyORB.Servants;
with PolyORB.ORB;
with PolyORB.POA;
with PolyORB.POA_Manager;
with PolyORB.POA_Types;
with PolyORB.References;
with PolyORB.References.IOR;
with PolyORB.Types;
with PolyORB.Setup;

with Ping_Object;

package body Server_Common is

   use Ada.Text_IO;

   use PolyORB.Errors;
   use PolyORB.Objects;
   use PolyORB.ORB;
   use PolyORB.POA;
   use PolyORB.Setup;
   use PolyORB.References;

   use Ping_Object;

   My_Servant : PolyORB.Servants.Servant_Access;
   My_Ref : PolyORB.References.Ref;

   ----------------------------
   -- Initialize_Test_Object --
   ----------------------------

   procedure Initialize_Test_Object
   is
      My_Id  : PolyORB.Objects.Object_Id_Access;
      Error  : Error_Container;

      Obj_Adapter : constant PolyORB.Obj_Adapters.Obj_Adapter_Access
        := PolyORB.ORB.Object_Adapter (PolyORB.Setup.The_ORB);
      URI : PolyORB.Types.String;

   begin
      PolyORB.POA_Manager.Activate
        (PolyORB.POA_Manager.POAManager_Access
         (PolyORB.POA_Manager.Entity_Of
          (PolyORB.POA.Obj_Adapter (Obj_Adapter.all).POA_Manager)),
         Error);

      My_Servant := new Ping_Object.My_Object;
      --  Create application server object

      PolyORB.Obj_Adapters.Export
        (Obj_Adapter,
         My_Servant,
         null,
         My_Id,
         Error);
      --  Register it with the POA

      if Found (Error) then
         raise Program_Error;
      end if;

      Put_Line ("Registered object: " & PolyORB.Objects.Image (My_Id.all));

      Create_Reference
        (The_ORB, My_Id, "IDL:Ping:1.0", My_Ref);
      --  Obtain object reference

      PolyORB.POA_Types.Oid_To_Rel_URI
        (PolyORB.POA_Types.Obj_Adapter (Obj_Adapter.all)'Access,
         My_Id, URI, Error);
      Put_Line ("Reference is     : " & PolyORB.References.Image (My_Ref));
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

end Server_Common;
