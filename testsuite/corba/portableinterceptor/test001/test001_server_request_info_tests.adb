------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--    T E S T 0 0 1 _ S E R V E R _ R E Q U E S T _ I N F O _ T E S T S     --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2004-2023, Free Software Foundation, Inc.          --
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

with CORBA.Policy;

with Test001_Interface.Helper;
with Test001_Server_Interceptor;

package body Test001_Server_Request_Info_Tests is

   use CORBA;
   use CORBA.TypeCode;
   use PortableInterceptor;
   use PortableInterceptor.ServerRequestInfo;
   use Test001_Globals;
   use Test001_Interface.Helper;

   ---------------------
   -- Test_Adapter_Id --
   ---------------------

   procedure Test_Adapter_Id
     (Point  : Server_Interception_Point;
      Info   : PortableInterceptor.ServerRequestInfo.Local_Ref)
   is
      pragma Unreferenced (Info);

      Operation : constant String := "adapter_id";

   begin
      --  XXX Not yet implemented in ServerRequestInfo

      Output (Point, Operation, Pass_Not_Implemented, " (NO TEST)");
   end Test_Adapter_Id;

   -----------------------
   -- Test_Adapter_Name --
   -----------------------

   procedure Test_Adapter_Name
     (Point  : Server_Interception_Point;
      Info   : PortableInterceptor.ServerRequestInfo.Local_Ref)
   is
      pragma Unreferenced (Info);

      Operation : constant String := "adapter_name";

   begin
      --  XXX Not yet implemented in ServerRequestInfo

      Output (Point, Operation, Pass_Not_Implemented, " (NO TEST)");
   end Test_Adapter_Name;

   ------------------------------------
   -- Test_Add_Reply_Service_Context --
   ------------------------------------

   procedure Test_Add_Reply_Service_Context
     (Point  : Server_Interception_Point;
      Info   : PortableInterceptor.ServerRequestInfo.Local_Ref)
   is
      Operation : constant String := "add_reply_service_context";

   begin
      add_reply_service_context (Info, Test_Reply_Context, True);
      Output (Point, Operation, True);

   exception
      when others =>
         Output (Point, Operation, False);
   end Test_Add_Reply_Service_Context;

   ----------------------------
   -- Test_Get_Server_Policy --
   ----------------------------

   procedure Test_Get_Server_Policy
     (Point  : Server_Interception_Point;
      Info   : PortableInterceptor.ServerRequestInfo.Local_Ref)
   is
      Operation : constant String := "get_server_policy";
      Pol       : CORBA.Policy.Ref;
      --  pragma Unreferenced (Pol);
      pragma Warnings (Off, Pol); --  WAG:5.02 DB08-008
      --  Assigned but never read

   begin
      --  XXX Functionality test not implemented

      Pol := get_server_policy (Info, 1);

      Output (Point, Operation, True);

   exception
      when E : Inv_Policy =>
         declare
            Members : System_Exception_Members;
         begin
            Get_Members (E, Members);
            if Members.Minor = OMGVMCID + 3 then
               Output (Point, Operation, True, " (INV_POLICY)");
            else
               Output (Point, Operation, False);
            end if;
         end;

      when others =>
         Output (Point, Operation, False);

   end Test_Get_Server_Policy;

   --------------------
   -- Test_Object_Id --
   --------------------

   procedure Test_Object_Id
     (Point : Server_Interception_Point;
      Info  : PortableInterceptor.ServerRequestInfo.Local_Ref)
   is
      Operation : constant String := "object_id";
      Valid     : constant Boolean
        := Point /= Receive_Request_Service_Contexts;

   begin
      declare
         Aux : constant ObjectId := get_object_id (Info);
      begin
         if not Valid then
            Output (Point, Operation, False);
         elsif Aux /= Test_ObjectId then
            Output (Point, Operation, False);
         else
            Output (Point, Operation, True);
         end if;
      end;

   exception
      when E : Bad_Inv_Order =>
         declare
            Members : System_Exception_Members;
         begin
            Get_Members (E, Members);
            if not Valid and then Members.Minor = OMGVMCID + 14 then
               Output (Point, Operation, True);
            else
               Output (Point, Operation, False);
            end if;
         end;

      when E : No_Resources =>
         declare
            Members : System_Exception_Members;
         begin
            Get_Members (E, Members);
            if Valid
              and then (Point = Send_Exception
                          or else Point = Send_Other)
              and then Members.Minor = OMGVMCID + 1
            then
               Output (Point, Operation, True, " (NO_RESOURCES)");
            else
               Output (Point, Operation, False);
            end if;
         end;

      when others =>
         Output (Point, Operation, False);
   end Test_Object_Id;

   -----------------
   -- Test_ORB_Id --
   -----------------

   procedure Test_ORB_Id
     (Point : Server_Interception_Point;
      Info  : PortableInterceptor.ServerRequestInfo.Local_Ref)
   is
      Operation : constant String := "orb_id";
      Valid     : constant Boolean
        := Point /= Receive_Request_Service_Contexts;
      Aux       : ORBId;
      --  pragma Unreferenced (Aux);
      pragma Warnings (Off, Aux); --  WAG:5.02 DB08-008
      --  Assigned but never read

   begin
      --  XXX Functionality test not implemented

      Aux := get_orb_id (Info);
      Output (Point, Operation, False);

   exception
      when E : Bad_Inv_Order =>
         declare
            Members : System_Exception_Members;
         begin
            Get_Members (E, Members);
            if not Valid and then Members.Minor = OMGVMCID + 14 then
               Output (Point, Operation, True);
            else
               Output (Point, Operation, False);
            end if;
         end;

      when others =>
         Output (Point, Operation, False);
   end Test_ORB_Id;

   ----------------------------
   -- Test_Sending_Exception --
   ----------------------------

   procedure Test_Sending_Exception
     (Point : Server_Interception_Point;
      Info  : PortableInterceptor.ServerRequestInfo.Local_Ref)
   is
      Operation : constant String  := "sending_exception";
      Valid     : constant Boolean := Point = Send_Exception;
      Exc       : Any;

   begin
      Exc := get_sending_exception (Info);

      if not Valid then
         Output (Point, Operation, False);
      elsif Get_Type (Exc) /= TC_Test_Exception then
         Output (Point, Operation, False);
      else
         Output (Point, Operation, True);
      end if;

   exception
      when E : Bad_Inv_Order =>
         declare
            Members : System_Exception_Members;

         begin
            Get_Members (E, Members);
            if not Valid and then Members.Minor = OMGVMCID + 14 then
               Output (Point, Operation, True);
            else
               Output (Point, Operation, False);
            end if;
         end;

      when others =>
         Output (Point, Operation, False);
   end Test_Sending_Exception;

   --------------------
   -- Test_Server_Id --
   --------------------

   procedure Test_Server_Id
     (Point : Server_Interception_Point;
      Info  : PortableInterceptor.ServerRequestInfo.Local_Ref)
   is
      Operation : constant String := "server_id";
      Valid     : constant Boolean
        := Point /= Receive_Request_Service_Contexts;
      Aux       : ServerId;
      --  pragma Unreferenced (Aux);
      pragma Warnings (Off, Aux); --  WAG:5.02 DB08-008
      --  Assigned but never read

   begin
      --  XXX Functionality test not implemented

      Aux := get_server_id (Info);
      Output (Point, Operation, False);

   exception
      when E : Bad_Inv_Order =>
         declare
            Members : System_Exception_Members;
         begin
            Get_Members (E, Members);
            if not Valid and then Members.Minor = OMGVMCID + 14 then
               Output (Point, Operation, True);
            else
               Output (Point, Operation, False);
            end if;
         end;

      when others =>
         Output (Point, Operation, False);
   end Test_Server_Id;

   -------------------
   -- Test_Set_Slot --
   -------------------

   procedure Test_Set_Slot
     (Point : Server_Interception_Point;
      Info  : PortableInterceptor.ServerRequestInfo.Local_Ref)
   is
      Operation : constant String := "set_slot";

   begin
      set_slot (Info, 100, CORBA.Internals.Get_Empty_Any (TC_Null));
      --  Operation must raise InvalidSlot exception because slot is not
      --  allocated. The slot allocation, Get_Slot/Set_Slot Requests and
      --  PICurrent operations tested in test002.

      Output (Point, Operation, False);
   exception
      when InvalidSlot =>
         Output (Point, Operation, True);

      when others =>
         Output (Point, Operation, False);
   end Test_Set_Slot;

   ----------------------
   -- Test_Target_Is_A --
   ----------------------

   procedure Test_Target_Is_A
     (Point : Server_Interception_Point;
      Info  : PortableInterceptor.ServerRequestInfo.Local_Ref)
   is
      Operation : constant String  := "target_is_a";
      Valid     : constant Boolean := Point = Receive_Request;
      Aux       : Boolean;

   begin
      Aux :=
        target_is_a (Info, To_CORBA_String (Test001_Interface.Repository_Id));

      if not Valid then
         Output (Point, Operation, False);
      elsif not Aux then
         Output (Point, Operation, False);
      elsif
        target_is_a
        (Info, To_CORBA_String (Test001_Server_Interceptor.Repository_Id))
      then
         Output (Point, Operation, False);
      else
         Output (Point, Operation, True);
      end if;

   exception
      when E : Bad_Inv_Order =>
         declare
            Members : System_Exception_Members;
         begin
            Get_Members (E, Members);
            if not Valid and then Members.Minor = OMGVMCID + 14 then
               Output (Point, Operation, True);
            else
               Output (Point, Operation, False);
            end if;
         end;

      when others =>
         Output (Point, Operation, False);
   end Test_Target_Is_A;

   ----------------------------------------
   -- Test_Target_Most_Derived_Interface --
   ----------------------------------------

   procedure Test_Target_Most_Derived_Interface
     (Point : Server_Interception_Point;
      Info  : PortableInterceptor.ServerRequestInfo.Local_Ref)
   is
      Operation : constant String  := "target_most_derived_interface";
      Valid     : constant Boolean := Point = Receive_Request;
      Aux       : RepositoryId;

   begin
      Aux := get_target_most_derived_interface (Info);

      if not Valid then
         Output (Point, Operation, False);
      elsif Aux /= Test001_Interface.Repository_Id then
         Output (Point, Operation, False);
      else
         Output (Point, Operation, True);
      end if;

   exception
      when E : Bad_Inv_Order =>
         declare
            Members : System_Exception_Members;

         begin
            Get_Members (E, Members);
            if not Valid and then Members.Minor = OMGVMCID + 14 then
               Output (Point, Operation, True);
            else
               Output (Point, Operation, False);
            end if;
         end;

      when others =>
         Output (Point, Operation, False);
   end Test_Target_Most_Derived_Interface;

end Test001_Server_Request_Info_Tests;
