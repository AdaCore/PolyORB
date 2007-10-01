------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--    T E S T 0 0 1 _ S E R V E R _ R E Q U E S T _ I N F O _ T E S T S     --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2004-2005 Free Software Foundation, Inc.           --
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
     (Point  : in     Server_Interception_Point;
      Info   : in     PortableInterceptor.ServerRequestInfo.Local_Ref)
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
     (Point  : in     Server_Interception_Point;
      Info   : in     PortableInterceptor.ServerRequestInfo.Local_Ref)
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
     (Point  : in     Server_Interception_Point;
      Info   : in     PortableInterceptor.ServerRequestInfo.Local_Ref)
   is
      Operation : constant String := "add_reply_service_context";

   begin
      Add_Reply_Service_Context (Info, Test_Reply_Context, True);
      Output (Point, Operation, True);

   exception
      when others =>
         Output (Point, Operation, False);
   end Test_Add_Reply_Service_Context;

   ----------------------------
   -- Test_Get_Server_Policy --
   ----------------------------

   procedure Test_Get_Server_Policy
     (Point  : in     Server_Interception_Point;
      Info   : in     PortableInterceptor.ServerRequestInfo.Local_Ref)
   is
      Operation : constant String := "get_server_policy";
      Pol       : CORBA.Policy.Ref;
      --  pragma Unreferenced (Pol);
      pragma Warnings (Off, Pol); --  WAG:5.02 DB08-008
      --  Assigned but never read

   begin
      --  XXX Functionality test not implemented

      Pol := Get_Server_Policy (Info, 1);

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
     (Point : in Server_Interception_Point;
      Info  : in PortableInterceptor.ServerRequestInfo.Local_Ref)
   is
      Operation : constant String := "object_id";
      Valid     : constant Boolean
        := Point /= Receive_Request_Service_Contexts;

   begin
      declare
         Aux : constant ObjectId := Get_Object_Id (Info);
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
     (Point : in Server_Interception_Point;
      Info  : in PortableInterceptor.ServerRequestInfo.Local_Ref)
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

      Aux := Get_ORB_Id (Info);
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
     (Point : in Server_Interception_Point;
      Info  : in PortableInterceptor.ServerRequestInfo.Local_Ref)
   is
      Operation : constant String  := "sending_exception";
      Valid     : constant Boolean := Point = Send_Exception;
      Exc       : Any;

   begin
      Exc := Get_Sending_Exception (Info);

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
     (Point : in Server_Interception_Point;
      Info  : in PortableInterceptor.ServerRequestInfo.Local_Ref)
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

      Aux := Get_Server_Id (Info);
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
     (Point : in Server_Interception_Point;
      Info  : in PortableInterceptor.ServerRequestInfo.Local_Ref)
   is
      Operation : constant String := "set_slot";

   begin
      Set_Slot (Info, 100, CORBA.Internals.Get_Empty_Any (TC_Null));
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
     (Point : in Server_Interception_Point;
      Info  : in PortableInterceptor.ServerRequestInfo.Local_Ref)
   is
      Operation : constant String  := "target_is_a";
      Valid     : constant Boolean := Point = Receive_Request;
      Aux       : Boolean;

   begin
      Aux :=
        Target_Is_A (Info, To_CORBA_String (Test001_Interface.Repository_Id));

      if not Valid then
         Output (Point, Operation, False);
      elsif not Aux then
         Output (Point, Operation, False);
      elsif
        Target_Is_A
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
     (Point : in Server_Interception_Point;
      Info  : in PortableInterceptor.ServerRequestInfo.Local_Ref)
   is
      Operation : constant String  := "target_most_derived_interface";
      Valid     : constant Boolean := Point = Receive_Request;
      Aux       : RepositoryId;

   begin
      Aux := Get_Target_Most_Derived_Interface (Info);

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
