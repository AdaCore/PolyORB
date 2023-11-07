------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--           T E S T 0 0 1 _ R E Q U E S T _ I N F O _ T E S T S            --
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

with CORBA.Object;
with CORBA.Repository_Root;

with Dynamic;
with IOP;
with Messaging;

with Test001_Interface.Helper;

package body Test001_Request_Info_Tests is

   use CORBA;
   use CORBA.Repository_Root;
   use CORBA.TypeCode;
   use Dynamic;
   use IOP;
   use Messaging;
   use PortableInterceptor;
   use PortableInterceptor.RequestInfo;
   use Test001_Globals;
   use Test001_Interface.Helper;

   --------------------
   -- Test_Arguments --
   --------------------

   procedure Test_Arguments
     (Point : Interception_Point;
      Info  : PortableInterceptor.RequestInfo.Local_Ref'Class)
   is
      Operation : constant String := "arguments";
      Valid     : constant Boolean
        := Point = Send_Request
             or else Point = Receive_Reply
             or else Point = Receive_Request
             or else Point = Send_Reply;
      Args      : ParameterList;
   begin
      Args := get_arguments (Info);

      if not Valid then
         Output (Point, Operation, False);
      elsif Length (Args) /= 1 then
         Output (Point, Operation, False);
      elsif Get_Type (Get_Element (Args, 1).argument) /= TC_Long then
         Output (Point, Operation, False);
      elsif From_Any (Get_Element (Args, 1).argument) /= Long'(10) then
         Output (Point, Operation, False);
      elsif Get_Element (Args, 1).mode /= PARAM_IN then
         Output (Point, Operation, False);
      else
         Output (Point, Operation, True);
      end if;

   exception
      when E : No_Resources =>
         declare
            Members : System_Exception_Members;
         begin
            Get_Members (E, Members);
            if Valid and then Members.Minor = OMGVMCID + 1 then
               Output (Point, Operation, True, " (NO_RESOURCES)");
            else
               Output (Point, Operation, False);
            end if;
         end;

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
   end Test_Arguments;

   -------------------
   -- Test_Contexts --
   -------------------

   procedure Test_Contexts
     (Point : Interception_Point;
      Info  : PortableInterceptor.RequestInfo.Local_Ref'Class)
   is
      Operation : constant String := "contexts";
      Valid     : constant Boolean
        := Point /= Send_Poll
             and then Point /= Receive_Request_Service_Contexts;
      Cont      : ContextList;
      --  pragma Unreferenced (Cont);
      pragma Warnings (Off, Cont); --  WAG:5.02 DB08-008
      --  Assigned but never read

   begin
      --  XXX Functionality test not implemented

      Cont := get_contexts (Info);
      if not Valid then
         Output (Point, Operation, False);
      else
         Output (Point, Operation, True);
      end if;

   exception
      when E : No_Resources =>
         declare
            Members : System_Exception_Members;
         begin
            Get_Members (E, Members);
            if Valid and then Members.Minor = OMGVMCID + 1 then
               Output (Point, Operation, True, " (NO_RESOURCES)");
            else
               Output (Point, Operation, False);
            end if;
         end;

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
   end Test_Contexts;

   ---------------------
   -- Test_Exceptions --
   ---------------------

   procedure Test_Exceptions
     (Point : Interception_Point;
      Info  : PortableInterceptor.RequestInfo.Local_Ref'Class)
   is
      Operation : constant String := "exceptions";
      Valid     : constant Boolean
        := Point /= Send_Poll and Point /= Receive_Request_Service_Contexts;
      Excs      : ExceptionList;

   begin
      Excs := get_exceptions (Info);

      if not Valid then
         Output (Point, Operation, False);
      elsif Length (Excs) /= 1 then
         Output (Point, Operation, False);
      elsif Get_Element (Excs, 1) /= TC_Test_Exception then
         Output (Point, Operation, False);
      else
         Output (Point, Operation, True);
      end if;

   exception
      when E : No_Resources =>
         declare
            Members : System_Exception_Members;
         begin
            Get_Members (E, Members);
            if Valid and then Members.Minor = OMGVMCID + 1 then
               Output (Point, Operation, True, " (NO_RESOURCES)");
            else
               Output (Point, Operation, False);
            end if;
         end;

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
   end Test_Exceptions;

   ----------------------------
   -- Test_Forward_Reference --
   ----------------------------

   procedure Test_Forward_Reference
     (Point : Interception_Point;
      Info  : PortableInterceptor.RequestInfo.Local_Ref'Class)
   is
      Operation : constant String := "forward_reference";
      Valid     : constant Boolean
        := (Point = Receive_Other or else Point = Send_Other)
             and then get_reply_status (Info) = LOCATION_FORWARD;
      Obj       : CORBA.Object.Ref;

   begin
      Obj := get_forward_reference (Info);

      if Valid
        and then CORBA.Object.Is_Equivalent (Obj, Test_Forward_Object)
      then
         Output (Point, Operation, True);
      else
         Output (Point, Operation, False);
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
   end Test_Forward_Reference;

   ------------------------------------
   -- Test_Get_Reply_Service_Context --
   ------------------------------------

   procedure Test_Get_Reply_Service_Context
     (Point : Interception_Point;
      Info  : PortableInterceptor.RequestInfo.Local_Ref'Class)
   is
      Operation : constant String := "get_reply_service_context";
      Valid     : constant Boolean
        := Point /= Send_Request
             and then Point /= Send_Poll
             and then Point /= Receive_Request_Service_Contexts
             and then Point /= Receive_Request;
      Context   : IOP.ServiceContext;

   begin
      begin
         Context := get_reply_service_context (Info, 123456);
         Output (Point, Operation, False);
         return;

      exception
         when E : Bad_Inv_Order =>
            declare
               Members : System_Exception_Members;
            begin
               Get_Members (E, Members);
               if Valid or else Members.Minor /= OMGVMCID + 14 then
                  Output (Point, Operation, False);
                  return;
               end if;
            end;

         when E : Bad_Param =>
            declare
               Members : System_Exception_Members;
            begin
               Get_Members (E, Members);
               if not Valid or else Members.Minor /= OMGVMCID + 26 then
                  Output (Point, Operation, False);
                  return;
               end if;
            end;

         when others =>
            Output (Point, Operation, False);
            return;
      end;

      if Point = Receive_Reply
        or else Point = Receive_Exception
        or else Point = Receive_Other
      then
         begin
            Context :=
              get_reply_service_context (Info, Test_Reply_Context.context_id);

            if Context /= Test_Reply_Context then
               Output (Point, Operation, False);
               return;
            end if;

         exception
            when others =>
               Output (Point, Operation, False);
               return;
         end;
      end if;

      Output (Point, Operation, True);
   end Test_Get_Reply_Service_Context;

   --------------------------------------
   -- Test_Get_Request_Service_Context --
   --------------------------------------

   procedure Test_Get_Request_Service_Context
     (Point : Interception_Point;
      Info  : PortableInterceptor.RequestInfo.Local_Ref'Class)
   is
      Operation : constant String  := "get_request_service_context";
      Valid     : constant Boolean := Point /= Send_Poll;
      Context   : IOP.ServiceContext;

   begin
      begin
         Context := get_request_service_context (Info, 123456);
         Output (Point, Operation, False);
         return;

      exception
         when E : Bad_Inv_Order =>
            declare
               Members : System_Exception_Members;
            begin
               Get_Members (E, Members);
               if Valid or else Members.Minor /= OMGVMCID + 14 then
                  Output (Point, Operation, False);
                  return;
               end if;
            end;

         when E : Bad_Param =>
            declare
               Members : System_Exception_Members;
            begin
               Get_Members (E, Members);
               if not Valid or else Members.Minor /= OMGVMCID + 26 then
                  Output (Point, Operation, False);
                  return;
               end if;
            end;

         when others =>
            Output (Point, Operation, False);
            return;
      end;

      if Point = Receive_Request_Service_Contexts
        or else Point = Receive_Request
      then
         begin
            Context :=
              get_request_service_context
                (Info, Test_Request_Context.context_id);

            if Context /= Test_Request_Context then
               Output (Point, Operation, False);
               return;
            end if;

         exception
            when others =>
               Output (Point, Operation, False);
               return;
         end;
      end if;

      Output (Point, Operation, True);
   end Test_Get_Request_Service_Context;

   -------------------
   -- Test_Get_Slot --
   -------------------

   procedure Test_Get_Slot
     (Point : Interception_Point;
      Info  : PortableInterceptor.RequestInfo.Local_Ref'Class)
   is
      Operation : constant String := "get_slot";
      Val       : Any;
      --  pragma Unreferenced (Val);
      pragma Warnings (Off, Val); --  WAG:5.02 DB08-008
      --  Assigned but never read

   begin
      Val := get_slot (Info, 100);
      --  Operation must raise InvalidSlot exception because slot is not
      --  allocated. The slot allocation, Get_Slot/Set_Slot Requests and
      --  PICurrent operations tested in test002.
      Output (Point, Operation, False);

   exception
      when InvalidSlot =>
         Output (Point, Operation, True);

      when others =>
         Output (Point, Operation, False);
   end Test_Get_Slot;

   --------------------
   -- Test_Operation --
   --------------------

   procedure Test_Operation
     (Point : Interception_Point;
      Info  : PortableInterceptor.RequestInfo.Local_Ref'Class)
   is
      Operation : constant String := "operation";

   begin
      if get_operation (Info) = "Func" then
         Output (Point, Operation, True);
      else
         Output (Point, Operation, False);
      end if;

   exception
      when others =>
         Output (Point, Operation, False);
   end Test_Operation;

   ----------------------------
   -- Test_Operation_Context --
   ----------------------------

   procedure Test_Operation_Context
     (Point : Interception_Point;
      Info  : PortableInterceptor.RequestInfo.Local_Ref'Class)
   is
      Operation : constant String := "operation_context";
      Valid     : constant Boolean
        := Point /= Send_Poll
             and then (Point = Receive_Request or Point = Send_Reply);
      Cont      : RequestContext;
      --  pragma Unreferenced (Cont);
      pragma Warnings (Off, Cont); --  WAG:5.02 DB08-008
      --  Assigned but never read

   begin
      --  XXX Functionality test not implemented

      Cont := get_operation_context (Info);

      if not Valid then
         Output (Point, Operation, False);
      else
         Output (Point, Operation, True);
      end if;

   exception
      when E : No_Resources =>
         declare
            Members : System_Exception_Members;

         begin
            Get_Members (E, Members);
            if Valid and then Members.Minor = OMGVMCID + 1 then
               Output (Point, Operation, True, " (NO_RESOURCES)");
            else
               Output (Point, Operation, False);
            end if;
         end;

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
   end Test_Operation_Context;

   ---------------------
   -- Test_Request_Id --
   ---------------------

   procedure Test_Request_Id
     (Point    : Interception_Point;
      Info     : PortableInterceptor.RequestInfo.Local_Ref'Class;
      Suppress : Boolean := False)
   is
      Operation : constant String := "request_id";
      Aux       : CORBA.Unsigned_Long;

   begin
      Aux := get_request_id (Info);

      if Point in Client_Interception_Point then
         if Point = Send_Request then
            Test_Client_Request_Id := Aux;

            if not Suppress then
               Output (Point, Operation, True);
            end if;

         else
            Output (Point, Operation, Test_Client_Request_Id = Aux);
         end if;

      else  --  Point in Server_Interception_Point
         if Point = Receive_Request_Service_Contexts then
            Test_Server_Request_Id := Aux;

            if not Suppress then
               Output (Point, Operation, True);
            end if;

         else
            Output (Point, Operation, Test_Server_Request_Id = Aux);
         end if;
      end if;

   exception
      when others =>
         Output (Point, Operation, False);
   end Test_Request_Id;

   -----------------------
   -- Test_Reply_Status --
   -----------------------

   procedure Test_Reply_Status
     (Point : Interception_Point;
      Info  : PortableInterceptor.RequestInfo.Local_Ref'Class)
   is
      Operation : constant String := "reply_status";
      Valid     : constant Boolean
        := Point /= Send_Request and then Point /= Send_Poll
             and then Point /= Receive_Request_Service_Contexts
             and then Point /= Receive_Request;
      Stat      : ReplyStatus;

   begin
      --  XXX This is only return value validity test

      Stat := get_reply_status (Info);

      if not Valid then
         Output (Point, Operation, False);

      elsif Point = Receive_Reply and then Stat /= SUCCESSFUL then
         Output (Point, Operation, False);

      elsif Point = Receive_Exception
        and then (Stat /= PortableInterceptor.SYSTEM_EXCEPTION
                    and then Stat /= PortableInterceptor.USER_EXCEPTION)
      then
         Output (Point, Operation, False);

      elsif Point = Receive_Other
        and then (Stat /= SUCCESSFUL
                    and then Stat /= LOCATION_FORWARD
                    and then Stat /= TRANSPORT_RETRY
                    and then Stat /= PortableInterceptor.UNKNOWN)
      then
         Output (Point, Operation, False);

      elsif Point = Send_Reply and then Stat /= SUCCESSFUL then
         Output (Point, Operation, False);

      elsif Point = Send_Exception
        and then (Stat /= PortableInterceptor.SYSTEM_EXCEPTION
                    and then Stat /= PortableInterceptor.USER_EXCEPTION)
      then
         Output (Point, Operation, False);

      elsif Point = Send_Other
        and then (Stat /= SUCCESSFUL
                    and then Stat /= LOCATION_FORWARD
                    and then Stat /= PortableInterceptor.UNKNOWN)
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
   end Test_Reply_Status;

   -----------------
   -- Test_Result --
   -----------------

   procedure Test_Result
     (Point : Interception_Point;
      Info  : PortableInterceptor.RequestInfo.Local_Ref'Class)
   is
      Operation : constant String := "result";
      Valid     : constant Boolean
        := Point = Receive_Reply or Point = Send_Reply;
      Res       : Any;

   begin
      Res := get_result (Info);

      if not Valid then
         Output (Point, Operation, False);
      elsif Get_Type (Res) /= TC_Long then
         Output (Point, Operation, False);
      elsif From_Any (Res) /= Long'(12) then
         Output (Point, Operation, False);
      else
         Output (Point, Operation, True);
      end if;

   exception
      when E : No_Resources =>
         declare
            Members : System_Exception_Members;

         begin
            Get_Members (E, Members);
            if Valid and then Members.Minor = OMGVMCID + 1 then
               Output (Point, Operation, True, " (NO_RESOURCES)");
            else
               Output (Point, Operation, False);
            end if;
         end;

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
   end Test_Result;

   ----------------------------
   -- Test_Response_Expected --
   ----------------------------

   procedure Test_Response_Expected
     (Point : Interception_Point;
      Info  : PortableInterceptor.RequestInfo.Local_Ref'Class)
   is
      Operation : constant String := "response_expected";

   begin
      --  The operation is not oneway: a response is always expected

      if get_response_expected (Info) then
         Output (Point, Operation, True);
      else
         Output (Point, Operation, False);
      end if;

   exception
      when others =>
         Output (Point, Operation, False);
   end Test_Response_Expected;

   ---------------------
   -- Test_Sync_Scope --
   ---------------------

   procedure Test_Sync_Scope
     (Point : Interception_Point;
      Info  : PortableInterceptor.RequestInfo.Local_Ref'Class)
   is
      Operation : constant String := "sync_scope";
      Valid     : constant Boolean := Point /= Send_Poll;
      Aux       : SyncScope;

   begin
      Aux := get_sync_scope (Info);

      --  We test only non oneway operation, thus Sync_Scope always
      --  will be Sync_With_Target
      if Valid
        and then Aux = SYNC_WITH_TARGET
      then
         Output (Point, Operation, True);
      else
         Output (Point, Operation, False);
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
   end Test_Sync_Scope;

end Test001_Request_Info_Tests;
