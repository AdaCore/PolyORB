------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--           T E S T 0 0 1 _ R E Q U E S T _ I N F O _ T E S T S            --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2004 Free Software Foundation, Inc.             --
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

with CORBA.Object;
with CORBA.Repository_Root;

with Dynamic;

with Test001_Globals;
with Test001_Interface.Helper;

package body Test001_Request_Info_Tests is

   use CORBA;
   use CORBA.Repository_Root;
   use CORBA.TypeCode;
   use Dynamic;
   use PortableInterceptor;
   use PortableInterceptor.RequestInfo;
   use Test001_Globals;
   use Test001_Interface.Helper;

   --------------------
   -- Test_Arguments --
   --------------------

   procedure Test_Arguments
     (Point : in Interception_Point;
      Info  : in PortableInterceptor.RequestInfo.Local_Ref'Class)
   is
      Operation : constant String := "arguments";
      Valid     : constant Boolean
        := Point = Send_Request
             or else Point = Receive_Reply
             or else Point = Receive_Request
             or else Point = Send_Reply;
      Args      : ParameterList;
   begin
      Args := Get_Arguments (Info);

      if not Valid then
         Output (Point, Operation, False);
      elsif Length (Args) /= 1 then
         Output (Point, Operation, False);
      elsif Get_Type (Get_Element (Args, 1).Argument) /= TC_Long then
         Output (Point, Operation, False);
      elsif From_Any (Get_Element (Args, 1).Argument) /= Long'(10) then
         Output (Point, Operation, False);
      elsif Get_Element (Args, 1).Mode /= PARAM_IN then
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
            if Valid and then Members.Minor = 1 then
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
            if not Valid and then Members.Minor = 14 then
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
     (Point : in Interception_Point;
      Info  : in PortableInterceptor.RequestInfo.Local_Ref'Class)
   is
      Operation : constant String := "contexts";
      Valid     : constant Boolean
        := Point /= Send_Poll
             and then Point /= Receive_Request_Service_Contexts;
      Cont      : ContextList;

   begin
      --  XXX Functionality test not implemented

      Cont := Get_Contexts (Info);
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
            if Valid and then Members.Minor = 1 then
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
            if not Valid and then Members.Minor = 14 then
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
     (Point : in Interception_Point;
      Info  : in PortableInterceptor.RequestInfo.Local_Ref'Class)
   is
      Operation : constant String := "exceptions";
      Valid     : constant Boolean
        := Point /= Send_Poll and Point /= Receive_Request_Service_Contexts;
      Excs      : ExceptionList;

   begin
      Excs := Get_Exceptions (Info);

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
            if Valid and then Members.Minor = 1 then
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
            if not Valid and then Members.Minor = 14 then
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
     (Point : in Interception_Point;
      Info  : in PortableInterceptor.RequestInfo.Local_Ref'Class)
   is
      Operation : constant String := "forward_reference";
      Valid     : constant Boolean
        := (Point = Receive_Other or else Point = Send_Other)
             and then Get_Reply_Status (Info) = Location_Forward;
      Obj       : CORBA.Object.Ref;

   begin
      Obj := Get_Forward_Reference (Info);

      --  XXX In current test implementation we can't test location forwarding,
      --  thus exception always raised.
      Output (Point, Operation, False);

   exception
      when E : Bad_Inv_Order =>
         declare
            Members : System_Exception_Members;
         begin
            Get_Members (E, Members);
            if not Valid and then Members.Minor = 14 then
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
     (Point : in Interception_Point;
      Info  : in PortableInterceptor.RequestInfo.Local_Ref'Class)
   is
      pragma Unreferenced (Info);

      Operation : constant String := "get_reply_service_context";

   begin
      --  XXX Not yet implemented in RequestInfo

      Output (Point, Operation, False, " (NO TEST)");
   end Test_Get_Reply_Service_Context;

   --------------------------------------
   -- Test_Get_Request_Service_Context --
   --------------------------------------

   procedure Test_Get_Request_Service_Context
     (Point : in Interception_Point;
      Info  : in PortableInterceptor.RequestInfo.Local_Ref'Class)
   is
      pragma Unreferenced (Info);

      Operation : constant String := "get_request_service_context";

   begin
      --  XXX Not yet implemented in RequestInfo

      Output (Point, Operation, False, " (NO TEST)");
   end Test_Get_Request_Service_Context;

   -------------------
   -- Test_Get_Slot --
   -------------------

   procedure Test_Get_Slot
     (Point : in Interception_Point;
      Info  : in PortableInterceptor.RequestInfo.Local_Ref'Class)
   is
      Operation : constant String := "get_slot";
      Val       : Any;

   begin
      Val := Get_Slot (Info, 100);
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
     (Point : in Interception_Point;
      Info  : in PortableInterceptor.RequestInfo.Local_Ref'Class)
   is
      Operation : constant String := "operation";

   begin
      if Get_Operation (Info) = "Func" then
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
     (Point : in Interception_Point;
      Info  : in PortableInterceptor.RequestInfo.Local_Ref'Class)
   is
      Operation : constant String := "operation_context";
      Valid     : constant Boolean
        := Point /= Send_Poll
             and then (Point = Receive_Request or Point = Send_Reply);
      Cont      : RequestContext;

   begin
      --  XXX Functionality test not implemented

      Cont := Get_Operation_Context (Info);

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
            if Valid and then Members.Minor = 1 then
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
            if not Valid and then Members.Minor = 14 then
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
     (Point : in Interception_Point;
      Info  : in PortableInterceptor.RequestInfo.Local_Ref'Class)
   is
      Operation : constant String := "request_id";
      Aux : CORBA.Unsigned_Long;
      pragma Warnings (Off, Aux);

   begin
      --  XXX Functionality test not implemented

      Aux := Get_Request_Id (Info);
      Output (Point, Operation, True);
   exception
      when others =>
         Output (Point, Operation, False);
   end Test_Request_Id;

   -----------------------
   -- Test_Reply_Status --
   -----------------------

   procedure Test_Reply_Status
     (Point : in Interception_Point;
      Info  : in PortableInterceptor.RequestInfo.Local_Ref'Class)
   is
      Operation : constant String := "reply_status";
      Valid     : constant Boolean
        := Point /= Send_Request and then Point /= Send_Poll
             and then Point /= Receive_Request_Service_Contexts
             and then Point /= Receive_Request;
      Stat      : ReplyStatus;

   begin
      --  XXX This is only return value validity test

      Stat := Get_Reply_Status (Info);

      if not Valid then
         Output (Point, Operation, False);

      elsif Point = Receive_Reply and then Stat /= Successful then
         Output (Point, Operation, False);

      elsif Point = Receive_Exception
        and then (Stat /= PortableInterceptor.System_Exception
                    or else Stat /= PortableInterceptor.User_Exception)
      then
         Output (Point, Operation, False);

      elsif Point = Receive_Other
        and then (Stat /= Successful
                    or else Stat /= Location_Forward
                    or else Stat /= Transport_Retry
                    or else Stat /= PortableInterceptor.Unknown)
      then
         Output (Point, Operation, False);

      elsif Point = Send_Reply and then Stat /= Successful then
         Output (Point, Operation, False);

      elsif Point = Send_Exception
        and then (Stat /= PortableInterceptor.System_Exception
                    or else Stat /= PortableInterceptor.User_Exception)
      then
         Output (Point, Operation, False);

      elsif Point = Send_Other
        and then (Stat /= Successful
                    or else Stat /= Location_Forward
                    or else Stat /= PortableInterceptor.Unknown)
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
            if not Valid and then Members.Minor = 14 then
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
     (Point : in Interception_Point;
      Info  : in PortableInterceptor.RequestInfo.Local_Ref'Class)
   is
      Operation : constant String := "result";
      Valid     : constant Boolean
        := Point = Receive_Reply or Point = Send_Reply;
      Res       : Any;

   begin
      Res := Get_Result (Info);

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
            if Valid and then Members.Minor = 1 then
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
            if not Valid and then Members.Minor = 14 then
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
     (Point : in Interception_Point;
      Info  : in PortableInterceptor.RequestInfo.Local_Ref'Class)
   is
      Operation : constant String := "response_expected";

   begin
      --  XXX Functionality test not implemented

      if Get_Response_Expected (Info) then
         null;
      end if;

      Output (Point, Operation, True);
   exception
      when others =>
         Output (Point, Operation, False);
   end Test_Response_Expected;

   ---------------------
   -- Test_Sync_Scope --
   ---------------------

   procedure Test_Sync_Scope
     (Point : in Interception_Point;
      Info  : in PortableInterceptor.RequestInfo.Local_Ref'Class)
   is
      pragma Unreferenced (Info);

      Operation : constant String := "sync_scope";

   begin
      --  XXX Not yet implemented in RequestInfo

      Output (Point, Operation, False, " (NO TEST)");
   end Test_Sync_Scope;

end Test001_Request_Info_Tests;
