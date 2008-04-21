------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--      T E S T 0 0 1 _ S E R V E R _ I N T E R C E P T O R . I M P L       --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2004-2008, Free Software Foundation, Inc.          --
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

with CORBA.Object;
with PortableInterceptor.Helper;
with PortableInterceptor.Interceptor;

with Test001_Globals;
with Test001_Request_Info_Tests;
with Test001_Server_Request_Info_Tests;

package body Test001_Server_Interceptor.Impl is

   use Test001_Globals;
   use Test001_Request_Info_Tests;
   use Test001_Server_Request_Info_Tests;

   procedure Test_Interception_Point
     (Point  : Server_Interception_Point;
      Info   : PortableInterceptor.ServerRequestInfo.Local_Ref);

   ----------
   -- Is_A --
   ----------

   function Is_A
     (Self            : access Object;
      Logical_Type_Id : Standard.String)
      return Boolean
   is
      pragma Unreferenced (Self);

   begin
      return CORBA.Is_Equivalent
        (Logical_Type_Id,
         Test001_Server_Interceptor.Repository_Id)
        or else CORBA.Is_Equivalent
          (Logical_Type_Id,
           "IDL:omg.org/CORBA/Object:1.0")
        or else CORBA.Is_Equivalent
           (Logical_Type_Id,
         PortableInterceptor.ServerRequestInterceptor.Repository_Id)
        or else CORBA.Is_Equivalent
           (Logical_Type_Id,
         PortableInterceptor.Interceptor.Repository_Id);
   end Is_A;

   ---------------------
   -- Receive_Request --
   ---------------------

   procedure Receive_Request
     (Self : access Object;
      RI   : PortableInterceptor.ServerRequestInfo.Local_Ref)
   is
      pragma Unreferenced (Self);

   begin
      Test_Interception_Point (Receive_Request, RI);

      if Forward_Location then
         Forward_Location := False;

         PortableInterceptor.Helper.Raise_ForwardRequest
           (PortableInterceptor.ForwardRequest_Members'
            (Forward => CORBA.Object.Ref (Test_Forward_Object)));
      end if;
   end Receive_Request;

   --------------------------------------
   -- Receive_Request_Service_Contexts --
   --------------------------------------

   procedure Receive_Request_Service_Contexts
     (Self : access Object;
      RI   : PortableInterceptor.ServerRequestInfo.Local_Ref)
   is
      pragma Unreferenced (Self);

   begin
      Test_Interception_Point (Receive_Request_Service_Contexts, RI);
   end Receive_Request_Service_Contexts;

   --------------------
   -- Send_Exception --
   --------------------

   procedure Send_Exception
     (Self : access Object;
      RI   : PortableInterceptor.ServerRequestInfo.Local_Ref)
   is
      pragma Unreferenced (Self);

   begin
      Test_Interception_Point (Send_Exception, RI);
   end Send_Exception;

   ----------------
   -- Send_Other --
   ----------------

   procedure Send_Other
     (Self : access Object;
      RI   : PortableInterceptor.ServerRequestInfo.Local_Ref)
   is
      pragma Unreferenced (Self);

   begin
      Test_Interception_Point (Send_Other, RI);
   end Send_Other;

   ----------------
   -- Send_Reply --
   ----------------

   procedure Send_Reply
     (Self : access Object;
      RI   : PortableInterceptor.ServerRequestInfo.Local_Ref)
   is
      pragma Unreferenced (Self);

   begin
      Test_Interception_Point (Send_Reply, RI);
   end Send_Reply;

   -----------------------------
   -- Test_Interception_Point --
   -----------------------------

   procedure Test_Interception_Point
     (Point  : Server_Interception_Point;
      Info   : PortableInterceptor.ServerRequestInfo.Local_Ref)
   is
   begin
      if not Test001_Globals.Enable_Test_Point (Point) then

         --  Test_Request_Id must be always called at
         --  Receive_Request_Service_Contexts point,
         --  because it store actual request_id.

         if Point = Receive_Request_Service_Contexts then
            begin
               Test_Request_Id (Point, Info, True);
            exception
               when others =>
                  Output (Point, "request_id", False);
            end;
         end if;

         return;
      end if;

      begin
         Test_Request_Id (Point, Info);
      exception
         when others =>
            Output (Point, "request_id", False);
      end;

      begin
         Test_Operation (Point, Info);
      exception
         when others =>
            Output (Point, "operation", False);
      end;

      begin
         Test_Arguments (Point, Info);
      exception
         when others =>
            Output (Point, "arguments", False);
      end;

      begin
         Test_Exceptions (Point, Info);
      exception
         when others =>
            Output (Point, "exceptions", False);
      end;

      begin
         Test_Contexts (Point, Info);
      exception
         when others =>
            Output (Point, "contexts", False);
      end;

      begin
         Test_Operation_Context (Point, Info);
      exception
         when others =>
            Output (Point, "operation_context", False);
      end;

      begin
         Test_Result (Point, Info);
      exception
         when others =>
            Output (Point, "result", False);
      end;

      begin
         Test_Response_Expected (Point, Info);
      exception
         when others =>
            Output (Point, "response_expected", False);
      end;

      begin
         Test_Sync_Scope (Point, Info);
      exception
         when others =>
            Output (Point, "sync_scope", False);
      end;

      begin
         Test_Reply_Status (Point, Info);
      exception
         when others =>
            Output (Point, "reply_status", False);
      end;

      begin
         Test_Forward_Reference (Point, Info);
      exception
         when others =>
            Output (Point, "forward_reference", False);
      end;

      begin
         Test_Get_Slot (Point, Info);
      exception
         when others =>
            Output (Point, "get_slot", False);
      end;

      begin
         Test_Get_Request_Service_Context (Point, Info);
      exception
         when others =>
            Output (Point, "get_request_service_context", False);
      end;

      begin
         Test_Get_Reply_Service_Context (Point, Info);
      exception
         when others =>
            Output (Point, "get_reply_service_context", False);
      end;

      begin
         Test_Sending_Exception (Point, Info);
      exception
         when others =>
            Output (Point, "sending_exception", False);
      end;

      begin
         Test_Object_Id (Point, Info);
      exception
         when others =>
            Output (Point, "object_id", False);
      end;

      begin
         Test_Adapter_Id (Point, Info);
      exception
         when others =>
            Output (Point, "adapter_id", False);
      end;

      begin
         Test_Server_Id (Point, Info);
      exception
         when others =>
            Output (Point, "server_id", False);
      end;

      begin
         Test_ORB_Id (Point, Info);
      exception
         when others =>
            Output (Point, "ORB_id", False);
      end;

      begin
         Test_Adapter_Name (Point, Info);
      exception
         when others =>
            Output (Point, "adapter_name", False);
      end;

      begin
         Test_Target_Most_Derived_Interface (Point, Info);
      exception
         when others =>
            Output (Point, "target_most_derived_interface", False);
      end;

      begin
         Test_Get_Server_Policy (Point, Info);
      exception
         when others =>
            Output (Point, "get_server_policy", False);
      end;

      begin
         Test_Set_Slot (Point, Info);
      exception
         when others =>
            Output (Point, "set_slot", False);
      end;

      begin
         Test_Target_Is_A (Point, Info);
      exception
         when others =>
            Output (Point, "target_is_a", False);
      end;

      begin
         Test_Add_Reply_Service_Context (Point, Info);
      exception
         when others =>
            Output (Point, "add_reply_service_context", False);
      end;
   end Test_Interception_Point;

end Test001_Server_Interceptor.Impl;
