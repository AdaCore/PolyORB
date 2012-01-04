------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--      T E S T 0 0 1 _ C L I E N T _ I N T E R C E P T O R . I M P L       --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2004-2012, Free Software Foundation, Inc.          --
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

with CORBA;
with PortableInterceptor.ClientRequestInterceptor;
with PortableInterceptor.Interceptor;

with Test001_Client_Request_Info_Tests;
with Test001_Globals;
with Test001_Request_Info_Tests;

package body Test001_Client_Interceptor.Impl is

   use Test001_Client_Request_Info_Tests;
   use Test001_Globals;
   use Test001_Request_Info_Tests;

   procedure Test_Interception_Point
     (Point  : Client_Interception_Point;
      Info   : PortableInterceptor.ClientRequestInfo.Local_Ref);

   ----------
   -- Is_A --
   ----------

   function Is_A
     (Self            : not null access Object;
      Logical_Type_Id : Standard.String) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return
        CORBA.Is_Equivalent
          (Logical_Type_Id, Test001_Client_Interceptor.Repository_Id)
          or else
        CORBA.Is_Equivalent
          (Logical_Type_Id,
           PortableInterceptor.ClientRequestInterceptor.Repository_Id)
          or else
        CORBA.Is_Equivalent
          (Logical_Type_Id, PortableInterceptor.Interceptor.Repository_Id)
          or else
        CORBA.Is_Equivalent
          (Logical_Type_Id, "IDL:omg.org/CORBA/Object:1.0");
   end Is_A;

   -----------------------
   -- Receive_Exception --
   -----------------------

   procedure Receive_Exception
     (Self : access Object;
      RI   : PortableInterceptor.ClientRequestInfo.Local_Ref)
   is
      pragma Unreferenced (Self);

   begin
      Test_Interception_Point (Receive_Exception, RI);
   end Receive_Exception;

   -------------------
   -- Receive_Other --
   -------------------

   procedure Receive_Other
     (Self : access Object;
      RI   : PortableInterceptor.ClientRequestInfo.Local_Ref)
   is
      pragma Unreferenced (Self);

   begin
      Test_Interception_Point (Receive_Other, RI);
   end Receive_Other;

   -------------------
   -- Receive_Reply --
   -------------------

   procedure Receive_Reply
     (Self : access Object;
      RI   : PortableInterceptor.ClientRequestInfo.Local_Ref)
   is
      pragma Unreferenced (Self);

   begin
      Test_Interception_Point (Receive_Reply, RI);
   end Receive_Reply;

   ---------------
   -- Send_Poll --
   ---------------

   procedure Send_Poll
     (Self : access Object;
      RI   : PortableInterceptor.ClientRequestInfo.Local_Ref)
   is
      pragma Unreferenced (Self);

   begin
      Test_Interception_Point (Send_Poll, RI);
   end Send_Poll;

   ------------------
   -- Send_Request --
   ------------------

   procedure Send_Request
     (Self : access Object;
      RI   : PortableInterceptor.ClientRequestInfo.Local_Ref)
   is
      pragma Unreferenced (Self);

   begin
      Test_Interception_Point (Send_Request, RI);
   end Send_Request;

   -----------------------------
   -- Test_Interception_Point --
   -----------------------------

   procedure Test_Interception_Point
     (Point  : Client_Interception_Point;
      Info   : PortableInterceptor.ClientRequestInfo.Local_Ref)
   is
   begin
      if not Test001_Globals.Enable_Test_Point (Point) then

         --  Test_Request_Id must be always called at Send_Request point,
         --  because it store actual request_id.

         if Point = Send_Request then
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
         Test_Target (Point, Info);
      exception
         when others =>
            Output (Point, "target", False);
      end;

      begin
         Test_Effective_Target (Point, Info);
      exception
         when others =>
            Output (Point, "effective_target", False);
      end;

      begin
         Test_Effective_Profile (Point, Info);
      exception
         when others =>
            Output (Point, "effective_profile", False);
      end;

      begin
         Test_Received_Exception (Point, Info);
      exception
         when others =>
            Output (Point, "received_exception", False);
      end;

      begin
         Test_Received_Exception_Id (Point, Info);
      exception
         when others =>
            Output (Point, "received_exception_id", False);
      end;

      begin
         Test_Get_Effective_Component (Point, Info);
      exception
         when others =>
            Output (Point, "get_effective_component", False);
      end;

      begin
         Test_Get_Effective_Components (Point, Info);
      exception
         when others =>
            Output (Point, "get_effective_components", False);
      end;

      begin
         Test_Get_Request_Policy (Point, Info);
      exception
         when others =>
            Output (Point, "get_request_policy", False);
      end;

      begin
         Test_Add_Request_Service_Context (Point, Info);
      exception
         when others =>
            Output (Point, "add_request_service_context", False);
      end;
   end Test_Interception_Point;

end Test001_Client_Interceptor.Impl;
