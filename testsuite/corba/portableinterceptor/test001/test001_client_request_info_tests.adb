------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--    T E S T 0 0 1 _ C L I E N T _ R E Q U E S T _ I N F O _ T E S T S     --
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
with CORBA.Policy;

with Test001_Globals;
with Test001_Interface.Helper;

package body Test001_Client_Request_Info_Tests is

   use CORBA;
   use CORBA.Object;
   use CORBA.TypeCode;
   use PortableInterceptor.ClientRequestInfo;
   use Test001_Globals;
   use Test001_Interface;
   use Test001_Interface.Helper;

   --------------------------------------
   -- Test_Add_Request_Service_Context --
   --------------------------------------

   procedure Test_Add_Request_Service_Context
     (Point : in     Client_Interception_Point;
      Info  : in     PortableInterceptor.ClientRequestInfo.Local_Ref)
   is
      Operation : constant String  := "add_request_service_context";
      Valid     : constant Boolean := Point = Send_Request;

   begin
      Add_Request_Service_Context (Info, Test_Request_Context, False);
      if Valid then
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
            if not Valid and then Members.Minor = 14 then
               Output (Point, Operation, True);
            else
               Output (Point, Operation, False);
            end if;
         end;

      when others =>
         Output (Point, Operation, False);
   end Test_Add_Request_Service_Context;

   ----------------------------
   -- Test_Effective_Profile --
   ----------------------------

   procedure Test_Effective_Profile
     (Point : in     Client_Interception_Point;
      Info  : in     PortableInterceptor.ClientRequestInfo.Local_Ref)
   is
      pragma Unreferenced (Info);

      Operation : constant String := "effective_profile";

   begin
      --  XXX Not yet implemented in ClientRequestInfo

      Output (Point, Operation, False, " (NO TEST)");
   end Test_Effective_Profile;

   ---------------------------
   -- Test_Effective_Target --
   ---------------------------

   procedure Test_Effective_Target
     (Point : in     Client_Interception_Point;
      Info  : in     PortableInterceptor.ClientRequestInfo.Local_Ref)
   is
      Operation : constant String := "effective_target";
      Obj       : CORBA.Object.Ref;

   begin
      Obj := Get_Effective_Target (Info);

      if Is_Equivalent (Obj, Test001_Globals.Test_Object) then
         Output (Point, Operation, True);
      else
         Output (Point, Operation, False);
      end if;

   exception
      when others =>
         Output (Point, Operation, False);
   end Test_Effective_Target;

   ----------------------------------
   -- Test_Get_Effective_Component --
   ----------------------------------

   procedure Test_Get_Effective_Component
     (Point : in     Client_Interception_Point;
      Info  : in     PortableInterceptor.ClientRequestInfo.Local_Ref)
   is
      pragma Unreferenced (Info);

      Operation : constant String := "get_effective_component";

   begin
      --  XXX Not yet implemented in ClientRequestInfo

      Output (Point, Operation, False, " (NO TEST)");
   end Test_Get_Effective_Component;

   -----------------------------------
   -- Test_Get_Effective_Components --
   -----------------------------------

   procedure Test_Get_Effective_Components
     (Point : in     Client_Interception_Point;
      Info  : in     PortableInterceptor.ClientRequestInfo.Local_Ref)
   is
      pragma Unreferenced (Info);

      Operation : constant String := "get_effective_components";

   begin
      --  XXX Not yet implemented in ClientRequestInfo

      Output (Point, Operation, False, " (NO TEST)");
   end Test_Get_Effective_Components;

   -----------------------------
   -- Test_Get_Request_Policy --
   -----------------------------

   procedure Test_Get_Request_Policy
     (Point : in     Client_Interception_Point;
      Info  : in     PortableInterceptor.ClientRequestInfo.Local_Ref)
   is
      Operation : constant String := "get_request_policy";
      Valid     : constant Boolean := Point /= Send_Poll;
      Pol       : CORBA.Policy.Ref;
      pragma Unreferenced (Pol);
      --  Assigned but never read

   begin
      --  XXX Functionality test not implemented

      Pol := Get_Request_Policy (Info, 1);

      if not Valid then
         Output (Point, Operation, False);
      else
         Output (Point, Operation, True);
      end if;

   exception
      when E : Inv_Policy =>
         declare
            Members : System_Exception_Members;
         begin
            Get_Members (E, Members);
            if Valid and then Members.Minor = 2 then
               Output (Point, Operation, True, " (INV_POLICY)");
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
   end Test_Get_Request_Policy;

   -----------------------------
   -- Test_Received_Exception --
   -----------------------------

   procedure Test_Received_Exception
     (Point  : in     Client_Interception_Point;
      Info   : in     PortableInterceptor.ClientRequestInfo.Local_Ref)
   is
      Operation : constant String := "received_exception";
      Valid     : constant Boolean := Point = Receive_Exception;
      Exc       : Any;

   begin
      Exc := Get_Received_Exception (Info);

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
            if not Valid and then Members.Minor = 14 then
               Output (Point, Operation, True);
            else
               Output (Point, Operation, False);
            end if;
         end;

      when others =>
         Output (Point, Operation, False);
   end Test_Received_Exception;

   --------------------------------
   -- Test_Received_Exception_Id --
   --------------------------------

   procedure Test_Received_Exception_Id
     (Point  : in     Client_Interception_Point;
      Info   : in     PortableInterceptor.ClientRequestInfo.Local_Ref)
   is
      Operation : constant String := "received_exception_id";
      Valid     : constant Boolean := Point = Receive_Exception;
      Id        : RepositoryId;

   begin
      Id := Get_Received_Exception_Id (Info);

      if Id /= Test_Exception_Repository_Id then
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
   end Test_Received_Exception_Id;

   -----------------
   -- Test_Target --
   -----------------

   procedure Test_Target
     (Point  : in     Client_Interception_Point;
      Info   : in     PortableInterceptor.ClientRequestInfo.Local_Ref)
   is
      Operation : constant String := "target";
      Obj       : CORBA.Object.Ref;

   begin
      Obj := Get_Target (Info);

      if Is_Equivalent (Obj, Test001_Globals.Test_Object) then
         Output (Point, Operation, True);
      else
         Output (Point, Operation, False);
      end if;

   exception
      when others =>
         Output (Point, Operation, False);
   end Test_Target;

end Test001_Client_Request_Info_Tests;
