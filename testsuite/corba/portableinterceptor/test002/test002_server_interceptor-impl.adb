------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--      T E S T 0 0 2 _ S E R V E R _ I N T E R C E P T O R . I M P L       --
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

with PortableInterceptor.Current;
with PortableInterceptor.Interceptor;
with PortableInterceptor.ServerRequestInterceptor;

with PolyORB.Utils.Report;

with Test002_Globals;

package body Test002_Server_Interceptor.Impl is

   use CORBA;
   use CORBA.TypeCode;
   use PolyORB.Utils.Report;
   use PortableInterceptor.Current;
   use PortableInterceptor.ServerRequestInfo;
   use Test002_Globals;

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
          (Logical_Type_Id, Test002_Server_Interceptor.Repository_Id)
          or else
        CORBA.Is_Equivalent
          (Logical_Type_Id,
           PortableInterceptor.ServerRequestInterceptor.Repository_Id)
          or else
        CORBA.Is_Equivalent
          (Logical_Type_Id, PortableInterceptor.Interceptor.Repository_Id)
          or else
        CORBA.Is_Equivalent
          (Logical_Type_Id, "IDL:omg.org/CORBA/Object:1.0");
   end Is_A;

   --------------------------------------
   -- Receive_Request_Service_Contexts --
   --------------------------------------

   procedure Receive_Request_Service_Contexts
     (Self : access Object;
      RI   : PortableInterceptor.ServerRequestInfo.Local_Ref)
   is
      pragma Unreferenced (Self);

      Aux : Any;

   begin
      begin
         Aux := Get_Slot (RI, Test_Slot);

         if Get_Type (Aux) /= TC_Null then
            Output ("Uninitialized SRSC slot value", False);
         else
            Output ("Uninitialized SRSC slot value", True);
         end if;

      exception
         when others =>
            Output ("Uninitialized SRSC slot value", False);
      end;

      begin
         Set_Slot (RI, Test_Slot, To_Any (Long (16)));

         begin
            Aux := Get_Slot (RI, Test_Slot);

            if Get_Type (Aux) /= TC_Long then
               Output ("Setting SRSC slot value", False);
            elsif From_Any (Aux) /= Long (16) then
               Output ("Setting SRSC slot value", False);
            else
               Output ("Setting SRSC slot value", True);
            end if;
         end;

      exception
         when others =>
            Output ("Setting SRSC slot value", False);
      end;

      --  Preparing for test of correctly copied SRSC to STSC

      Set_Slot (PI_Current, Test_Slot, To_Any (Long (18)));
   end Receive_Request_Service_Contexts;

   ---------------------
   -- Receive_Request --
   ---------------------

   procedure Receive_Request
     (Self : access Object;
      RI   : PortableInterceptor.ServerRequestInfo.Local_Ref)
   is
      pragma Unreferenced (Self);

      Aux : Any;

   begin
      begin
         Aux := Get_Slot (PI_Current, Test_Slot);

         if Get_Type (Aux) /= TC_Long then
            Output ("Slot value correctly copied from SRSC to STSC", False);
         elsif From_Any (Aux) /= Long (16) then
            Output ("Slot value correctly copied from SRSC to STSC", False);
         else
            Output ("Slot value correctly copied from SRSC to STSC", True);
         end if;

      exception
         when others =>
            Output ("Slot value correctly copied from SRSC to STSC", False);
      end;

      begin
         Aux := Get_Slot (RI, Test_Slot);

         if Get_Type (Aux) /= TC_Long then
            Output ("Slot value is unchanged in SRSC", False);
         elsif From_Any (Aux) /= Long (16) then
            Output ("Slot value is unchanged in SRSC", False);
         else
            Output ("Slot value is unchanged in SRSC", True);
         end if;

      exception
         when others =>
            Output ("Slot value is unchanged in SRSC", False);
      end;

      --  Preparing for test of passing STSC to servant manager

      Set_Slot (PI_Current, Test_Slot, To_Any (Long (20)));
   end Receive_Request;

   ----------------
   -- Send_Reply --
   ----------------

   procedure Send_Reply
     (Self : access Object;
      RI   : PortableInterceptor.ServerRequestInfo.Local_Ref)
   is
      pragma Unreferenced (Self);

      Aux : Any;

   begin
      begin
         Aux := Get_Slot (PI_Current, Test_Slot);

         if Get_Type (Aux) /= TC_Long then
            Output
              ("Slot value correctly passed to send point in STSC", False);
         elsif From_Any (Aux) /= Long (22) then
            Output
              ("Slot value correctly passed to send point in STSC", False);
         else
            Output ("Slot value correctly passed to send point in STSC", True);
         end if;

      exception
         when others =>
            Output
              ("Slot value correctly passed to send point in STSC", False);
      end;

      begin
         Aux := Get_Slot (RI, Test_Slot);

         if Get_Type (Aux) /= TC_Long then
            Output ("Slot value correctly copied from STSC to SRSC", False);
         elsif From_Any (Aux) /= Long (22) then
            Output ("Slot value correctly copied from STSC to SRSC", False);
         else
            Output ("Slot value correctly copied from STSC to SRSC", True);
         end if;

      exception
         when others =>
            Output ("Slot value correctly copied from STSC to SRSC", False);
      end;

      --  Setting to STSC slot value another value

      Set_Slot (PI_Current, Test_Slot, To_Any (Long (24)));
   end Send_Reply;

end Test002_Server_Interceptor.Impl;
