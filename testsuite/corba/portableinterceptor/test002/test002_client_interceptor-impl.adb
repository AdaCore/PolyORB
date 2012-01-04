------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--      T E S T 0 0 2 _ C L I E N T _ I N T E R C E P T O R . I M P L       --
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

with PortableInterceptor.Interceptor;
with PortableInterceptor.ClientRequestInterceptor;
with PortableInterceptor.Current;

with PolyORB.Utils.Report;

with Test002_Globals;

package body Test002_Client_Interceptor.Impl is

   use CORBA;
   use CORBA.TypeCode;
   use PolyORB.Utils.Report;
   use PortableInterceptor.ClientRequestInfo;
   use PortableInterceptor.Current;
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
          (Logical_Type_Id, Test002_Client_Interceptor.Repository_Id)
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

   -------------------
   -- Receive_Reply --
   -------------------

   procedure Receive_Reply
     (Self : access Object;
      RI   : PortableInterceptor.ClientRequestInfo.Local_Ref)
   is
      pragma Unreferenced (Self);

      Aux : Any;

   begin
      Aux := Get_Slot (RI, Test_Slot);

      if Get_Type (Aux) /= TC_Long then
         Output ("Slot value is unchanged in CRSC after invocation", False);
      elsif From_Any (Aux) /= Long (10) then
         Output ("Slot value is unchanged in CRSC after invocation", False);
      else
         Output ("Slot value is unchanged in CRSC after invocation", True);
      end if;

      Set_Slot (PI_Current, Test_Slot, To_Any (Long (14)));
   exception
      when others =>
         Output ("Slot value is unchanged in CRSC after invocation", False);
   end Receive_Reply;

   ------------------
   -- Send_Request --
   ------------------

   procedure Send_Request
     (Self : access Object;
      RI   : PortableInterceptor.ClientRequestInfo.Local_Ref)
   is
      pragma Unreferenced (Self);

      Aux : Any;

   begin
      Aux := Get_Slot (RI, Test_Slot);

      if Get_Type (Aux) /= TC_Long then
         Output ("Slot value correctly copied from CTSC to CRSC", False);
      elsif From_Any (Aux) /= Long (10) then
         Output ("Slot value correctly copied from CTSC to CRSC", False);
      else
         Output ("Slot value correctly copied from CTSC to CRSC", True);
      end if;

      Set_Slot (PI_Current, Test_Slot, To_Any (Long (12)));
   exception
      when others =>
         Output ("Slot value correctly copied from CTSC to CRSC", False);
   end Send_Request;

end Test002_Client_Interceptor.Impl;
