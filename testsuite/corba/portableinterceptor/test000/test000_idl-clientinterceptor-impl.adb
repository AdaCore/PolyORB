------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--   T E S T 0 0 0 _ I D L . C L I E N T I N T E R C E P T O R . I M P L    --
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

with PortableInterceptor.Interceptor;
with PortableInterceptor.ClientRequestInterceptor;

with Test000_Globals;

package body Test000_Idl.ClientInterceptor.Impl is

   -------------
   -- Disable --
   -------------

   procedure Disable (Self : access Object) is
   begin
      Self.Active := False;
   end Disable;

   ------------
   -- Enable --
   ------------

   procedure Enable (Self : access Object) is
   begin
      Self.Active := True;
   end Enable;

   --------------
   -- Get_Name --
   --------------

   function Get_Name (Self : access Object) return CORBA.String is
   begin
      return Self.Name;
   end Get_Name;

   ----------
   -- Init --
   ----------

   procedure Init
     (Self : access Object;
      Name : Standard.String)
   is
   begin
      Self.Name   := CORBA.To_CORBA_String (Name);
      Self.State  := (others => Do_Nothing);
      Self.Active := False;
   end Init;

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
          (Logical_Type_Id, Test000_Idl.ClientInterceptor.Repository_Id)
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
      pragma Unreferenced (RI);
   begin
      if not Self.Active then
         return;
      end if;

      Test000_Globals.Log_Point
       (CORBA.To_Standard_String (Self.Name), Client_Receive_Exception);

      case Self.State (Client_Receive_Exception) is
         when Do_Nothing =>
            null;

         when Raise_Exception =>
            CORBA.Raise_No_Permission (CORBA.Default_Sys_Member);

         when others =>
            raise Program_Error;
      end case;
   end Receive_Exception;

   -------------------
   -- Receive_Other --
   -------------------

   procedure Receive_Other
     (Self : access Object;
      RI   : PortableInterceptor.ClientRequestInfo.Local_Ref)
   is
      pragma Unreferenced (RI);
   begin
      if not Self.Active then
         return;
      end if;

      Test000_Globals.Log_Point
       (CORBA.To_Standard_String (Self.Name), Client_Receive_Other);

      case Self.State (Client_Receive_Other) is
         when Do_Nothing =>
            null;

         when others =>
            raise Program_Error;
      end case;
   end Receive_Other;

   -------------------
   -- Receive_Reply --
   -------------------

   procedure Receive_Reply
     (Self : access Object;
      RI   : PortableInterceptor.ClientRequestInfo.Local_Ref)
   is
      pragma Unreferenced (RI);
   begin
      if not Self.Active then
         return;
      end if;

      Test000_Globals.Log_Point
       (CORBA.To_Standard_String (Self.Name), Client_Receive_Reply);

      case Self.State (Client_Receive_Reply) is
         when Do_Nothing =>
            null;

         when Raise_Exception =>
            CORBA.Raise_No_Permission (CORBA.Default_Sys_Member);

         when others =>
            raise Program_Error;
      end case;
   end Receive_Reply;

   ------------------
   -- Send_Request --
   ------------------

   procedure Send_Request
     (Self : access Object;
      RI   : PortableInterceptor.ClientRequestInfo.Local_Ref)
   is
      pragma Unreferenced (RI);
   begin
      if not Self.Active then
         return;
      end if;

      Test000_Globals.Log_Point
       (CORBA.To_Standard_String (Self.Name), Client_Send_Request);

      case Self.State (Client_Send_Request) is
         when Do_Nothing =>
            null;

         when Raise_Exception =>
            CORBA.Raise_No_Permission (CORBA.Default_Sys_Member);

         when others =>
            raise Program_Error;
      end case;
   end Send_Request;

   ---------------
   -- Send_Poll --
   ---------------

   procedure Send_Poll
     (Self : access Object;
      RI   : PortableInterceptor.ClientRequestInfo.Local_Ref)
   is
      pragma Unreferenced (RI);
   begin
      if not Self.Active then
         return;
      end if;

      Test000_Globals.Log_Point
       (CORBA.To_Standard_String (Self.Name), Client_Send_Poll);

      case Self.State (Client_Send_Poll) is
         when Do_Nothing =>
            null;

         when others =>
            raise Program_Error;
      end case;
   end Send_Poll;

   ------------------
   -- Set_Behavior --
   ------------------

   procedure Set_Behavior
     (Self     : access Object;
      Point    : Client_Interception_Point;
      Behavior : Interceptor_Behavior)
   is
   begin
      Self.State (Point) := Behavior;
   end Set_Behavior;

end Test000_Idl.ClientInterceptor.Impl;
