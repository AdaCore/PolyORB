------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--   T E S T 0 0 0 _ I D L . S E R V E R I N T E R C E P T O R . I M P L    --
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
with Test000_Globals;

package body Test000_Idl.ServerInterceptor.Impl is

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
          (Logical_Type_Id, Test000_Idl.ServerInterceptor.Repository_Id)
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

   ---------------------
   -- Receive_Request --
   ---------------------

   procedure Receive_Request
     (Self : access Object;
      RI   : PortableInterceptor.ServerRequestInfo.Local_Ref)
   is
      pragma Unreferenced (RI);
   begin
      if not Self.Active then
         return;
      end if;

      Test000_Globals.Log_Point
        (CORBA.To_Standard_String (Self.Name), Server_Receive_Request);

      case Self.State (Server_Receive_Request) is
         when Do_Nothing =>
            null;

         when Raise_Exception =>
            CORBA.Raise_No_Permission (CORBA.Default_Sys_Member);

         when others =>
            raise Program_Error;
      end case;
   end Receive_Request;

   --------------------------------------
   -- Receive_Request_Service_Contexts --
   --------------------------------------

   procedure Receive_Request_Service_Contexts
     (Self : access Object;
      RI   : PortableInterceptor.ServerRequestInfo.Local_Ref)
   is
      pragma Unreferenced (RI);
   begin
      if not Self.Active then
         return;
      end if;

      Test000_Globals.Log_Point
        (CORBA.To_Standard_String (Self.Name),
         Server_Receive_Request_Service_Contexts);

      case Self.State (Server_Receive_Request_Service_Contexts) is
         when Do_Nothing =>
            null;

         when Raise_Exception =>
            CORBA.Raise_No_Permission (CORBA.Default_Sys_Member);

         when others =>
            raise Program_Error;
      end case;
   end Receive_Request_Service_Contexts;

   --------------------
   -- Send_Exception --
   --------------------

   procedure Send_Exception
     (Self : access Object;
      RI   : PortableInterceptor.ServerRequestInfo.Local_Ref)
   is
      pragma Unreferenced (RI);
   begin
      if not Self.Active then
         return;
      end if;

      Test000_Globals.Log_Point
        (CORBA.To_Standard_String (Self.Name), Server_Send_Exception);

      case Self.State (Server_Send_Exception) is
         when Do_Nothing =>
            null;

         when Raise_Exception =>
            CORBA.Raise_No_Permission (CORBA.Default_Sys_Member);

         when others =>
            raise Program_Error;
      end case;
   end Send_Exception;

   ----------------
   -- Send_Other --
   ----------------

   procedure Send_Other
     (Self : access Object;
      RI   : PortableInterceptor.ServerRequestInfo.Local_Ref)
   is
      pragma Unreferenced (RI);
   begin
      if not Self.Active then
         return;
      end if;

      Test000_Globals.Log_Point
        (CORBA.To_Standard_String (Self.Name), Server_Send_Other);

      case Self.State (Server_Send_Other) is
         when Do_Nothing =>
            null;

         when Raise_Exception =>
            CORBA.Raise_No_Permission (CORBA.Default_Sys_Member);

         when others =>
            raise Program_Error;
      end case;
   end Send_Other;

   ----------------
   -- Send_Reply --
   ----------------

   procedure Send_Reply
     (Self : access Object;
      RI   : PortableInterceptor.ServerRequestInfo.Local_Ref)
   is
      pragma Unreferenced (RI);
   begin
      if not Self.Active then
         return;
      end if;

      Test000_Globals.Log_Point
        (CORBA.To_Standard_String (Self.Name), Server_Send_Reply);

      case Self.State (Server_Send_Reply) is
         when Do_Nothing =>
            null;

         when Raise_Exception =>
            CORBA.Raise_No_Permission (CORBA.Default_Sys_Member);

         when others =>
            raise Program_Error;
      end case;
   end Send_Reply;

   ------------------
   -- Set_Behavior --
   ------------------

   procedure Set_Behavior
     (Self     : access Object;
      Point    : Server_Interception_Point;
      Behavior : Interceptor_Behavior)
   is
   begin
      Self.State (Point) := Behavior;
   end Set_Behavior;

end Test000_Idl.ServerInterceptor.Impl;
