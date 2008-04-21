------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--   T E S T 0 0 0 _ I D L . S E R V E R I N T E R C E P T O R . I M P L    --
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
--                PolyORB is maintained by ACT Europe.                      --
--                    (email: sales@act-europe.fr)                          --
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
     (Self            : access Object;
      Logical_Type_Id : Standard.String)
      return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return CORBA.Is_Equivalent
        (Logical_Type_Id,
         Test000_Idl.ServerInterceptor.Repository_Id)
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
