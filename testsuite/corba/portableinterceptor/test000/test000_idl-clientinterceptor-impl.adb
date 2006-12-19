------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--   T E S T 0 0 0 _ I D L . C L I E N T I N T E R C E P T O R . I M P L    --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2004-2006, Free Software Foundation, Inc.          --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
-- not, write to the Free Software Foundation, 51 Franklin Street, Fifth    --
-- Floor, Boston, MA 02111-1301, USA.                                       --
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
      Name : in     Standard.String)
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
      Logical_Type_Id : in     Standard.String)
      return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return CORBA.Is_Equivalent
        (Logical_Type_Id,
         Test000_Idl.ClientInterceptor.Repository_Id)
        or else CORBA.Is_Equivalent
          (Logical_Type_Id,
           "IDL:omg.org/CORBA/Object:1.0")
        or else CORBA.Is_Equivalent
           (Logical_Type_Id,
         PortableInterceptor.ClientRequestInterceptor.Repository_Id)
        or else CORBA.Is_Equivalent
           (Logical_Type_Id,
         PortableInterceptor.Interceptor.Repository_Id);
   end Is_A;

   -----------------------
   -- Receive_Exception --
   -----------------------

   procedure Receive_Exception
     (Self : access Object;
      RI   : in     PortableInterceptor.ClientRequestInfo.Local_Ref)
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
      RI   : in     PortableInterceptor.ClientRequestInfo.Local_Ref)
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
      RI   : in     PortableInterceptor.ClientRequestInfo.Local_Ref)
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
      RI   : in     PortableInterceptor.ClientRequestInfo.Local_Ref)
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
      RI   : in     PortableInterceptor.ClientRequestInfo.Local_Ref)
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
      Point    : in     Client_Interception_Point;
      Behavior : in     Interceptor_Behavior)
   is
   begin
      Self.State (Point) := Behavior;
   end Set_Behavior;

end Test000_Idl.ClientInterceptor.Impl;
