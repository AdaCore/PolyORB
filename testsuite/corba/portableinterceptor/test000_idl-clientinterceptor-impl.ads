------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--   T E S T 0 0 0 _ I D L . C L I E N T I N T E R C E P T O R . I M P L    --
--                                                                          --
--                                 S p e c                                  --
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

with CORBA;
with PortableInterceptor.ClientRequestInfo;
with PortableInterceptor.ClientRequestInterceptor.Impl;

package Test000_Idl.ClientInterceptor.Impl is

   type Object is
     new PortableInterceptor.ClientRequestInterceptor.Impl.Object with private;

   type Object_Ptr is access all Object'Class;

   procedure Init
     (Self : access Object;
      Name : in     Standard.String);

   procedure Set_Behavior
     (Self     : access Object;
      Point    : in     Client_Interception_Point;
      Behavior : in     Interceptor_Behavior);

   procedure Enable (Self : access Object);

   procedure Disable (Self : access Object);

   function Is_A
     (Self : access Object;
      Logical_Type_Id : Standard.String)
     return Boolean;

private

   type State_Array is
     array (Client_Interception_Point) of Interceptor_Behavior;

   type Object is
     new PortableInterceptor.ClientRequestInterceptor.Impl.Object with
   record
      Name   : CORBA.String;
      State  : State_Array;
      Active : Boolean;
   end record;

   --  Derived from Interceptor.

   function Get_Name (Self : access Object) return CORBA.String;

   --  Derived from ClientRequestInterceptor.

   procedure Send_Request
     (Self : access Object;
      RI   : in     PortableInterceptor.ClientRequestInfo.Local_Ref);

   procedure Send_Poll
     (Self : access Object;
      RI   : in     PortableInterceptor.ClientRequestInfo.Local_Ref);

   procedure Receive_Reply
     (Self : access Object;
      RI   : in     PortableInterceptor.ClientRequestInfo.Local_Ref);

   procedure Receive_Exception
     (Self : access Object;
      RI   : in     PortableInterceptor.ClientRequestInfo.Local_Ref);

   procedure Receive_Other
     (Self : access Object;
      RI   : in     PortableInterceptor.ClientRequestInfo.Local_Ref);

end Test000_Idl.ClientInterceptor.Impl;
