------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--      T E S T 0 0 2 _ S E R V E R _ I N T E R C E P T O R . I M P L       --
--                                                                          --
--                                 S p e c                                  --
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

with PortableInterceptor.ServerRequestInfo;
with PortableInterceptor.ServerRequestInterceptor.Impl;

package Test002_Server_Interceptor.Impl is

   type Object is
      new PortableInterceptor.ServerRequestInterceptor.Impl.Object
         with private;

   type Object_Ptr is access all Object'Class;

private

   type Object is
      new PortableInterceptor.ServerRequestInterceptor.Impl.Object
         with null record;

   function Is_A
     (Self            : not null access Object;
      Logical_Type_Id : Standard.String) return Boolean;

   --  Derived from ServerRequestInterceptor

   procedure Receive_Request_Service_Contexts
     (Self : access Object;
      RI   : PortableInterceptor.ServerRequestInfo.Local_Ref);

   procedure Receive_Request
     (Self : access Object;
      RI   : PortableInterceptor.ServerRequestInfo.Local_Ref);

   procedure Send_Reply
     (Self : access Object;
      RI   : PortableInterceptor.ServerRequestInfo.Local_Ref);

end Test002_Server_Interceptor.Impl;
