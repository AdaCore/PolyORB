------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--            PORTABLEINTERCEPTOR.CLIENTREQUESTINTERCEPTOR.IMPL             --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2004-2012, Free Software Foundation, Inc.          --
--                                                                          --
-- This specification is derived from the CORBA Specification, and adapted  --
-- for use with PolyORB. The copyright notice above, and the license        --
-- provisions that follow apply solely to the contents neither explicitly   --
-- nor implicitly specified by the CORBA Specification defined by the OMG.  --
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

with PortableInterceptor.ClientRequestInfo;
with PortableInterceptor.Interceptor.Impl;

package PortableInterceptor.ClientRequestInterceptor.Impl is

   type Object is
      new PortableInterceptor.Interceptor.Impl.Object with private;

   type Object_Ptr is access all Object'Class;

   procedure Send_Request
     (Self : access Object;
      RI   : PortableInterceptor.ClientRequestInfo.Local_Ref);

   procedure Send_Poll
     (Self : access Object;
      RI   : PortableInterceptor.ClientRequestInfo.Local_Ref);

   procedure Receive_Reply
     (Self : access Object;
      RI   : PortableInterceptor.ClientRequestInfo.Local_Ref);

   procedure Receive_Exception
     (Self : access Object;
      RI   : PortableInterceptor.ClientRequestInfo.Local_Ref);

   procedure Receive_Other
     (Self : access Object;
      RI   : PortableInterceptor.ClientRequestInfo.Local_Ref);

   function Is_A
     (Self            : not null access Object;
      Logical_Type_Id : Standard.String) return Boolean;

private

   type Object is
      new PortableInterceptor.Interceptor.Impl.Object with null record;

end PortableInterceptor.ClientRequestInterceptor.Impl;
