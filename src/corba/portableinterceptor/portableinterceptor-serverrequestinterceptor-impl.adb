------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--            PORTABLEINTERCEPTOR.SERVERREQUESTINTERCEPTOR.IMPL             --
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

package body PortableInterceptor.ServerRequestInterceptor.Impl is

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
         PortableInterceptor.ServerRequestInterceptor.Repository_Id)
        or else CORBA.Is_Equivalent
          (Logical_Type_Id,
           "IDL:omg.org/CORBA/Object:1.0")
        or else CORBA.Is_Equivalent
           (Logical_Type_Id,
            PortableInterceptor.Interceptor.Repository_Id);
   end Is_A;

   ---------------------
   -- Receive_Request --
   ---------------------

   procedure Receive_Request
     (Self : access Object;
      RI   : in     PortableInterceptor.ServerRequestInfo.Local_Ref)
   is
      pragma Unreferenced (Self);
      pragma Unreferenced (RI);
   begin
      null;
   end Receive_Request;

   --------------------------------------
   -- Receive_Request_Service_Contexts --
   --------------------------------------

   procedure Receive_Request_Service_Contexts
     (Self : access Object;
      RI   : in     PortableInterceptor.ServerRequestInfo.Local_Ref)
   is
      pragma Unreferenced (Self);
      pragma Unreferenced (RI);
   begin
      null;
   end Receive_Request_Service_Contexts;

   --------------------
   -- Send_Exception --
   --------------------

   procedure Send_Exception
     (Self : access Object;
      RI   : in     PortableInterceptor.ServerRequestInfo.Local_Ref)
   is
      pragma Unreferenced (Self);
      pragma Unreferenced (RI);
   begin
      null;
   end Send_Exception;

   ----------------
   -- Send_Other --
   ----------------

   procedure Send_Other
     (Self : access Object;
      RI   : in     PortableInterceptor.ServerRequestInfo.Local_Ref)
   is
      pragma Unreferenced (Self);
      pragma Unreferenced (RI);
   begin
      null;
   end Send_Other;

   ----------------
   -- Send_Reply --
   ----------------

   procedure Send_Reply
     (Self : access Object;
      RI   : in     PortableInterceptor.ServerRequestInfo.Local_Ref)
   is
      pragma Unreferenced (Self);
      pragma Unreferenced (RI);
   begin
      null;
   end Send_Reply;

end PortableInterceptor.ServerRequestInterceptor.Impl;
