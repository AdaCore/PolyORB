------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--              PORTABLEINTERCEPTOR.SERVERREQUESTINTERCEPTOR                --
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

with CORBA.Object;

with PortableInterceptor.ServerRequestInterceptor.Impl;

package body PortableInterceptor.ServerRequestInterceptor is

   ---------------------
   -- Receive_Request --
   ---------------------

   procedure Receive_Request
     (Self : in Local_Ref;
      RI   : in PortableInterceptor.ServerRequestInfo.Local_Ref)
   is
      Self_Ref : CORBA.Object.Ref
        := CORBA.Object.Ref (Self);
   begin

      if CORBA.Object.Is_Nil (Self_Ref) then
         CORBA.Raise_Inv_Objref (CORBA.Default_Sys_Member);
      end if;

      PortableInterceptor.ServerRequestInterceptor.Impl.Receive_Request
        (PortableInterceptor.ServerRequestInterceptor.Impl.Object_Ptr
           (Entity_Of (Self)),
         RI);
   end Receive_Request;

   --------------------------------------
   -- Receive_Request_Service_Contexts --
   --------------------------------------

   procedure Receive_Request_Service_Contexts
     (Self : in Local_Ref;
      RI   : in PortableInterceptor.ServerRequestInfo.Local_Ref)
   is
      Self_Ref : constant CORBA.Object.Ref := CORBA.Object.Ref (Self);
   begin

      if CORBA.Object.Is_Nil (Self_Ref) then
         CORBA.Raise_Inv_Objref (CORBA.Default_Sys_Member);
      end if;

      Impl.Receive_Request_Service_Contexts
        (PortableInterceptor.ServerRequestInterceptor.Impl.Object_Ptr
          (Entity_Of (Self)),
         RI);
   end Receive_Request_Service_Contexts;

   --------------------
   -- Send_Exception --
   --------------------

   procedure Send_Exception
     (Self : in Local_Ref;
      RI   : in PortableInterceptor.ServerRequestInfo.Local_Ref)
   is
      Self_Ref : constant CORBA.Object.Ref := CORBA.Object.Ref (Self);
   begin

      if CORBA.Object.Is_Nil (Self_Ref) then
         CORBA.Raise_Inv_Objref (CORBA.Default_Sys_Member);
      end if;

      PortableInterceptor.ServerRequestInterceptor.Impl.Send_Exception
        (PortableInterceptor.ServerRequestInterceptor.Impl.Object_Ptr
           (Entity_Of (Self)),
         RI);
   end Send_Exception;

   ----------------
   -- Send_Other --
   ----------------

   procedure Send_Other
     (Self : in Local_Ref;
      RI   : in PortableInterceptor.ServerRequestInfo.Local_Ref)
   is
      Self_Ref : constant CORBA.Object.Ref := CORBA.Object.Ref (Self);
   begin

      if CORBA.Object.Is_Nil (Self_Ref) then
         CORBA.Raise_Inv_Objref (CORBA.Default_Sys_Member);
      end if;

      PortableInterceptor.ServerRequestInterceptor.Impl.Send_Other
        (PortableInterceptor.ServerRequestInterceptor.Impl.Object_Ptr
           (Entity_Of (Self)),
         RI);
   end Send_Other;

   ----------------
   -- Send_Reply --
   ----------------

   procedure Send_Reply
     (Self : in Local_Ref;
      RI   : in PortableInterceptor.ServerRequestInfo.Local_Ref)
   is
      Self_Ref : constant CORBA.Object.Ref := CORBA.Object.Ref (Self);
   begin

      if CORBA.Object.Is_Nil (Self_Ref) then
         CORBA.Raise_Inv_Objref (CORBA.Default_Sys_Member);
      end if;

      PortableInterceptor.ServerRequestInterceptor.Impl.Send_Reply
        (PortableInterceptor.ServerRequestInterceptor.Impl.Object_Ptr
           (Entity_Of (Self)),
         RI);
   end Send_Reply;

end PortableInterceptor.ServerRequestInterceptor;
