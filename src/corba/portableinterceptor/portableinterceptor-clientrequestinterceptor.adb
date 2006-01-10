------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--              PORTABLEINTERCEPTOR.CLIENTREQUESTINTERCEPTOR                --
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

with CORBA;
with CORBA.Object;
with PortableInterceptor.ClientRequestInterceptor.Impl;

package body PortableInterceptor.ClientRequestInterceptor is

   -----------------------
   -- Receive_Exception --
   -----------------------

   procedure Receive_Exception
     (Self : Local_Ref;
      RI   : PortableInterceptor.ClientRequestInfo.Local_Ref)
   is
      Self_Ref : constant CORBA.Object.Ref := CORBA.Object.Ref (Self);
   begin

      if CORBA.Object.Is_Nil (Self_Ref) then
         CORBA.Raise_Inv_Objref (CORBA.Default_Sys_Member);
      end if;

      PortableInterceptor.ClientRequestInterceptor.Impl.Receive_Exception
        (PortableInterceptor.ClientRequestInterceptor.Impl.Object_Ptr
           (Entity_Of (Self)),
         RI);
   end Receive_Exception;

   -------------------
   -- Receive_Other --
   -------------------

   procedure Receive_Other
     (Self : Local_Ref;
      RI   : PortableInterceptor.ClientRequestInfo.Local_Ref)
   is
      Self_Ref : constant CORBA.Object.Ref := CORBA.Object.Ref (Self);
   begin

      if CORBA.Object.Is_Nil (Self_Ref) then
         CORBA.Raise_Inv_Objref (CORBA.Default_Sys_Member);
      end if;

      PortableInterceptor.ClientRequestInterceptor.Impl.Receive_Other
        (PortableInterceptor.ClientRequestInterceptor.Impl.Object_Ptr
           (Entity_Of (Self)),
         RI);
   end Receive_Other;

   -------------------
   -- Receive_Reply --
   -------------------

   procedure Receive_Reply
     (Self : Local_Ref;
      RI   : PortableInterceptor.ClientRequestInfo.Local_Ref)
   is
      Self_Ref : constant CORBA.Object.Ref := CORBA.Object.Ref (Self);
   begin

      if CORBA.Object.Is_Nil (Self_Ref) then
         CORBA.Raise_Inv_Objref (CORBA.Default_Sys_Member);
      end if;

      PortableInterceptor.ClientRequestInterceptor.Impl.Receive_Reply
        (PortableInterceptor.ClientRequestInterceptor.Impl.Object_Ptr
           (Entity_Of (Self)),
         RI);
   end Receive_Reply;

   ---------------
   -- Send_Poll --
   ---------------

   procedure Send_Poll
     (Self : Local_Ref;
      RI   : PortableInterceptor.ClientRequestInfo.Local_Ref)
   is
      Self_Ref : constant CORBA.Object.Ref := CORBA.Object.Ref (Self);
   begin

      if CORBA.Object.Is_Nil (Self_Ref) then
         CORBA.Raise_Inv_Objref (CORBA.Default_Sys_Member);
      end if;

      PortableInterceptor.ClientRequestInterceptor.Impl.Send_Poll
        (PortableInterceptor.ClientRequestInterceptor.Impl.Object_Ptr
           (Entity_Of (Self)),
         RI);
   end Send_Poll;

   ------------------
   -- Send_Request --
   ------------------

   procedure Send_Request
     (Self : Local_Ref;
      RI   : PortableInterceptor.ClientRequestInfo.Local_Ref)
   is
      Self_Ref : constant CORBA.Object.Ref := CORBA.Object.Ref (Self);
   begin

      if CORBA.Object.Is_Nil (Self_Ref) then
         CORBA.Raise_Inv_Objref (CORBA.Default_Sys_Member);
      end if;

      PortableInterceptor.ClientRequestInterceptor.Impl.Send_Request
        (PortableInterceptor.ClientRequestInterceptor.Impl.Object_Ptr
           (Entity_Of (Self)),
         RI);
   end Send_Request;

end PortableInterceptor.ClientRequestInterceptor;
