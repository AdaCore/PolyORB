-----------------------------------------------------------------------
-----------------------------------------------------------------------
----                                                               ----
----                         AdaBroker                             ----
----                                                               ----
----                       package Giop_s                          ----
----                                                               ----
----                                                               ----
----   Copyright (C) 1999 ENST                                     ----
----                                                               ----
----   This file is part of the AdaBroker library                  ----
----                                                               ----
----   The AdaBroker library is free software; you can             ----
----   redistribute it and/or modify it under the terms of the     ----
----   GNU Library General Public License as published by the      ----
----   Free Software Foundation; either version 2 of the License,  ----
----   or (at your option) any later version.                      ----
----                                                               ----
----   This library is distributed in the hope that it will be     ----
----   useful, but WITHOUT ANY WARRANTY; without even the implied  ----
----   warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR     ----
----   PURPOSE.  See the GNU Library General Public License for    ----
----   more details.                                               ----
----                                                               ----
----   You should have received a copy of the GNU Library General  ----
----   Public License along with this library; if not, write to    ----
----   the Free Software Foundation, Inc., 59 Temple Place -       ----
----   Suite 330, Boston, MA 02111-1307, USA                       ----
----                                                               ----
----                                                               ----
----                                                               ----
----   Description                                                 ----
----   -----------                                                 ----
----                                                               ----
----     This package is wrapped around the C++ class              ----
----   Ada_Giop_s declared in Ada_Giop_s.hh.                       ----
----     It provides the same functions as this package plus       ----
----   the Ada version of thouse where arguments types are         ----
----   to be change.                                               ----
----     It does not include an Init function since it is          ----
----   useless for AdaBroker. (AdaBroker never creates a Giop_s    ----
----   object)                                                     ----
----                                                               ----
----                                                               ----
----   authors : Sebastien Ponce, Fabien Azavant                   ----
----   date    : 02/28/99                                          ----
----                                                               ----
-----------------------------------------------------------------------
-----------------------------------------------------------------------


with Interfaces.C ;
with Interfaces.CPP ;
with Interfaces.C.Strings ;
with System ;

with Corba ;
with Giop ;
with Sys_Dep ;
with Netbufferedstream ;

package Giop_S is

   type Object is new Netbufferedstream.Object with record
      Table1 : Interfaces.CPP.Vtable_Ptr ;
      -- Ada field : needed to interface C++ and Ada
   end record;
   pragma CPP_Class (Object) ;
   pragma CPP_Vtable (Object,Table1,1) ;
   -- this type is both a C and an Ada class
   -- it is wrapped around Ada_Giop_s
   -- (see Ada_Giop_s.hh)


   type Object_Ptr is access all Object ;
   -- type pointer on type Object


   function Reply_Header_Size (Self : in Object'Class)
                               return Corba.Unsigned_Long ;
   -- compute the size of the header for a reply


   procedure Request_Received (Self : in Object'Class ;
                               Skip : in Boolean) ;
   -- informs the ORB that the request was received
   -- (see giopDriver.h L150 for more details)


   procedure Initialize_Reply (Self : in Object'Class ;
                               Status : in Giop.Reply_Status_Type ;
                               MsgSize : in Corba.Unsigned_long);
   -- Initialisation of a reply
   -- (see giopDriver.h L150 for more details)


   procedure Reply_Completed (Self : in Object'Class);
   pragma Import (C,Reply_Completed,"ReplyCompleted__10Ada_Giop_s") ;
   -- wrapper around  Ada_Giop_s procedure InitialiseReply
   -- (see Ada_Giop_s.hh)
   -- it is both a C and an Ada procedure.
   -- it informs the ORB that the reply was completed
   -- (see giopDriver.h L150 for more details)


private

   function Constructor return Object'Class;
   pragma CPP_Constructor (Constructor);
   pragma Import (CPP,Constructor,"__10Ada_Giop_s");
   -- default constructor of the C class.
   -- Actually, this constructor does nothing.

end Giop_S ;

