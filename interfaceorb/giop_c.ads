-----------------------------------------------------------------------
-----------------------------------------------------------------------
----                                                               ----
----                         AdaBroker                             ----
----                                                               ----
----                       package Giop_c                          ----
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
----   Ada_Giop_c declared in Ada_Giop_c.hh.                       ----
----     It provides the same functions as this package plus       ----
----   the Ada version of thouse where arguments types are         ----
----   to be change.                                               ----
----     It includes the definition of the function                ----
----   RequestHeaderSize declared in giopDriver.h but not          ----
----   present in Ada_Giop_c since it is a static function.        ----
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
with Rope ;
with Netbufferedstream ;

package Giop_C is

   type Object is new NetbufferedStream.Object with record
      Table1 : Interfaces.CPP.Vtable_Ptr ;
      -- Ada field : needed to interface C++ and Ada
   end record;
   pragma CPP_Class (Object) ;
   pragma CPP_Vtable (Object,Table1,1) ;
   -- this type is both a C and an Ada class
   -- it is wrapped around Ada_Giop_c
   -- (see Ada_Giop_c.hh)


   type Object_Ptr is access all Object ;
   -- type pointer on type Object


   procedure Init (Self : in out Object'Class ;
                   R : in Rope.Object) ;
   -- Ada constructor of the class.
   -- This function must be called after each declaration of
   -- an Object object. If it is not, you can not use the object.


   procedure Initialize_Request (Self : in Object'Class ;
                                 Objkey : in Corba.Octet ;
                                 Objkeysize : in Corba.Unsigned_Long ;
                                 Opname : in CORBA.STRING ;
                                 MsgSize : in Corba.Unsigned_Long ;
                                 Oneway : in CORBA.Boolean) ;
   -- Initialisation of a request
   -- (see giopDriver.h L150 for more details)


   function Receive_Reply (Self : in Object'Class)
                           return Giop.Reply_Status_Type ;
   -- called to inform the ORD that the reply was received
   -- (see giopDriver.h L150 for more details)


   procedure Request_Completed (Self : in Object'Class ;
                                Skip_Msg : in CORBA.Boolean := False) ;
   -- called to inform the ORB that the request was completed
   -- (see giopDriver.h L150 for more details)


   function Request_Header_Size (Objkeysize : in Corba.Unsigned_long ;
                                 Opnamesize : in Corba.Unsigned_long)
                                 return Corba.Unsigned_long ;
   -- Returns the header size. This includes the size of the GIOP message
   -- header and the Request message header.


private

   function Constructor return Object'Class;
   pragma CPP_Constructor (Constructor);
   pragma Import (CPP,Constructor,"__10Ada_Giop_c");
   -- default constructor of the C class.
   -- Actually, this constructor does nothing and you must
   -- call Init to init properly an object.

end Giop_C ;

