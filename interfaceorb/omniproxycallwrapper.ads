-----------------------------------------------------------------------
-----------------------------------------------------------------------
----                                                               ----
----                         AdaBroker                             ----
----                                                               ----
----                       package Giop                            ----
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
----   For each function defined in the IDL file, a descendant     ----
----   of Omniproxycalldesc is created. It is the object in        ----
----   charge of storing the arguments of the function,            ----
----   marshalling them into a bufferedstream, call the remote     ----
----   object, and unmarshall the result.                          ----
----                                                               ----
----                                                               ----
----   authors : Sebastien Ponce, Fabien Azavant                   ----
----   date    : 02/28/99                                          ----
----                                                               ----
-----------------------------------------------------------------------
-----------------------------------------------------------------------


with Corba.Object ;
with Omniproxycalldesc ;
with Omniobject ;

package omniProxyCallWrapper is

   procedure Invoke (Obj : in Corba.Object.Ref'Class ;
                     Call_Desc : in out OmniProxyCallDesc.Object'Class ) ;
   -- reimplemented in Ada to call the C++ ORB
   -- (modified by Fabien)
   --
   -- previous solution :
   -- wrapper around void invoke(omniObject* o, OmniProxyCallDesc& call_desc)
   -- in proxyCall.cc L 46


   procedure One_Way(Obj : in Corba.Object.Ref'Class ;
                     Call_Desc : in out OmniProxyCallDesc.Object'Class) ;
   -- reimplemented in Ada to call the C++ ORB
   -- see proxyCall.cc L181


private

   function Omni_Call_Transient_Exception_Handler
     (Obj : in Omniobject.Object'Class ;
      Retries : in Corba.Unsigned_Long ;
      Minor : in Corba.Unsigned_Long ;
      Status : in Corba.Completion_Status)
      return Corba.Boolean ;
   -- This method is wrapped around C method _omni_callTransientExceptionHandler
   -- ( see Ada_Corba_Exceptions.hh )


   function Omni_Comm_Failure_Exception_Handler
     (Obj : in Omniobject.Object'Class ;
      Retries : in Corba.Unsigned_Long ;
      Minor : in Corba.Unsigned_Long ;
      Status : in Corba.Completion_Status)
      return Corba.Boolean ;
   -- This method is wrapped around C method _omni_commFailureExceptionHandler
   -- ( see Ada_Corba_Exceptions.hh )


   function Omni_System_Exception_Handler
     (Obj : in Omniobject.Object'Class ;
      Retries : in Corba.Unsigned_Long ;
      Minor : in Corba.Unsigned_Long ;
      Status : in Corba.Completion_Status)
      return Corba.Boolean ;
   -- This method is wrapped around C method _omni_callSystemExceptionHandler
   -- ( see Ada_Corba_Exceptions.hh )


end omniproxyCallWrapper ;



