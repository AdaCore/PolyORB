-----------------------------------------------------------------------
-----------------------------------------------------------------------
----                                                               ----
----                         AdaBroker                             ----
----                                                               ----
----                package Omniowproxycalldesc                    ----
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
----      This is a root class. For each subprogram of an IDL      ----
----    interface which is declared "one way", a descendant of     ----
----    this class has to be provided.                             ----
----    It contains all the information to make the remote call :  ----
----    arguments, results, exceptions, and how to send them on/   ----
----    reveive them from a giop.                                  ----
----    ( see proxyCall.h )                                        ----
----                                                               ----
----                                                               ----
----   authors : Sebastien Ponce, Fabien Azavant                   ----
----   date    : 02/28/99                                          ----
----                                                               ----
-----------------------------------------------------------------------
-----------------------------------------------------------------------


with Corba ;
with Giop_C ;

package omniOWProxyCallDesc is

   type Object is abstract tagged limited private ;
   -- type of an omniowProxyCallDesc object


   type Object_Ptr is access all Object ;
   -- type pointer on type Object


   function Aligned_Size(Self : in Object ;
                         Size_In: in Corba.Unsigned_Long )
                         return Corba.Unsigned_Long is abstract ;
   -- This function computes the size needed to marshall the arguments
   -- of the subprogram


   procedure Marshal_Arguments (Self : in Object ;
                                Giop_Client: in out Giop_C.Object ) is abstract ;
   -- marshalls the arguments of the subprogram into a Giop_C object


   function Operation (Self : in Object)
                       return Corba.String is abstract ;
   -- returns the name of the subprogram

private
   type Object is abstract tagged limited null record ;
   -- implementation of the private type Object

end omniOWproxyCallDesc ;

