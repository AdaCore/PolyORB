-----------------------------------------------------------------------
-----------------------------------------------------------------------
----                                                               ----
----                         AdaBroker                             ----
----                                                               ----
----                 package Omniproxycalldesc                     ----
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
----    interface, a descendant of this class has to be provided.  ----
----    It contains al the information to make the remote call :   ----
----    arguments, results, exceptions, and how to send them on/   ----
----    reveive them from a giop.                                  ----
----                                                               ----
----                                                               ----
----   authors : Sebastien Ponce, Fabien Azavant                   ----
----   date    : 02/28/99                                          ----
----                                                               ----
-----------------------------------------------------------------------
-----------------------------------------------------------------------


with Corba ;
with Giop_C ;

package omniProxyCallDesc is

   type Object is abstract tagged private ;
   -- type of an omniProxyCallDesc object


   type Object_Ptr is access all Object ;
   -- type pointer on type Object


   procedure Init (Self : in out Object ;
                   Has_Exceptions : Corba.Boolean := False ) ;
   -- Ses the boolean Pd_Has_User_Exception


   function Operation (Self : in Object)
                       return CORBA.String is abstract ;
   -- returns the name of the subprogram


   procedure Free(Self : in out Object) is abstract ;
   -- frees all the members of the omniProxyCallDesc


   function Aligned_Size(Self : in Object ;
                         Size_In: in Corba.Unsigned_Long )
                         return Corba.Unsigned_Long is abstract ;
   -- This function computes the size needed to marshall the arguments
   -- of the subprogram


   procedure Marshal_Arguments (Self : in Object ;
                                Giop_Client: in out Giop_C.Object ) is abstract ;
   -- marshalls the arguments of the subprogram into a Giop_C object


   procedure Unmarshal_Returned_Values (Self : in out Object ;
                                        Giop_Client: in Giop_C.Object ) is abstract ;
   -- unmarshalls the returned values of the subprogram from a Giop_C object


   procedure User_Exception (Self : in Object ;
                             Giop_Client : in Giop_C.Object ;
                             RepoId : in CORBA.String) ;
   -- must be overloaded by call descs which have exceptions


   function Has_User_Exceptions (Self : in Object)
                                 return CORBA.Boolean ;
   -- returns Pd_Has_User_Exception

private

   type Object is abstract tagged record
      Pd_Has_User_Exception : Corba.Boolean ;
   end record ;
   -- implementation of the private type Object

end omniproxyCallDesc ;

