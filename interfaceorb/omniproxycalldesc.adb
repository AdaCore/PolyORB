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
----    interface, which is not declared "one way", a descendant   ----
----    of this class has to be provided.                          ----
----    It contains al the information to make the remote call :   ----
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


with Ada.Exceptions ;

package body omniProxyCallDesc is

   -- Set_User_exceptions
   ----------------------
   procedure Set_User_exceptions (Self : in out Object ;
                                  Has_Exceptions : Corba.Boolean ) is
   begin
      Self.Pd_Has_User_Exception := Has_Exceptions ;
   end ;


   -- Align_Size
   -------------
   function Align_Size(Self : in Object ;
                       Size_In: in Corba.Unsigned_Long )
                       return Corba.Unsigned_Long is
   begin
      Ada.Exceptions.Raise_Exception(Corba.Adabroker_Fatal_Error'Identity,
                                     "omniProxyCallDesc.AlignSize should not be "
                                     & "called on a procedure or function with no arguments") ;
      return 0 ;
   end ;


   -- Marshall_Arguments
   ---------------------
   procedure Marshal_Arguments (Self : in Object ;
                                Giop_Client: in out Giop_C.Object ) is
   begin
      Ada.Exceptions.Raise_Exception(Corba.Adabroker_Fatal_Error'Identity,
                                     "omniProxyCallDesc.Marshal_Arguments should not be "
                                     & "called on a procedure or function with no arguments") ;
   end ;


   -- Unmarshall_Returned_Values
   -----------------------------
   procedure Unmarshal_Returned_Values (Self : in out Object ;
                                        Giop_Client: in out Giop_C.Object ) is
   begin
      Ada.Exceptions.Raise_Exception(Corba.Adabroker_Fatal_Error'Identity,
                                     "omniProxyCallDesc.Unmarshal_Returned_Values should not be "
                                     & "called on a procedure") ;
   end ;


   -- User_Exception
   -----------------
   procedure User_Exception (Self : in Object ;
                             Giop_Client : in out Giop_C.Object ;
                             RepoId : in CORBA.String) is
   begin
      if Self.Pd_Has_User_Exception then
         Ada.Exceptions.Raise_Exception(Corba.Adabroker_Fatal_Error'Identity,
                                        "Omniproxycalldesc.User_Exception"
                                        & Corba.CRLF
                                        & "Should not be called for a calldesc that can throw exceptions"
                                        & "This procedure should have been overloaded") ;
      else
         -- nothing to be done since we do not throw any exception
         null ;
      end if ;
   end ;


   -- Has_User_Exceptions
   ----------------------
   function Has_User_Exceptions (Self : in Object)
                                 return CORBA.Boolean is
   begin
      return Self.Pd_Has_User_Exception ;
   end ;

end omniproxyCallDesc ;



