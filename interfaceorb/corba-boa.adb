-----------------------------------------------------------------------
-----------------------------------------------------------------------
----                                                               ----
----                         AdaBroker                             ----
----                                                               ----
----                    package Corba.Boa                          ----
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
----     This package is wrapped around a C++ class whose name     ----
----   is BOA declared in file CORBA.h.                            ----
----     It provides two types of methods : the C functions        ----
----   of the BOA class and their equivalent in                    ----
----   Ada. (the first ones have a C_ prefix.)                     ----
----                                                               ----
----                                                               ----
----   authors : Sebastien Ponce, Fabien Azavant                   ----
----   date    : 02/28/99                                          ----
----                                                               ----
-----------------------------------------------------------------------
-----------------------------------------------------------------------


with Sys_Dep ;

package body Corba.Boa is


   -- C_Implementation_Is_Ready
   ----------------------------
   procedure C_Implementation_Is_Ready(Self : in Object ;
                                       ImplementationDef_ptr : in System.Address ;
                                       Non_Blocking : in Sys_Dep.C_Boolean );
   pragma Import(CPP, C_Implementation_Is_Ready, "impl_is_ready__Q25CORBA3BOAPQ25CORBA17ImplementationDefb") ;
   -- corresponds to CORBA::BOA::impl_is_ready
   -- see CORBA.h L1967


   -- Implementation_Is_Ready
   --------------------------
   procedure Implementation_Is_Ready(Self : in Object ;
                                     Non_Blocking : in Boolean := False )  is
      Tmp : System.Address := System.Null_Address ;
      NB : Sys_Dep.C_Boolean := Sys_Dep.Boolean_Ada_To_C(Non_Blocking) ;
   begin
      C_Implementation_Is_Ready(Self, Tmp, NB) ;
   end ;


   -- Object_Is_Ready
   -------------------
   procedure Object_Is_Ready(Self: in Object ;
                             Obj: in Omniobject.Implemented_Object'Class ) is
   begin
      -- it does not take the BOA into account because thereis only
      -- one BOA in omniORB2. ( See corbaBoa.cc )
      Omniobject.Object_Is_Ready(Obj) ;
   end ;

   -- Object_Is_Ready
   -------------------
   procedure Dispose_Object(Self: in Object ;
                            Obj: in Omniobject.Implemented_Object'Class ) is
   begin
      -- it does not take the BOA into account because thereis only
      -- one BOA in omniORB2. ( See corbaBoa.cc )
      Omniobject.Dispose_Object(Obj) ;
   end ;




end Corba.Boa ;






