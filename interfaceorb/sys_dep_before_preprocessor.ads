-----------------------------------------------------------------------
-----------------------------------------------------------------------
----                                                               ----
----                         AdaBroker                             ----
----                                                               ----
----                      package sys_dep                          ----
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
----   This package defines some system dependent types.           ----
----   The system dependence is mostly due to the C++              ----
----   interfacing.                                                ----
----   In fact, this file is not system dependant but his          ----
----   implementation (see sys_dep_before_preprocessor.adb).       ----
----                                                               ----
----                                                               ----
----   authors : Sebastien Ponce, Fabien Azavant                   ----
----   date    : 02/28/99                                          ----
----                                                               ----
-----------------------------------------------------------------------
-----------------------------------------------------------------------


with Interfaces.C ;

package Sys_Dep is

   type C_Boolean is range 0..1 ;

#if BOOLEAN_ON_1_BYTE
  for C_Boolean'Size use 1 ;
#elsif BOOLEAN_ON_4_BYTES
  for C_Boolean'Size use 4 ;
#else
  ::::::::::::::::::::::::::::::::::::::
  ::::::::::::::::::::::::::::::::::::::
  ::::::::::::::::::::::::::::::::::::::
  -- You cannot compile This package
  -- Beacause you have not specified
  -- the size of boolean types on your platform
  -- see sys_dep_definition_file.txt
  ::::::::::::::::::::::::::::::::::::::
  ::::::::::::::::::::::::::::::::::::::
  ::::::::::::::::::::::::::::::::::::::
#end if ;


   C_True : C_Boolean := C_Boolean(1) ;
   C_False : C_Boolean := C_Boolean(0) ;
   --    Definition of C_Boolean, the Ada equivalent of the C++
   -- bool type. Needed to interface C functions in Ada
   --    It is supposed here that the C internal representation of type
   -- boolean need one Bytes. If it is not the case, an error will occur
   -- when compiling and executing Ada_sys_Dep.cc and you will never come
   -- to this file.

   function Boolean_C_To_Ada (C_Bool : C_Boolean) return Boolean;
   function Boolean_Ada_To_C (Bool : Boolean) return C_Boolean;
   -- conversion of C_Boolean into Ada boolean and vice versa

end Sys_Dep ;



