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
----                                                               ----
----                                                               ----
----   authors : Sebastien Ponce, Fabien Azavant                   ----
----   date    : 02/28/99                                          ----
----                                                               ----
-----------------------------------------------------------------------
-----------------------------------------------------------------------


with Interfaces.C ; use Interfaces.C ;
with Ada.Text_IO ;

package body Sys_Dep is

   -- System_Dependant_Error
   -------------------------
   System_Dependant_Error : exception;
   -- this exception is raised if the configuration of the
   -- system is not suitable


   -- Boolean_C_To_Ada
   -------------------
   function Boolean_C_To_Ada (C_Bool : C_Boolean) return Boolean is
   begin
      return C_Bool /= C_False ;
      -- no use of C_True since every C_Boolean except 0 is true
      -- and not only C_True which value is 1
   end ;


   -- Boolean_Ada_To_C
   -------------------
   function Boolean_Ada_To_C (Bool : Boolean) return C_Boolean is
   begin
      if Bool
      then
         return C_True ;
      else
         return C_False ;
      end if ;
   end;


begin
   -- This code is executed when one declares a with clause
   -- for this library. It checks the system's configuration
   if Interfaces.C.UCHAR_MAX /= 255
   then
      Ada.Text_IO.Put_Line ("Error in package Sys_Dep :");
      Ada.Text_IO.Put_Line ("   The size of Ada char is not compatible with Corba.") ;
      Ada.Text_IO.Put_Line ("   Interfaces.C.UCHAR_MAX must be equal to 255.") ;
      raise System_Dependant_Error ;
      -- raise exception if the size of Interfaces.C.unsigned_char
      -- is not 8 bits. Needed to ensure compatibility between C++
      -- and Ada definitions of the Corba type Octet
   end if ;

end Sys_Dep ;





