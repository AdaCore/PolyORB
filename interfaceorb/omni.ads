-----------------------------------------------------------------------
-----------------------------------------------------------------------
----                                                               ----
----                         AdaBroker                             ----
----                                                               ----
----                       package Omni                            ----
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
----     This package is wrapped around the C class omni_C2Ada     ----
----   declared in omni_C2Ada.hh;                                  ----
----     It provides the 2 functions of omni_C2Ada and their       ----
----   equivalent in ADA.                                          ----
----                                                               ----
----                                                               ----
----   authors : Sebastien Ponce, Fabien Azavant                   ----
----   date    : 02/28/99                                          ----
----                                                               ----
-----------------------------------------------------------------------
-----------------------------------------------------------------------


with Interfaces.CPP ;
with Interfaces.C.Strings ;
with System ;

with OmniObject ;
with Corba ;

package Omni is

   type Alignment_T is new Integer;
   ALIGN_1 : constant Alignment_T := 1;
   ALIGN_2 : constant Alignment_T := 2;
   ALIGN_4 : constant Alignment_T := 4;
   ALIGN_8 : constant Alignment_T := 8;
   -- several kind of alignements for a list of Bytes

   function Align_To(Size : in Corba.Unsigned_Long ;
                     Align : in Alignment_T)
                     return Corba.Unsigned_Long ;
   -- this function increases size in order to align it with the
   -- alignement given in parameter.

end Omni ;
