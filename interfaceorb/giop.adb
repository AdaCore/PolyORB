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
----     This package is wrapped around the C++ class GIOP         ----
----   declared in GIOP.h.                                         ----
----     It provides some Ada equivalents of C++ types and the     ----
----   corresponding translation fonctions.                        ----
----                                                               ----
----                                                               ----
----   authors : Sebastien Ponce, Fabien Azavant                   ----
----   date    : 02/28/99                                          ----
----                                                               ----
-----------------------------------------------------------------------
-----------------------------------------------------------------------


with Ada.Unchecked_Conversion ;

package body Giop is

   -- Ada_To_C_Int
   ---------------
   function Ada_To_C_Int is
     new Ada.Unchecked_Conversion (Integer,
                                   Interfaces.C.Int) ;
   -- needed to change ada type Integer
   -- into C type Interfaces.C.Int


   -- Reply_Status_Type_To_C_Int
   -----------------------------
   function Reply_Status_Type_To_C_Int (Status : in Reply_Status_Type)
                                        return Interfaces.C.Int is
   begin
      return Ada_To_C_Int (Reply_Status_Type'Pos(Status)) ;
   end;


   -- C_Int_To_Reply_Status_Type
   -----------------------------
   function C_Int_To_Reply_Status_Type (N : in Interfaces.C.Int)
                                        return Reply_Status_Type is
   begin
      return Reply_Status_Type'Val(N) ;
   end;


end Giop ;
