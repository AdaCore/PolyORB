-----------------------------------------------------------------------
-----------------------------------------------------------------------
----                                                               ----
----                         AdaBroker                             ----
----                                                               ----
----                    package Corba.Orb                          ----
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
----   This package implements the ORB facilities, as              ----
----   specified in CORBA 2.0                                      ----
----                                                               ----
----                                                               ----
----   authors : Sebastien Ponce, Fabien Azavant                   ----
----   date    : 02/28/99                                          ----
----                                                               ----
-----------------------------------------------------------------------
-----------------------------------------------------------------------

with Ada.Exceptions ;
with Ada.Unchecked_Conversion ;
with Interfaces.C.Strings ;
with System ;
with System.Address_To_Access_Conversions ;

with Corba.Command_Line ;
with Omniobject ;
use type Omniobject.Object_Ptr ;
with Corba.Object ; use Corba.Object ;

with Adabroker_Debug ; use Adabroker_Debug ;

package body Corba.Orb is


   --------------------------------------------------
   ---        ORB initialization                 ----
   --------------------------------------------------

   function C_ORB_Init( Argc : in Interfaces.C.Int ;
                        Argv : in System.Address ;
                        Orbname : in Interfaces.C.Strings.Chars_Ptr)
                       return Object ;
    pragma Import(CPP, C_ORB_Init, "Ada_ORB_init__FiPPcPCc") ;
    -- wrapper around Ada_Orb_init
    -- see Ada_Corba_Orb.hh


   -- ORB_Init
   -----------
   function ORB_Init(Orb_Name : in Standard.String) return Object is
      C_Orb_Name : Interfaces.C.Strings.Chars_Ptr ;
      Result : Object ;
   begin

      pragma Debug(Output(Debug, "--- Corba.Orb.Orb_Init ---"))  ;
      pragma Debug(Output(Debug, Orb_Name))  ;

      C_Orb_Name := Interfaces.C.Strings.New_String(Orb_Name) ;
      -- deallocated 4 lines further

      pragma Debug(Output(Debug, "Corba.Orb.Orb_Init : calling CORBA::ORB_init"))  ;

      Result :=  C_Orb_Init(Corba.Command_Line.Argc,
                            Corba.Command_Line.Argv,
                            C_Orb_Name) ;

      pragma Debug(Output(Debug, "Corba.Orb.Orb_Init : ORB initialized !"))  ;

      Interfaces.C.Strings.Free(C_Orb_Name) ;
      return Result ;
   end ;


   -- C_BOA_Init
   -------------
   function C_BOA_Init(Self : in Object ;
                       Argc : in Interfaces.C.Int ;
                       Argv : in System.Address ;
                       Boaname : in Interfaces.C.Strings.Chars_Ptr)
                   return Corba.Boa.Object ;
   pragma Import(CPP,C_BOA_Init,"Ada_BOA_init__FPQ25CORBA3ORBiPPcPCc") ;
   -- calls Ada_Boa_Init
   -- see Ada_Corba_Orb.hh


   -- BOA_Init
   -----------
   function BOA_Init(Self : in Object ;
                     Boa_Name : in Standard.String)
                     return Corba.Boa.Object is
      C_Boa_Name : Interfaces.C.Strings.Chars_Ptr ;
      Result : Corba.Boa.Object ;
   begin

      C_Boa_Name := Interfaces.C.Strings.New_String(Boa_Name) ;
      -- deallocated 4 lines further

      Result :=  C_Boa_Init(Self,
                            Corba.Command_Line.Argc,
                            Corba.Command_Line.Argv,
                            C_Boa_Name) ;

      Interfaces.C.Strings.Free(C_Boa_Name) ;
      return Result ;
   end ;


end Corba.Orb ;
