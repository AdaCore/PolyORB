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
                       return System.Address ;
    pragma Import(CPP, C_ORB_Init, "Ada_ORB_init__FiPPcPCc") ;
    -- wrapper around Ada_Orb_init
    -- see Ada_Corba.hh


   -- ORB_Init
   -----------
   function ORB_Init(Orb_Name : in Standard.String) return Object_Ptr is
      C_Orb_Name : Interfaces.C.Strings.Chars_Ptr ;
      C_Result : System.Address ;
      C_Argc : Interfaces.C.Int := Corba.Command_Line.Argc ;
      C_Argv : System.Address := Corba.Command_Line.Argv ;
      package A2a is new System.Address_To_Access_Conversions(Object) ;
      function Conv is new Ada.Unchecked_Conversion(A2a.Object_Pointer, Object_Ptr);
   begin

      pragma Debug(Output(Debug, "--- Corba.Orb.Orb_Init ---"))  ;

      C_Orb_Name := Interfaces.C.Strings.New_String(Orb_Name) ;
      -- Never deallocated, but it may be used by the ORB
      -- and this function is called only once

      pragma Debug(Output(Debug, "Corba.Orb.Orb_Init : calling CORBA::ORB_init"))  ;

      C_Result :=  C_Orb_Init(C_Argc,
                              C_Argv,
                              C_Orb_Name) ;

      pragma Debug(Output(Debug, "Corba.Orb.Orb_Init : ORB initialized !"))  ;
      return Conv(A2a.To_Pointer(C_Result)) ;
   end ;


   -- C_BOA_Init
   -------------
   function C_BOA_Init(Self : in Object'Class ;
                       Argc : in Interfaces.C.Int ;
                       Argv : in System.Address ;
                       Boaname : in Interfaces.C.Strings.Chars_Ptr)
                   return System.Address ;
   pragma Import(CPP,C_BOA_Init,"BOA_init__Q25CORBA3ORBRiPPcPCc") ;
   -- calls Ada_BOA_init
   -- corresponds to
   -- CORBA::BOA_ptr
   -- CORBA::
   -- ORB::BOA_init(int &argc, char **argv, const char *boa_identifier)
   -- corbaBoa.cc L 180


   -- BOA_Init
   -----------
   function BOA_Init(Self : in Object_Ptr ;
                     Boa_Name : in Standard.String)
                     return Corba.Boa.Object_Ptr is
      C_Boa_Name : Interfaces.C.Strings.Chars_Ptr ;
      C_Result : System.Address ;
      package A2a is new System.Address_To_Access_Conversions(Corba.Boa.Object) ;
      function Conv is new Ada.Unchecked_Conversion(A2a.Object_Pointer,
                                                    Corba.Boa.Object_Ptr);
   begin

      C_Boa_Name := Interfaces.C.Strings.New_String(Boa_Name) ;
      -- Never deallocated, but it may be used by the ORB
      -- and this function is called only once

      C_Result :=  C_Boa_Init(Self.all,
                        Corba.Command_Line.Argc,
                        Corba.Command_Line.Argv,
                        C_Boa_Name) ;
      return Conv(A2a.To_Pointer(C_Result)) ;
   end ;


end Corba.Orb ;
