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
----  This package corresponds to the C class IOP defined in file  ----
----  IOP.h and wrapped around in ada_Iop.hh. It provides the type ----
----  Tagged_Profile_List and some methods to marshall and         ----
----  unmarshall it.                                               ----
----                                                               ----
----                                                               ----
----   authors : Sebastien Ponce, Fabien Azavant                   ----
----   date    : 02/28/99                                          ----
----                                                               ----
-----------------------------------------------------------------------
-----------------------------------------------------------------------


package body Iop is

   -- Marshall2
   ------------
   procedure Marshall2 (A : in Iop.Tagged_Profile_List ;
                        S : in out Membufferedstream.Object'Class) ;
   pragma Import (CPP,Marshall2,"marshall__FPt25_CORBA_Unbounded_Sequence1ZQ23IOP13TaggedProfileR21Ada_memBufferedStream") ;
   -- wrapper around Ada_Iop method marshall
   -- (see Ada_Iop.h)
   -- name was changed to avoid conflict

   -- Marshall
   -----------
   procedure Marshall (A : in Iop.Tagged_Profile_List ;
                       S : in out Membufferedstream.Object'Class) is
   begin
      Marshall2 (A,S) ;
   end ;


   -- UnMarshall2
   --------------
   procedure UnMarshall2 (A : in out Iop.Tagged_Profile_List ;
                          S : in out Membufferedstream.Object'Class) ;
   pragma Import (CPP,UnMarshall2,"unmarshall__FRPt25_CORBA_Unbounded_Sequence1ZQ23IOP13TaggedProfileR21Ada_memBufferedStream") ;
   -- wrapper around Ada_Iop method unmarshall
   -- (see Ada_Iop.h)
   -- name was changed to avoid conflict


   -- UnMarshall
   -------------
   procedure UnMarshall (A : in out IOP.Tagged_Profile_List ;
                         S : in out Membufferedstream.Object'Class) is
   begin
      UnMarshall2 (A,S) ;
   end ;


   -- C_NP_AlignedSize
   -------------------
   function C_NP_AlignedSize (A : in IOP.Tagged_Profile_List ;
                              Initialoffset : in Interfaces.C.Unsigned_Long)
                              return Interfaces.C.Unsigned_Long ;
   pragma Import (CPP,C_NP_AlignedSize,"NP_alignedSize__FPt25_CORBA_Unbounded_Sequence1ZQ23IOP13TaggedProfileUi") ;
   -- wrapper around Ada_Iop method NP_AlignedSize
   -- (see Ada_Iop.h)
   -- called by the Ada equivalent : Align_Size


   -- Align_Size
   -------------
   function Align_Size (A : in IOP.Tagged_Profile_List ;
                        Initialoffset : in Corba.Unsigned_Long)
                        return Corba.Unsigned_Long is
      C_Initialoffset : Interfaces.C.Unsigned_Long ;
      C_Result : Interfaces.C.Unsigned_Long ;
   begin
      -- transforms the arguments in a C type ...
      C_Initialoffset := Interfaces.C.Unsigned_Long (Initialoffset) ;
      -- ... calls the C function ...
      C_Result := C_NP_alignedSize (A,C_Initialoffset) ;
      -- ... and transforms the result in an Ada type
      return  Corba.Unsigned_Long (C_Result) ;
   end ;


   -- C_Length
   -----------
   function C_Length (A : in IOP.Tagged_Profile_List)
                      return Interfaces.C.Unsigned_Long ;
   pragma Import (CPP,C_Length,"length__FPt25_CORBA_Unbounded_Sequence1ZQ23IOP13TaggedProfile") ;
   -- wrapper around Ada_Iop method length
   -- (see Ada_Iop.h)
   -- called by the Ada equivalent : Length


   -- Length
   ---------
   function Length (A : in IOP.Tagged_Profile_List)
                    return Corba.Unsigned_Long is
      C_Result : Interfaces.C.Unsigned_Long ;
   begin
      -- calls the C function ...
      C_Result := C_Length (A) ;
      -- ... and transforms the result in an Ada type
      return  Corba.Unsigned_Long (C_Result) ;
   end ;

end Iop ;
