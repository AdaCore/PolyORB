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
----  IOP.h. It provides the type Tagged_Profile_List and some     ----
----  methods to marshall and unmarshall it.                       ----
----                                                               ----
----                                                               ----
----   authors : Sebastien Ponce, Fabien Azavant                   ----
----   date    : 02/28/99                                          ----
----                                                               ----
-----------------------------------------------------------------------
-----------------------------------------------------------------------



package body Iop is

   -- C_Marshall
   -------------
   procedure C_Marshall (A : in System.Address ;
                         S : in out System.Address) ;
   pragma Import (C,C_Marshall,"__ars__Ct25_CORBA_Unbounded_Sequence1ZQ23IOP13TaggedProfileR17NetBufferedStream") ;
   -- wrapper around IOP::TaggedProfile operator >>=
   -- (see IOP.h)
   -- called by the Ada equivalent : Marshall


   -- Marshall
   -----------
   procedure Marshall (A : in Iop.Tagged_Profile_List ;
                       S : in out Netbufferedstream.Object'Class) is
      C_A : System.Address ;
      C_S : System.Address ;
   begin
      -- transforms the arguments in a C type ...
      C_A := System.Address (A) ;
      C_S := S'Address ;
      -- ... and calls the C procedure
      C_Marshall (C_A,C_S) ;
   end ;


   -- C_UnMarshall
   ---------------
   procedure C_UnMarshall (A : out System.Address ;
                           S : in out System.Address) ;
   pragma Import (C,C_UnMarshall,"__als__t25_CORBA_Unbounded_Sequence1ZQ23IOP13TaggedProfileR17NetBufferedStream") ;
   -- wrapper around IOP::TaggedProfile operator <<=
   -- (see IOP.h)
   -- called by the Ada equivalent : UnMarshall


   -- UnMarshall
   -------------
   procedure UnMarshall (A : out IOP.Tagged_Profile_List ;
                         S : in out Netbufferedstream.Object'Class) is
      C_A : System.Address ;
      C_S : System.Address ;
   begin
      -- transforms the arguments in a C type ...
      C_S := S'Address ;
      -- ... calls the C function ...
      C_UnMarshall (C_A,C_S) ;
      -- ... and transforms the result in an Ada type
      A := IOP.Tagged_Profile_List (C_A) ;
   end ;


   -- C_Marshall2
   --------------
   procedure C_Marshall2 (A : in System.Address ;
                         S : in out System.Address) ;
   pragma Import (C,C_Marshall2,"__ars__Ct25_CORBA_Unbounded_Sequence1ZQ23IOP13TaggedProfileR17MemBufferedStream") ;
   -- wrapper around IOP::TaggedProfile operator >>=
   -- (see IOP.h)
   -- name was changed to avoid conflict
   -- called by the Ada equivalent : Marshall


   -- Marshall
   -----------
   procedure Marshall (A : in Iop.Tagged_Profile_List ;
                       S : in out Membufferedstream.Object'Class) is
      C_A : System.Address ;
      C_S : System.Address ;
   begin
      -- transforms the arguments in a C type ...
      C_A := System.Address (A) ;
      C_S := S'Address ;
      -- ... and calls the C procedure
      C_Marshall2 (C_A,C_S) ;
   end ;


   -- C_UnMarshall2
   ----------------
   procedure C_UnMarshall2 (A : out System.Address ;
                           S : in out System.Address) ;
   pragma Import (C,C_UnMarshall2,"__als__t25_CORBA_Unbounded_Sequence1ZQ23IOP13TaggedProfileR17MemBufferedStream") ;
   -- wrapper around IOP::TaggedProfile operator <<=
   -- (see IOP.h)
   -- name was changed to avoid conflict
   -- called by the Ada equivalent : UnMarshall2


   -- UnMarshall
   -------------
   procedure UnMarshall (A : out IOP.Tagged_Profile_List ;
                         S : in out Membufferedstream.Object'Class) is
      C_A : System.Address ;
      C_S : System.Address ;
   begin
      -- transforms the arguments in a C type ...
      C_S := S'Address ;
      -- ... calls the C function ...
      C_UnMarshall2 (C_A,C_S) ;
      -- ... and transforms the result in an Ada type
      A := IOP.Tagged_Profile_List (C_A) ;
   end ;


   -- C_NP_AlignedSize
   -------------------
   function C_NP_AlignedSize (A : in System.Address ;
                              Initialoffset : in Interfaces.C.Unsigned_Long)
                              return Interfaces.C.Unsigned_Long ;
   pragma Import (C,C_NP_AlignedSize,"NP_alignedSize__Ct25_CORBA_Unbounded_Sequence1ZQ23IOP13TaggedProfileUi") ;
   -- wrapper around IOP::TaggedProfile function NP_alignedSize
   -- (see IOP.h)
   -- called by the Ada equivalent : Align_Size


   -- Align_Size
   -------------
   function Align_Size (A : in IOP.Tagged_Profile_List ;
                        Initialoffset : in Corba.Unsigned_Long)
                        return Corba.Unsigned_Long is
      C_A : System.Address ;
      C_Initialoffset : Interfaces.C.Unsigned_Long ;
      C_Result : Interfaces.C.Unsigned_Long ;
   begin
      -- transforms the arguments in a C type ...
      C_A := System.Address (A) ;
      C_Initialoffset := Interfaces.C.Unsigned_Long (Initialoffset) ;
      -- ... calls the C function ...
      C_Result := C_NP_alignedSize (C_A,C_Initialoffset) ;
      -- ... and transforms the result in an Ada type
      return  Corba.Unsigned_Long (C_Result) ;
   end ;


   -- C_Length
   -----------
   function C_Length (A : in System.Address)
                      return Interfaces.C.Unsigned_Long ;
   pragma Import (C,C_Length,"length__t15_CORBA_Sequence1ZQ23IOP13TaggedProfileUl") ;
   -- wrapper around IOP::TaggedProfile function length
   -- (see IOP.h)
   -- called by the Ada equivalent : Length


   -- Length
   ---------
   function Length (A : in IOP.Tagged_Profile_List)
                    return Corba.Unsigned_Long is
      C_A : System.Address ;
      C_Result : Interfaces.C.Unsigned_Long ;
   begin
      -- transforms the arguments in a C type ...
      C_A := System.Address (A) ;
      -- ... calls the C function ...
      C_Result := C_Length (C_A) ;
      -- ... and transforms the result in an Ada type
      return  Corba.Unsigned_Long (C_Result) ;
   end ;

end Iop ;
