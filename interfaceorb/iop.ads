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


with System ;
with Interfaces.C ;

with Netbufferedstream ;
with Membufferedstream ;
with Corba ;

package Iop is

   type Tagged_Profile_List is new System.Address ;
   -- corresponds to IOP::TaggedProfileList (see IOP.h)
   -- This object is never used in Ada (just taken from a C function
   -- and given to another one) so it is not right implemented.
   -- We just keep the system Address of the object.


   procedure Marshall (A : in Iop.Tagged_Profile_List ;
                       S : in out Netbufferedstream.Object'Class);
   pragma Import (CPP,Marshall,"marshall__FPt25_CORBA_Unbounded_Sequence1ZQ23IOP13TaggedProfileR17NetBufferedStream") ;
   -- wrapper around Ada_Iop method marshall
   -- (see Ada_Iop.h)
   -- Marshalls a Tagged_Profile_List into a Netbufferedstream


   procedure UnMarshall (A : out IOP.Tagged_Profile_List ;
                         S : in out Netbufferedstream.Object'Class);
   pragma Import (CPP,UnMarshall,"unmarshall__FPt25_CORBA_Unbounded_Sequence1ZQ23IOP13TaggedProfileR17NetBufferedStream") ;
   -- wrapper around Ada_Iop method marshall
   -- (see Ada_Iop.h)
   -- UnMarshalls a Tagged_Profile_List from a Netbufferedstream


   procedure Marshall (A : in Iop.Tagged_Profile_List ;
                       S : in out Membufferedstream.Object'Class);
   -- Marshalls a Tagged_Profile_List into a Membufferedstream


   procedure UnMarshall (A : out IOP.Tagged_Profile_List ;
                         S : in out Membufferedstream.Object'Class);
   -- UnMarshalls a Tagged_Profile_List from a Membufferedstream


   function Align_Size (A : in IOP.Tagged_Profile_List ;
                        Initialoffset : in Corba.Unsigned_Long)
                        return Corba.Unsigned_Long ;
   -- Computes the size needed to marshall a Tagged_Profile_List
   -- and add it to the Initialoffset


   function Length (A : in IOP.Tagged_Profile_List)
                    return Corba.Unsigned_Long ;
   -- Computes the length of a Tagged_Profile_List

end Iop ;
