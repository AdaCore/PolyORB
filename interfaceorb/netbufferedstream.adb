-----------------------------------------------------------------------
-----------------------------------------------------------------------
----                                                               ----
----                         AdaBroker                             ----
----                                                               ----
----                 package netbufferedstream                     ----
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
----   is Ada_netBufferedStream. (see Ada_netBufferedStream.hh)    ----
----     It provides two types of methods : the C functions        ----
----   of the Ada_netBufferedStream class and their equivalent     ----
----   in Ada. (he first ones have a C_ prefix.)                   ----
----     In addition, there is a raise_ada_exception function      ----
----   that allows C functions to raise the ada No_Initialisation  ----
----   exception.                                                  ----
----     At last, there is only one Init procedure in place of     ----
----   two in Ada_netBufferedStream since the second one is        ----
----   useless for AdaBroker.                                      ----
----                                                               ----
----                                                               ----
----   authors : Sebastien Ponce, Fabien Azavant                   ----
----   date    : 02/28/99                                          ----
----                                                               ----
-----------------------------------------------------------------------
-----------------------------------------------------------------------


with Ada.Exceptions ;
with Ada.Strings.Unbounded ;
use type Ada.Strings.Unbounded.Unbounded_String ;
with Ada.Strings ;
with Ada.Characters.Latin_1 ;
with Interfaces.C ;

with Corba ;
use type Corba.String ;
use type Corba.Unsigned_Long ;
with Omni ;

with Adabroker_Debug ;
use Adabroker_Debug ;

package body NetBufferedStream is

   -- C_Init
   ---------
   procedure C_Init (Self : in Object'Class ;
                     r : in System.Address ;
                     RdLock : in Sys_Dep.C_Boolean ;
                     WrLock : in Sys_Dep.C_Boolean ;
                     Bufsize : in Interfaces.C.Unsigned_Long) ;
   pragma Import (CPP,C_Init,"__17NetBufferedStreamP4RopebT2Ui") ;
   -- wrapper around Ada_netBufferedStream function Init
   -- (see Ada_netBufferedStream.hh)
   -- name was changed to avoid conflict
   -- called by the Ada equivalent : Init


   -- Init
   -------
   procedure Init (Self : in Object'Class ;
                   R : in Rope.Object ;
                   Rdlock : in Corba.Boolean ;
                   WrLock : in Corba.Boolean ;
                   Bufsize : in Corba.Unsigned_Long) is
      C_RdLock : Sys_Dep.C_Boolean ;
      C_WrLock : Sys_Dep.C_Boolean ;
      C_Bufsize : Interfaces.C.Unsigned_Long ;
   begin
      -- transforms the arguments into a C type ...
      C_RdLock := Sys_Dep.Boolean_Ada_To_C (RdLock) ;
      C_WrLock := Sys_Dep.Boolean_Ada_To_C (WrLock) ;
      C_Bufsize := Interfaces.C.Unsigned_Long (Bufsize) ;
      -- ... and calls the C procedure
      C_Init (Self, System.Address (R), C_RdLock, C_WrLock, C_Bufsize) ;
   end ;


   -- C_Marshall_1
   ---------------
   procedure C_Marshall_1 (A : in Interfaces.C.Char ;
                           S : in out Object'Class) ;
   pragma Import (CPP,C_Marshall_1,
                  "marshall__21Ada_netBufferedStreamUcR21Ada_netBufferedStream") ;
   -- wrapper around Ada_netBufferedStream function marshall
   -- (see Ada_netBufferedStream.hh)
   -- name was changed to avoid conflict
   -- called by the Ada equivalent : Marshall


   -- Marshall
   -----------
   procedure Marshall (A : in Corba.Char ;
                       S : in out Object'Class) is
      C_A : Interfaces.C.Char ;
   begin
      -- transforms the arguments in a C type ...
      C_A := Interfaces.C.Char(A) ;
      -- ... and calls the C procedure
      C_Marshall_1 (C_A,S) ;
   end;


   -- C_UnMarshall_1
   -----------------
   procedure C_UnMarshall_1 (A : out Interfaces.C.Char ;
                             S : in out Object'Class) ;
   pragma Import (CPP,C_UnMarshall_1,"unmarshall__21Ada_netBufferedStreamRUcR21Ada_netBufferedStream") ;
   -- wrapper around Ada_netBufferedStream function marshall
   -- (see Ada_netBufferedStream.hh)
   -- name was changed to avoid conflict
   -- called by the Ada equivalent : UnMarshall


   -- UnMarshall
   -------------
   procedure UnMarshall (A : out Corba.Char ;
                         S : in out Object'Class) is
      C_A : Interfaces.C.Char ;
   begin
      C_UnMarshall_1 (C_A,S) ;
      A := Corba.Char(C_A) ;
   end;


   -- Align_Size
   -------------
   function Align_Size (A : in Corba.Char ;
                        Initial_Offset : in Corba.Unsigned_Long ;
                        N : in Corba.Unsigned_Long := 1)
                        return Corba.Unsigned_Long is
   begin
      -- no alignment needed here
      return Initial_Offset + N ;
   end ;


   -- C_Marshall_2
   ---------------
   procedure C_Marshall_2 (A : in Sys_Dep.C_Boolean ;
                           S : in out Object'Class) ;
   pragma Import (CPP,C_Marshall_2,"marshall__21Ada_netBufferedStreambR21Ada_netBufferedStream") ;
   -- wrapper around Ada_netBufferedStream function marshall
   -- (see Ada_netBufferedStream.hh)
   -- name was changed to avoid conflict
   -- called by the Ada equivalent : Marshall


   -- Marshall
   -----------
   procedure Marshall (A : in Corba.Boolean ;
                       S : in out Object'Class) is
      C_A : Sys_Dep.C_Boolean ;
   begin
      -- transforms the arguments in a C type ...
      C_A := Sys_Dep.Boolean_Ada_To_C (A) ;
      -- ... and calls the C procedure
      C_Marshall_2 (C_A,S) ;
   end;


   -- C_UnMarshall_2
   -----------------
   procedure C_UnMarshall_2 (A : out Sys_Dep.C_Boolean ;
                             S : in out Object'Class) ;
   pragma Import (CPP,C_UnMarshall_2,"unmarshall__21Ada_netBufferedStreamRbR21Ada_netBufferedStream") ;
   -- wrapper around Ada_netBufferedStream function marshall
   -- (see Ada_netBufferedStream.hh)
   -- name was changed to avoid conflict
   -- called by the Ada equivalent : UnMarshall


   -- UnMarshall
   -------------
   procedure UnMarshall (A : out Corba.Boolean ;
                         S : in out Object'Class) is
      C_A : Sys_Dep.C_boolean ;
   begin
      C_UnMarshall_2 (C_A,S) ;
      A := Sys_Dep.Boolean_C_To_Ada(C_A) ;
   end;


   -- Align_Size
   -------------
   function Align_Size (A : in Corba.Boolean ;
                        Initial_Offset : in Corba.Unsigned_Long ;
                        N : in Corba.Unsigned_Long := 1)
                        return Corba.Unsigned_Long is
   begin
      -- no alignment needed here
      return Initial_Offset + N ;
      -- Boolean is marshalled as an unsigned_char
   end ;


   -- C_Marshall_3
   ---------------
   procedure C_Marshall_3 (A : in Interfaces.C.Short ;
                           S : in out Object'Class) ;
   pragma Import (CPP,C_Marshall_3,"marshall__21Ada_netBufferedStreamsR21Ada_netBufferedStream") ;
   -- wrapper around Ada_netBufferedStream function marshall
   -- (see Ada_netBufferedStream.hh)
   -- name was changed to avoid conflict
   -- called by the Ada equivalent : Marshall


   -- Marshall
   -----------
   procedure Marshall (A : in Corba.Short ;
                       S : in out Object'Class) is
      C_A : Interfaces.C.Short ;
   begin
      -- transforms the arguments in a C type ...
      pragma Debug(Output(Debug,"Netbufferedstream : marshalling Short "
                          & Corba.Short'Image(A))) ;
      C_A := Interfaces.C.Short(A) ;
      -- ... and calls the C procedure
      pragma Debug(Output(Debug,"Netbufferedstream : marshalling Short "
                          & Interfaces.C.Short'Image(C_A))) ;
      C_Marshall_3 (C_A,S) ;
   end;


   -- C_UnMarshall_3
   -----------------
   procedure C_UnMarshall_3 (A : out Interfaces.C.Short ;
                             S : in out Object'Class) ;
   pragma Import (CPP,C_UnMarshall_3,"unmarshall__21Ada_netBufferedStreamRsR21Ada_netBufferedStream") ;
   -- wrapper around Ada_netBufferedStream function marshall
   -- (see Ada_netBufferedStream.hh)
   -- name was changed to avoid conflict
   -- called by the Ada equivalent : UnMarshall


   -- UnMarshall
   -------------
   procedure UnMarshall (A : out Corba.Short ;
                         S : in out Object'Class) is
      C_A : Interfaces.C.Short ;
   begin
      C_UnMarshall_3 (C_A,S) ;
      pragma Debug(Output(Debug,"Netbufferedstream : unmarshalling Short "
                          & Interfaces.C.Short'Image(C_A))) ;
      A := Corba.Short(C_A) ;
      pragma Debug(Output(Debug,"Netbufferedstream : unmarshalling Short "
                          & Corba.Short'Image(A))) ;
   end;


   -- Align_Size
   -------------
   function Align_Size (A : in Corba.Short ;
                        Initial_Offset : in Corba.Unsigned_Long ;
                        N : in Corba.Unsigned_Long := 1)
                        return Corba.Unsigned_Long is
      Tmp : Corba.Unsigned_Long ;
   begin
      Tmp := Omni.Align_To (Initial_Offset,Omni.ALIGN_2) ;
      return Tmp + (2 * N) ;
   end ;


   -- C_Marshall_4
   ---------------
   procedure C_Marshall_4 (A : in Interfaces.C.Unsigned_Short ;
                           S : in out Object'Class) ;
   pragma Import (CPP,C_Marshall_4,"marshall__21Ada_netBufferedStreamUsR21Ada_netBufferedStream") ;
   -- wrapper around Ada_netBufferedStream function marshall
   -- (see Ada_netBufferedStream.hh)
   -- name was changed to avoid conflict
   -- called by the Ada equivalent : Marshall


   -- Marshall
   -----------
   procedure Marshall (A : in Corba.Unsigned_Short ;
                       S : in out Object'Class) is
      C_A : Interfaces.C.Unsigned_Short ;
   begin
      -- transforms the arguments in a C type ...
      C_A := Interfaces.C.Unsigned_Short (A) ;
      -- ... and calls the C procedure
      C_Marshall_4 (C_A,S) ;
   end;


   -- C_UnMarshall_4
   -----------------
   procedure C_UnMarshall_4 (A : out Interfaces.C.Unsigned_Short ;
                             S : in out Object'Class) ;
   pragma Import (CPP,C_UnMarshall_4,"unmarshall__21Ada_netBufferedStreamRUsR21Ada_netBufferedStream") ;
   -- wrapper around Ada_netBufferedStream function marshall
   -- (see Ada_netBufferedStream.hh)
   -- name was changed to avoid conflict
   -- called by the Ada equivalent : UnMarshall


   -- UnMarshall
   -------------
   procedure UnMarshall (A : out Corba.Unsigned_Short ;
                         S : in out Object'Class) is
      C_A : Interfaces.C.Unsigned_Short ;
   begin
      C_UnMarshall_4 (C_A,S) ;
      A := Corba.Unsigned_Short(C_A) ;
   end;


   -- Align_Size
   -------------
   function Align_Size (A : in Corba.Unsigned_Short ;
                        Initial_Offset : in Corba.Unsigned_Long ;
                        N : in Corba.Unsigned_Long := 1)
                        return Corba.Unsigned_Long is
      Tmp : Corba.Unsigned_Long ;
   begin
      Tmp := Omni.Align_To (Initial_Offset,Omni.ALIGN_2) ;
      return Tmp + (2 * N) ;
   end ;




   -- C_Marshall_5
   ---------------
   procedure C_Marshall_5 (A : in Interfaces.C.Long ;
                           S : in out Object'Class) ;
   pragma Import (CPP,C_Marshall_5,"marshall__21Ada_netBufferedStreamlR21Ada_netBufferedStream") ;
   -- wrapper around Ada_netBufferedStream function marshall
   -- (see Ada_netBufferedStream.hh)
   -- name was changed to avoid conflict
   -- called by the Ada equivalent : Marshall


   -- Marshall
   -----------
   procedure Marshall (A : in Corba.Long ;
                       S : in out Object'Class) is
      C_A : Interfaces.C.Long ;
   begin
      -- transforms the arguments in a C type ...
      C_A := Interfaces.C.Long(A) ;
      -- ... and calls the C procedure
      C_Marshall_5 (C_A,S) ;
   end;


   -- C_UnMarshall_5
   -----------------
   procedure C_UnMarshall_5 (A : out Interfaces.C.Long ;
                             S : in out Object'Class) ;
   pragma Import (CPP,C_UnMarshall_5,"unmarshall__21Ada_netBufferedStreamRlR21Ada_netBufferedStream") ;
   -- wrapper around Ada_netBufferedStream function marshall
   -- (see Ada_netBufferedStream.hh)
   -- name was changed to avoid conflict
   -- called by the Ada equivalent : UnMarshall


   -- UnMarshall
   -------------
   procedure UnMarshall (A : out Corba.Long ;
                         S : in out Object'Class) is
      C_A : Interfaces.C.Long ;
   begin
      C_UnMarshall_5 (C_A,S) ;
      A := Corba.Long(C_A) ;
   end;


   -- Align_Size
   -------------
   function Align_Size (A : in Corba.Long ;
                        Initial_Offset : in Corba.Unsigned_Long ;
                        N : in Corba.Unsigned_Long := 1)
                        return Corba.Unsigned_Long is
      Tmp : Corba.Unsigned_Long ;
   begin
      Tmp := Omni.Align_To (Initial_Offset,Omni.ALIGN_4) ;
      return Tmp + (4 * N) ;
   end ;


   -- C_Marshall_6
   ---------------
   procedure C_Marshall_6 (A : in Interfaces.C.Unsigned_Long ;
                           S : in out Object'Class) ;
   pragma Import (CPP,C_Marshall_6,"marshall__21Ada_netBufferedStreamUlR21Ada_netBufferedStream") ;
   -- wrapper around Ada_netBufferedStream function marshall
   -- (see Ada_netBufferedStream.hh)
   -- name was changed to avoid conflict
   -- called by the Ada equivalent : Marshall


   -- Marshall
   -----------
   procedure Marshall (A : in Corba.Unsigned_Long ;
                       S : in out Object'Class) is
      C_A : Interfaces.C.Unsigned_Long ;
   begin
      pragma Debug(Output(Debug,"Netbufferedstream : marshalling Unsigned Long")) ;
      -- transforms the arguments in a C type ...
      C_A := Interfaces.C.Unsigned_Long (A) ;
      -- ... and calls the C procedure
      C_Marshall_6 (C_A,S) ;
   end;


   -- C_UnMarshall_6
   -----------------
   procedure C_UnMarshall_6 (A : out Interfaces.C.Unsigned_Long ;
                             S : in out Object'Class) ;
   pragma Import (CPP,C_UnMarshall_6,"unmarshall__21Ada_netBufferedStreamRUlR21Ada_netBufferedStream") ;
   -- wrapper around Ada_netBufferedStream function marshall
   -- (see Ada_netBufferedStream.hh)
   -- name was changed to avoid conflict
   -- called by the Ada equivalent : UnMarshall


   -- UnMarshall
   -------------
   procedure UnMarshall (A : out Corba.Unsigned_Long ;
                         S : in out Object'Class) is
      C_A : Interfaces.C.Unsigned_Long ;
   begin
      pragma Debug(Output(Debug,"Netbufferedstream : unmarshalling Unsigned Long")) ;
      C_UnMarshall_6 (C_A,S) ;
      A := Corba.Unsigned_Long(C_A) ;
   end;


   -- Align_Size
   -------------
   function Align_Size (A : in Corba.Unsigned_Long ;
                        Initial_Offset : in Corba.Unsigned_Long ;
                        N : in Corba.Unsigned_Long := 1)
                        return Corba.Unsigned_Long is
      Tmp : Corba.Unsigned_Long ;
   begin
      Tmp := Omni.Align_To (Initial_Offset,Omni.ALIGN_4) ;
      return Tmp + (4 * N) ;
   end ;


   -- C_Marshall_7
   ---------------
   procedure C_Marshall_7 (A : in Interfaces.C.C_Float ;
                           S : in out Object'Class) ;
   pragma Import (CPP,C_Marshall_7,"marshall__21Ada_netBufferedStreamfR21Ada_netBufferedStream") ;
   -- wrapper around Ada_netBufferedStream function marshall
   -- (see Ada_netBufferedStream.hh)
   -- name was changed to avoid conflict
   -- called by the Ada equivalent : Marshall


   -- Marshall
   -----------
   procedure Marshall (A : in Corba.Float ;
                       S : in out Object'Class) is
      C_A : Interfaces.C.C_Float ;
   begin
      -- transforms the arguments in a C type ...
      C_A := Interfaces.C.C_Float (A) ;
      -- ... and calls the C procedure
      C_Marshall_7 (C_A,S) ;
   end;


   -- C_UnMarshall_7
   -----------------
   procedure C_UnMarshall_7 (A : out Interfaces.C.C_Float ;
                             S : in out Object'Class) ;
   pragma Import (CPP,C_UnMarshall_7,"unmarshall__21Ada_netBufferedStreamRfR21Ada_netBufferedStream") ;
   -- wrapper around Ada_netBufferedStream function marshall
   -- (see Ada_netBufferedStream.hh)
   -- name was changed to avoid conflict
   -- called by the Ada equivalent : UnMarshall


   -- UnMarshall
   -------------
   procedure UnMarshall (A : out Corba.Float ;
                         S : in out Object'Class) is
      C_A : Interfaces.C.C_Float ;
   begin
      C_UnMarshall_7 (C_A,S) ;
      A := Corba.Float(C_A) ;
   end;


   -- Align_Size
   -------------
   function Align_Size (A : in Corba.Float ;
                        Initial_Offset : in Corba.Unsigned_Long ;
                        N : in Corba.Unsigned_Long := 1)
                        return Corba.Unsigned_Long is
      Tmp : Corba.Unsigned_Long ;
   begin
      Tmp := Omni.Align_To (Initial_Offset,Omni.ALIGN_4) ;
      return Tmp + (4 * N) ;
   end ;


   -- C_Marshall_8
   ---------------
   procedure C_Marshall_8 (A : in Interfaces.C.Double ;
                           S : in out Object'Class) ;
   pragma Import (CPP,C_Marshall_8,"marshall__21Ada_netBufferedStreamdR21Ada_netBufferedStream") ;
   -- wrapper around Ada_netBufferedStream function marshall
   -- (see Ada_netBufferedStream.hh)
   -- name was changed to avoid conflict
   -- called by the Ada equivalent : Marshall


   -- Marshall
   -----------
   procedure Marshall (A : in Corba.Double ;
                       S : in out Object'Class) is
      C_A : Interfaces.C.Double ;
   begin
      -- transforms the arguments in a C type ...
      C_A := Interfaces.C.Double(A) ;
      -- ... and calls the C procedure
      C_Marshall_8 (C_A,S) ;
   end;


   -- C_UnMarshall_8
   -----------------
   procedure C_UnMarshall_8 (A : out Interfaces.C.Double ;
                             S : in out Object'Class) ;
   pragma Import (CPP,C_UnMarshall_8,"unmarshall__21Ada_netBufferedStreamRdR21Ada_netBufferedStream") ;
   -- wrapper around Ada_netBufferedStream function marshall
   -- (see Ada_netBufferedStream.hh)
   -- name was changed to avoid conflict
   -- called by the Ada equivalent : UnMarshall


   -- UnMarshall
   -------------
   procedure UnMarshall (A : out Corba.Double ;
                         S : in out Object'Class) is
      C_A : Interfaces.C.Double ;
   begin
      C_UnMarshall_8 (C_A,S) ;
      A := Corba.Double(C_A) ;
   end;


   -- Align_Size
   -------------
   function Align_Size (A : in Corba.Double ;
                        Initial_Offset : in Corba.Unsigned_Long ;
                        N : in Corba.Unsigned_Long := 1)
                        return Corba.Unsigned_Long is
      Tmp : Corba.Unsigned_Long ;
   begin
      Tmp := Omni.Align_To (Initial_Offset,Omni.ALIGN_8) ;
      return Tmp + (8 * N) ;
   end ;


   -- C_Marshall_9
   ---------------
   procedure C_Marshall_9 (A : in Corba.Octet ;
                           S : in out Object'Class) ;
   pragma Import (CPP,C_Marshall_9,"marshall__21Ada_netBufferedStreamUcR21Ada_netBufferedStream") ;
   -- wrapper around Ada_netBufferedStream function marshall
   -- FOR CHAR TYPE BECAUSE IT IS THE SAME TYPE IN C++
   -- (see Ada_netBufferedStream.hh)
   -- name was changed to avoid conflict
   -- called by the Ada equivalent : Marshall


   -- Marshall
   -----------
   procedure Marshall (A : in Corba.Octet ;
                       S : in out Object'Class) is
   begin
      C_Marshall_9 (A,S) ;
   end;


   -- C_UnMarshall_9
   -----------------
   procedure C_UnMarshall_9 (A : out Corba.Octet ;
                             S : in out Object'Class) ;
   pragma Import (CPP,C_UnMarshall_9,"unmarshall__21Ada_netBufferedStreamRUcR21Ada_netBufferedStream") ;
   -- wrapper around Ada_netBufferedStream function marshall
   -- FOR CHAR TYPE BECAUSE IT IS THE SAME TYPE IN C++
   -- (see Ada_netBufferedStream.hh)
   -- name was changed to avoid conflict
   -- called by the Ada equivalent : UnMarshall


   -- UnMarshall
   -------------
   procedure UnMarshall (A : out Corba.Octet ;
                         S : in out Object'Class) is
   begin
      C_UnMarshall_9 (A,S) ;
   end;


   -- Align_Size
   -------------
   function Align_Size (A : in Corba.Octet ;
                        Initial_Offset : in Corba.Unsigned_Long ;
                        N : in Corba.Unsigned_Long := 1)
                        return Corba.Unsigned_Long is
   begin
      return Initial_Offset + N ;
   end ;


   -- Marshall
   -----------
   procedure Marshall (A : in Corba.String ;
                       S : in out Object'Class) is
      Size : Corba.Unsigned_Long ;
      C : Standard.Character ;
   begin
      -- first marshall the size of the string + 1
      -- 1 is the size of the null character we must marshall
      -- at the end of the string (C style)
      Size := Corba.Length (A) + Corba.Unsigned_Long (1) ;

      Marshall (Size , S) ;
      -- Then marshall the string itself and a null character at the end

      for I in 1..Integer(Size)-1 loop
         C := Ada.Strings.Unbounded.Element (Ada.Strings.Unbounded.Unbounded_String (A),I) ;
         Marshall (C,S) ;
      end loop ;
      Marshall (Corba.Char (Ada.Characters.Latin_1.nul),S) ;
   end ;


   -- UnMarshall
   -------------
   procedure UnMarshall (A : out Corba.String ;
                         S : in out Object'Class) is
      Size : Corba.Unsigned_Long ;
      C : Standard.Character ;
   begin
      -- first unmarshalls the size of the string
      UnMarshall (Size,S) ;
      case Size is
         when 0 =>
            -- the size is never 0 so raise exception if it is the case
            Ada.Exceptions.Raise_Exception(Corba.Adabroker_Fatal_Error'Identity,
                                           "Size of the string was 0 in netbufferedstream.UnMarshall.") ;
         when 1 =>
            -- if the size is 1 then the String is empty
            A := Corba.String (Ada.Strings.Unbounded.To_Unbounded_String ("")) ;
         when others =>
            -- else we can unmarshall the string
            declare
               Tmp : String (1..Integer(Size)-1) ;
            begin
               for I in 1..Integer(Size)-1 loop
                  UnMarshall (Tmp(I),S);
               end loop ;
               A := Corba.String (Ada.Strings.Unbounded.To_Unbounded_String (Tmp)) ;
            end ;
      end case ;
      -- unmarshall the null character at the end of the string (C style)
      -- and verify it is null
      UnMarshall (C,S) ;
      if C /= Ada.Characters.Latin_1.Nul then
         Ada.Exceptions.Raise_Exception(Corba.Adabroker_Fatal_Error'Identity,
                                        "Size not ended by null character in netbufferedstream.UnMarshall.") ;
      end if ;
   end ;


   -- Align_Size
   -------------
   function Align_Size (A : in Corba.String ;
                        Initial_Offset : in Corba.Unsigned_Long ;
                        N : in Corba.Unsigned_Long := 1)
                        return Corba.Unsigned_Long is
      Tmp : Corba.Unsigned_Long ;
   begin
      -- alignment
      Tmp := Omni.Align_To (Initial_Offset,Omni.ALIGN_4) ;
      -- size of the size of the string
      Tmp := Tmp + 4 ;
      -- size of the string itself
      return Tmp + (N * (Corba.Length (A) + 1)) ;
      -- + 1 is for the null character (the strings are marshalled in C style)
   end ;


   -- Marshall
   -----------
   procedure Marshall (A : in Corba.Completion_Status ;
                       S : in out Object'Class) is
   begin
      -- maps the possible values on the firste shorts
      -- and marshall the right one
      case A is
         when Corba.Completed_Yes =>
            Marshall (Corba.Unsigned_Long (1),S) ;
         when Corba.Completed_No =>
            Marshall (Corba.Unsigned_Long (2),S) ;
         when Corba.Completed_Maybe =>
            Marshall (Corba.Unsigned_Long (3),S) ;
      end case ;
   end ;


   -- UnMarshall
   -------------
   procedure UnMarshall (A : out Corba.Completion_Status ;
                         S : in out Object'Class) is
      Tmp : Corba.Unsigned_Long ;
   begin
      -- unmarshalls an unsigned short
      UnMarshall (Tmp,S) ;
      -- and returns the corresponding Completion_Status
      case Tmp is
         when 1 =>
            A := Corba.Completed_Yes ;
         when 2 =>
            A := Corba.Completed_No ;
         when 3 =>
            A := Corba.Completed_Maybe ;
         when others =>
            Ada.Exceptions.Raise_Exception (Corba.AdaBroker_Fatal_Error'Identity,
                                            "Expected Completion_Status in netbufferedstream.UnMarshall" & Corba.CRLF &
                                            "Short out of range" & Corba.CRLF &
                                            "(see netbufferedstream L660)");
      end case ;
   end ;


   -- Align_Size
   -------------
   function Align_Size (A : in Corba.Completion_Status ;
                        Initial_Offset : in Corba.Unsigned_Long ;
                        N : in Corba.Unsigned_Long := 1)
                        return Corba.Unsigned_Long is
      Tmp : Corba.Unsigned_Long ;
   begin
      Tmp := Omni.Align_To (Initial_Offset,Omni.ALIGN_4) ;
      return Tmp + (4 * N) ;
      -- a Completion_Status is marshalled as an unsigned_long
   end ;


   -- Marshall
   -----------
   procedure Marshall (A : in Corba.Ex_Body'Class ;
                       S : in out Object'Class) is
   begin
      -- just marshall each field
      Marshall (A.Minor,S) ;
      Marshall (A.Completed,S) ;
   end;


   -- UnMarshall
   -------------
   procedure UnMarshall (A : out Corba.Ex_Body'Class ;
                         S : in out Object'Class) is
      Minor : Corba.Unsigned_Long ;
      Completed : Corba.Completion_Status ;
   begin
      -- Unmarshalls the two fields
      UnMarshall (Completed,S) ;
      UnMarshall (Minor,S) ;
      -- and return the object
      A.Minor := Minor ;
      A.Completed := Completed ;
   end;


   -- Align_Size
   -------------
   function Align_Size (A : in Corba.Ex_Body'Class ;
                        Initial_Offset : in Corba.Unsigned_Long ;
                        N : in Corba.Unsigned_Long := 1)
                        return Corba.Unsigned_Long is
      Tmp : Corba.Unsigned_Long ;
   begin
      Tmp := Omni.Align_To (Initial_Offset,Omni.ALIGN_4) ;
      return Tmp + (8 * N) ;
      -- an Ex_body has two fields : an unsigned_long -> 4 bytes
      --                             and a Completion_Status -> 4 bytes
   end ;



   -- C_Is_Reusing_Existing_Connection
   -----------------------------------
   function C_Is_Reusing_Existing_Connection (Self : in Object'Class)
                                              return Sys_Dep.C_Boolean;
   pragma Import (CPP,C_Is_Reusing_Existing_Connection,"isReUsingExistingConnection__21Ada_netBufferedStream") ;
   -- wrapper around     _CORBA_Boolean isReUsingExistingConnection() const;
   -- (see rope.h L 395)
   -- called by the Ada equivalent : Is_Reusing_Existing_Connection


   -- Is_Reusing_Existing_Connection
   ---------------------------------
   function Is_Reusing_Existing_Connection (Self : in Object'Class)
                                            return CORBA.Boolean is
      C_Result : Sys_Dep.C_Boolean ;
   begin
      -- calls the C function ...
      C_Result := C_Is_Reusing_Existing_Connection (Self) ;
      -- ...and transforms the result into an Ada type
      return Sys_Dep.Boolean_C_To_Ada (C_Result) ;
   end ;

end NetBufferedStream ;
