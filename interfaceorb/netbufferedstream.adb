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


with Ada.Unchecked_Conversion ;
with Ada.Exceptions ;
with Ada.Strings.Unbounded ;
use type Ada.Strings.Unbounded.Unbounded_String ;
with Ada.Strings ;
with Ada.Characters.Latin_1 ;

with Corba ;
use type Corba.String ;
use type Corba.Unsigned_Long ;

package body NetBufferedStream is

   -- Ada_To_C_Unsigned_Long
   -------------------------
   function Ada_To_C_Unsigned_Long is
     new Ada.Unchecked_Conversion (Corba.Unsigned_Long,
                                   Interfaces.C.Unsigned_Long) ;
   -- needed to change ada type Corba.Unsigned_Long
   -- into C type Interfaces.C.Unsigned_Long


   -- C_Init
   ---------
   procedure C_Init (Self : in Object'Class ;
                     r : in System.Address ;
                     RdLock : in Sys_Dep.C_Boolean ;
                     WrLock : in Sys_Dep.C_Boolean ;
                     Bufsize : in Interfaces.C.Unsigned_Long) ;
   pragma Import (C,C_Init,"__17NetBufferedStreamP4RopebT2Ui") ;
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
      C_Bufsize := Ada_To_C_Unsigned_Long (Bufsize) ;
      -- ... and calls the C procedure
      C_Init (Self, System.Address (R), C_RdLock, C_WrLock, C_Bufsize) ;
   end ;


   -- Ada_To_C_Char
   ----------------
   function Ada_To_C_Char is
     new Ada.Unchecked_Conversion (Corba.Char,
                                   Interfaces.C.Char) ;
   -- needed to change ada type Corba.Char
   -- into C type Interfaces.C.Char


   -- C_Marshall
   -------------
   procedure C_Marshall_1 (A : in Interfaces.C.Char ;
                           S : in out System.Address) ;
   pragma Import (C,C_Marshall_1,"marshall__21Ada_netBufferedStreamUcR17NetBufferedStream") ;
   -- wrapper around Ada_netBufferedStream function marshall
   -- (see Ada_netBufferedStream.hh)
   -- name was changed to avoid conflict
   -- called by the Ada equivalent : Marshall


   -- Marshall
   -----------
   procedure Marshall (A : in Corba.Char ;
                       S : in out Object'Class) is
      C_A : Interfaces.C.Char ;
      C_S : System.Address ;
   begin
      -- transforms the arguments in a C type ...
      C_A := Ada_To_C_Char (A) ;
      C_S := S'Address ;
      -- ... and calls the C procedure
      C_Marshall_1 (C_A,C_S) ;
   end;


   -- C_UnMarshall_1
   -----------------
   procedure C_UnMarshall_1 (A : out System.Address ;
                             S : in out System.Address) ;
   pragma Import (C,C_UnMarshall_1,"unmarshall__21Ada_netBufferedStreamRUcR17NetBufferedStream") ;
   -- wrapper around Ada_netBufferedStream function marshall
   -- (see Ada_netBufferedStream.hh)
   -- name was changed to avoid conflict
   -- called by the Ada equivalent : UnMarshall


   -- UnMarshall
   -------------
   procedure UnMarshall (A : out Corba.Char ;
                         S : in out Object'Class) is
      C_A : System.Address ;
      C_S : System.Address ;
   begin
      -- transforms the arguments in a C type ...
      C_A := A'Address ;
      C_S := S'Address ;
      -- ... and calls the C procedure
      C_UnMarshall_1 (C_A,C_S) ;
   end;


   -- C_Marshall_2
   procedure C_Marshall_2 (A : in Sys_Dep.C_Boolean ;
                           S : in out System.Address) ;
   pragma Import (C,C_Marshall_2,"marshall__21Ada_netBufferedStreambR17NetBufferedStream") ;
   -- wrapper around Ada_netBufferedStream function marshall
   -- (see Ada_netBufferedStream.hh)
   -- name was changed to avoid conflict
   -- called by the Ada equivalent : Marshall


   -- Marshall
   -----------
   procedure Marshall (A : in Corba.Boolean ;
                       S : in out Object'Class) is
      C_A : Sys_Dep.C_Boolean ;
      C_S : System.Address ;
   begin
      -- transforms the arguments in a C type ...
      C_A := Sys_Dep.Boolean_Ada_To_C (A) ;
      C_S := S'Address ;
      -- ... and calls the C procedure
      C_Marshall_2 (C_A,C_S) ;
   end;


   -- C_UnMarshall_2
   -----------------
   procedure C_UnMarshall_2 (A : out System.Address ;
                             S : in out System.Address) ;
   pragma Import (C,C_UnMarshall_2,"unmarshall__21Ada_netBufferedStreamRbR17NetBufferedStream") ;
   -- wrapper around Ada_netBufferedStream function marshall
   -- (see Ada_netBufferedStream.hh)
   -- name was changed to avoid conflict
   -- called by the Ada equivalent : UnMarshall


   -- UnMarshall
   -------------
   procedure UnMarshall (A : out Corba.Boolean ;
                         S : in out Object'Class) is
      C_A : System.Address ;
      C_S : System.Address ;
   begin
      -- transforms the arguments in a C type ...
      C_A := A'Address ;
      C_S := S'Address ;
      -- ... and calls the C procedure
      C_UnMarshall_2 (C_A,C_S) ;
   end;


   -- Ada_To_C_Short
   -----------------
   function Ada_To_C_Short is
     new Ada.Unchecked_Conversion (Corba.Short,
                                   Interfaces.C.Short) ;
   -- needed to change ada type Corba.Short
   -- into C type Interfaces.C.Short


   -- C_Marshall_3
   ---------------
   procedure C_Marshall_3 (A : in Interfaces.C.Short ;
                           S : in out System.Address) ;
   pragma Import (C,C_Marshall_3,"marshall__21Ada_netBufferedStreamsR17NetBufferedStream") ;
   -- wrapper around Ada_netBufferedStream function marshall
   -- (see Ada_netBufferedStream.hh)
   -- name was changed to avoid conflict
   -- called by the Ada equivalent : Marshall


   -- Marshall
   -----------
   procedure Marshall (A : in Corba.Short ;
                       S : in out Object'Class) is
      C_A : Interfaces.C.Short ;
      C_S : System.Address ;
   begin
      -- transforms the arguments in a C type ...
      C_A := Ada_To_C_Short (A) ;
      C_S := S'Address ;
      -- ... and calls the C procedure
      C_Marshall_3 (C_A,C_S) ;
   end;


   -- C_UnMarshall_3
   -----------------
   procedure C_UnMarshall_3 (A : out System.Address ;
                             S : in out System.Address) ;
   pragma Import (C,C_UnMarshall_3,"unmarshall__21Ada_netBufferedStreamRsR17NetBufferedStream") ;
   -- wrapper around Ada_netBufferedStream function marshall
   -- (see Ada_netBufferedStream.hh)
   -- name was changed to avoid conflict
   -- called by the Ada equivalent : UnMarshall


   -- UnMarshall
   -------------
   procedure UnMarshall (A : out Corba.Short ;
                         S : in out Object'Class) is
      C_A : System.Address ;
      C_S : System.Address ;
   begin
      -- transforms the arguments in a C type ...
      C_A := A'Address ;
      C_S := S'Address ;
      -- ... and calls the C procedure
      C_UnMarshall_3 (C_A,C_S) ;
   end;


   -- Ada_To_C_Unsigned_Short
   --------------------------
   function Ada_To_C_Unsigned_Short is
     new Ada.Unchecked_Conversion (Corba.Unsigned_Short,
                                   Interfaces.C.Unsigned_Short) ;
   -- needed to change ada type Corba.Unsigned_Short
   -- into C type Interfaces.C.Unsigned_Short


   -- C_Marshall_4
   ---------------
   procedure C_Marshall_4 (A : in Interfaces.C.Unsigned_Short ;
                           S : in out System.Address) ;
   pragma Import (C,C_Marshall_4,"marshall__21Ada_netBufferedStreamUsR17NetBufferedStream") ;
   -- wrapper around Ada_netBufferedStream function marshall
   -- (see Ada_netBufferedStream.hh)
   -- name was changed to avoid conflict
   -- called by the Ada equivalent : Marshall


   -- Marshall
   -----------
   procedure Marshall (A : in Corba.Unsigned_Short ;
                       S : in out Object'Class) is
      C_A : Interfaces.C.Unsigned_Short ;
      C_S : System.Address ;
   begin
      -- transforms the arguments in a C type ...
      C_A := Ada_To_C_Unsigned_Short (A) ;
      C_S := S'Address ;
      -- ... and calls the C procedure
      C_Marshall_4 (C_A,C_S) ;
   end;


   -- C_UnMarshall_4
   -----------------
   procedure C_UnMarshall_4 (A : out System.Address ;
                             S : in out System.Address) ;
   pragma Import (C,C_UnMarshall_4,"unmarshall__21Ada_netBufferedStreamRUsR17NetBufferedStream") ;
   -- wrapper around Ada_netBufferedStream function marshall
   -- (see Ada_netBufferedStream.hh)
   -- name was changed to avoid conflict
   -- called by the Ada equivalent : UnMarshall


   -- UnMarshall
   -------------
   procedure UnMarshall (A : out Corba.Unsigned_Short ;
                         S : in out Object'Class) is
      C_A : System.Address ;
      C_S : System.Address ;
   begin
      -- transforms the arguments in a C type ...
      C_A := A'Address ;
      C_S := S'Address ;
      -- ... and calls the C procedure
      C_UnMarshall_4 (C_A,C_S) ;
   end;


   -- Ada_To_C_Long
   -----------------
   function Ada_To_C_Long is
     new Ada.Unchecked_Conversion (Corba.Long,
                                   Interfaces.C.Long) ;
   -- needed to change ada type Corba.Long
   -- into C type Interfaces.C.Long


   -- C_Marshall_5
   ---------------
   procedure C_Marshall_5 (A : in Interfaces.C.Long ;
                           S : in out System.Address) ;
   pragma Import (C,C_Marshall_5,"marshall__21Ada_netBufferedStreamlR17NetBufferedStream") ;
   -- wrapper around Ada_netBufferedStream function marshall
   -- (see Ada_netBufferedStream.hh)
   -- name was changed to avoid conflict
   -- called by the Ada equivalent : Marshall


   -- Marshall
   -----------
   procedure Marshall (A : in Corba.Long ;
                       S : in out Object'Class) is
      C_A : Interfaces.C.Long ;
      C_S : System.Address ;
   begin
      -- transforms the arguments in a C type ...
      C_A := Ada_To_C_Long (A) ;
      C_S := S'Address ;
      -- ... and calls the C procedure
      C_Marshall_5 (C_A,C_S) ;
   end;


   -- C_UnMarshall_5
   -----------------
   procedure C_UnMarshall_5 (A : out System.Address ;
                             S : in out System.Address) ;
   pragma Import (C,C_UnMarshall_5,"unmarshall__21Ada_netBufferedStreamRlR17NetBufferedStream") ;
   -- wrapper around Ada_netBufferedStream function marshall
   -- (see Ada_netBufferedStream.hh)
   -- name was changed to avoid conflict
   -- called by the Ada equivalent : UnMarshall


   -- UnMarshall
   -------------
   procedure UnMarshall (A : out Corba.Long ;
                         S : in out Object'Class) is
      C_A : System.Address ;
      C_S : System.Address ;
   begin
      -- transforms the arguments in a C type ...
      C_A := A'Address ;
      C_S := S'Address ;
      -- ... and calls the C procedure
      C_UnMarshall_5 (C_A,C_S) ;
   end;


   -- C_Marshall_6
   ---------------
   procedure C_Marshall_6 (A : in Interfaces.C.Unsigned_Long ;
                           S : in out System.Address) ;
   pragma Import (C,C_Marshall_6,"marshall__21Ada_netBufferedStreamUlR17NetBufferedStream") ;
   -- wrapper around Ada_netBufferedStream function marshall
   -- (see Ada_netBufferedStream.hh)
   -- name was changed to avoid conflict
   -- called by the Ada equivalent : Marshall


   -- Marshall
   -----------
   procedure Marshall (A : in Corba.Unsigned_Long ;
                       S : in out Object'Class) is
      C_A : Interfaces.C.Unsigned_Long ;
      C_S : System.Address ;
   begin
      -- transforms the arguments in a C type ...
      C_A := Ada_To_C_Unsigned_Long (A) ;
      C_S := S'Address ;
      -- ... and calls the C procedure
      C_Marshall_6 (C_A,C_S) ;
   end;


   -- C_UnMarshall_6
   -----------------
   procedure C_UnMarshall_6 (A : out System.Address ;
                             S : in out System.Address) ;
   pragma Import (C,C_UnMarshall_6,"unmarshall__21Ada_netBufferedStreamRUlR17NetBufferedStream") ;
   -- wrapper around Ada_netBufferedStream function marshall
   -- (see Ada_netBufferedStream.hh)
   -- name was changed to avoid conflict
   -- called by the Ada equivalent : UnMarshall


   -- UnMarshall
   -------------
   procedure UnMarshall (A : out Corba.Unsigned_Long ;
                         S : in out Object'Class) is
      C_A : System.Address ;
      C_S : System.Address ;
   begin
      -- transforms the arguments in a C type ...
      C_A := A'Address ;
      C_S := S'Address ;
      -- ... and calls the C procedure
      C_UnMarshall_6 (C_A,C_S) ;
   end;


   -- Ada_To_C_Float
   -----------------
   function Ada_To_C_Float is
     new Ada.Unchecked_Conversion (Corba.Float,
                                   Interfaces.C.C_Float) ;
   -- needed to change ada type Corba.Float
   -- into C type Interfaces.C.C_Float


   -- C_Marshall_7
   ---------------
   procedure C_Marshall_7 (A : in Interfaces.C.C_Float ;
                           S : in out System.Address) ;
   pragma Import (C,C_Marshall_7,"marshall__21Ada_netBufferedStreamfR17NetBufferedStream") ;
   -- wrapper around Ada_netBufferedStream function marshall
   -- (see Ada_netBufferedStream.hh)
   -- name was changed to avoid conflict
   -- called by the Ada equivalent : Marshall


   -- Marshall
   -----------
   procedure Marshall (A : in Corba.Float ;
                       S : in out Object'Class) is
      C_A : Interfaces.C.C_Float ;
      C_S : System.Address ;
   begin
      -- transforms the arguments in a C type ...
      C_A := Ada_To_C_Float (A) ;
      C_S := S'Address ;
      -- ... and calls the C procedure
      C_Marshall_7 (C_A,C_S) ;
   end;


   -- C_UnMarshall_7
   -----------------
   procedure C_UnMarshall_7 (A : out System.Address ;
                             S : in out System.Address) ;
   pragma Import (C,C_UnMarshall_7,"unmarshall__21Ada_netBufferedStreamRfR17NetBufferedStream") ;
   -- wrapper around Ada_netBufferedStream function marshall
   -- (see Ada_netBufferedStream.hh)
   -- name was changed to avoid conflict
   -- called by the Ada equivalent : UnMarshall


   -- UnMarshall
   -------------
   procedure UnMarshall (A : out Corba.Float ;
                         S : in out Object'Class) is
      C_A : System.Address ;
      C_S : System.Address ;
   begin
      -- transforms the arguments in a C type ...
      C_A := A'Address ;
      C_S := S'Address ;
      -- ... and calls the C procedure
      C_UnMarshall_7 (C_A,C_S) ;
   end;


   -- Ada_To_C_Double
   ------------------
   function Ada_To_C_Double is
     new Ada.Unchecked_Conversion (Corba.Double,
                                   Interfaces.C.Double) ;
   -- needed to change ada type Corba.Double
   -- into C type Interfaces.C.Double


   -- C_Marshall_8
   ---------------
   procedure C_Marshall_8 (A : in Interfaces.C.Double ;
                           S : in out System.Address) ;
   pragma Import (C,C_Marshall_8,"marshall__21Ada_netBufferedStreamdR17NetBufferedStream") ;
   -- wrapper around Ada_netBufferedStream function marshall
   -- (see Ada_netBufferedStream.hh)
   -- name was changed to avoid conflict
   -- called by the Ada equivalent : Marshall


   -- Marshall
   -----------
   procedure Marshall (A : in Corba.Double ;
                       S : in out Object'Class) is
      C_A : Interfaces.C.Double ;
      C_S : System.Address ;
   begin
      -- transforms the arguments in a C type ...
      C_A := Ada_To_C_Double (A) ;
      C_S := S'Address ;
      -- ... and calls the C procedure
      C_Marshall_8 (C_A,C_S) ;
   end;


   -- C_UnMarshall_8
   -----------------
   procedure C_UnMarshall_8 (A : out System.Address ;
                             S : in out System.Address) ;
   pragma Import (C,C_UnMarshall_8,"unmarshall__21Ada_netBufferedStreamRdR17NetBufferedStream") ;
   -- wrapper around Ada_netBufferedStream function marshall
   -- (see Ada_netBufferedStream.hh)
   -- name was changed to avoid conflict
   -- called by the Ada equivalent : UnMarshall


   -- UnMarshall
   -------------
   procedure UnMarshall (A : out Corba.Double ;
                         S : in out Object'Class) is
      C_A : System.Address ;
      C_S : System.Address ;
   begin
      -- transforms the arguments in a C type ...
      C_A := A'Address ;
      C_S := S'Address ;
      -- ... and calls the C procedure
      C_UnMarshall_8 (C_A,C_S) ;
   end;


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
      for I in 1..Integer(Size) loop
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


   -- Marshall
   -----------
   procedure Marshall (A : in Corba.Completion_Status ;
                       S : in out Object'Class) is
   begin
      -- maps the possible values on the firste shorts
      -- and marshall the right one
      case A is
         when Corba.Completed_Yes =>
            Marshall (Corba.Unsigned_Short (1),S) ;
         when Corba.Completed_No =>
            Marshall (Corba.Unsigned_Short (2),S) ;
         when Corba.Completed_Maybe =>
            Marshall (Corba.Unsigned_Short (3),S) ;
      end case ;
   end;


   -- UnMarshall
   -------------
   procedure UnMarshall (A : out Corba.Completion_Status ;
                         S : in out Object'Class) is
      Tmp : Corba.Unsigned_Short ;
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
   end;


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


   -- C_Is_Reusing_Existing_Connection
   -----------------------------------
   function C_Is_Reusing_Existing_Connection (Self : in Object'Class)
                                              return Sys_Dep.C_Boolean;
   pragma Import (C,C_Is_Reusing_Existing_Connection,"isReUsingExistingConnection__CQ26Strand4Sync") ;
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
