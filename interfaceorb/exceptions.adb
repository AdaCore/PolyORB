-----------------------------------------------------------------------
-----------------------------------------------------------------------
----                                                               ----
----                         AdaBroker                             ----
----                                                               ----
----                   package Exceptions                          ----
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
----     This package deals with the raising of C exceptions in    ----
----   Ada and ada ones in C.                                      ----
----     It is both a C and a Ada class (see Ada_Exceptions.hh)    ----
----   and provides 2 mains methods : raise_C_Exception and        ----
----   raise_Ada_Exception. The first one is called by Ada code    ----
----   and implemented in C. The second is called by C code and    ----
----   implemented in Ada. Both translate exceptions in the other  ----
----   language.                                                   ----
----                                                               ----
----                                                               ----
----   authors : Sebastien Ponce, Fabien Azavant                   ----
----   date    : 02/28/99                                          ----
----                                                               ----
-----------------------------------------------------------------------
-----------------------------------------------------------------------


package body Exceptions is


   --------------------------------------------------
   ---       conversion functions                 ---
   --------------------------------------------------

   -- Int_To_Status
   ----------------
   function Int_To_Status (N : in Interfaces.C.Int)
                           return Corba.Completion_Status is
      Ada_N : Integer ;
   begin
      Ada_N := Integer (N) ;
      return Corba.Completion_Status'Val(Ada_N) ;
   end ;


   -- Status_To_Int
   ----------------
   function Status_To_Int (Status : in Corba.Completion_Status)
                           return Interfaces.C.Int is
      Ada_Result : Integer ;
   begin
      Ada_Result := Corba.Completion_Status'Pos(Status) ;
      return Interfaces.C.Int (Ada_Result) ;
   end ;


   --------------------------------------------------
   ---       UNKNOWN Exception                    ---
   --------------------------------------------------


   -- C_Raise_C_UNKNOWN_Exception
   -------------------------------
  procedure C_Raise_C_UNKNOWN_Exception (Pd_Minor : in Interfaces.C.Unsigned_Long ;
                                          Pd_Status : in Interfaces.C.Int) ;
   pragma Import (Cpp,C_Raise_C_UNKNOWN_Exception,"Raise_C_UNKNOWN_Exception__FUlQ25CORBA16CompletionStatus");
   -- Wrapped around C function Raise_C_UNKNOWN_Exception
   -- declared in Ada_exceptions.hh
   -- Called by Ada code.
   -- Handles in C a Corba exception that was raised in Ada.


   -- Raise_C_UNKNOWN_Exception
   ----------------------------
   procedure Raise_C_UNKNOWN_Exception (E : in Ada.Exceptions.Exception_Occurrence) is
      Member : Corba.Unknown_Members ;
      C_Minor : Interfaces.C.Unsigned_Long ;
      C_Status : Interfaces.C.Int ;
   begin
      -- transforms the arguments in a C type ...
      Corba.Get_Members (E,Member) ;
      C_Minor := Interfaces.C.Unsigned_Long (Member.Minor) ;
      C_Status := Status_To_Int (Member.Completed) ;
      -- ... and calls the C procedure
      C_Raise_C_UNKNOWN_Exception (C_Minor, C_Status) ;
   end ;


   -- Raise_Ada_UNKNOWN_Exception
   ------------------------------
   procedure Raise_Ada_UNKNOWN_Exception (Pd_Minor : in Corba.Unsigned_Long ;
                                          Pd_Status : in Corba.Completion_Status) is
   begin
      -- creates a new exception member with parameters
      -- and raises the exception
      Corba.Raise_Corba_Exception (Corba.Unknown'Identity,
                                   Corba.Unknown_Members'(Minor => Pd_Minor ,
                                    Completed => Pd_Status)) ;
   end ;


   -- C_Raise_Ada_UNKNOWN_Exception
   --------------------------------
   procedure C_Raise_Ada_UNKNOWN_Exception (Pd_Minor : in Interfaces.C.Unsigned_Long ;
                                            Pd_Status : in Interfaces.C.Int) is
      Ada_Pd_Minor : Corba.Unsigned_Long ;
      Ada_Pd_Status : Corba.Completion_Status ;
   begin
      -- transforms the arguments in a Ada type ...
      Ada_Pd_Minor := Corba.Unsigned_Long (Pd_Minor) ;
      Ada_Pd_Status := Int_To_Status (Pd_Status) ;
      -- ... and calls the C procedure
      Raise_Ada_UNKNOWN_Exception (Ada_Pd_Minor,Ada_Pd_Status) ;
   end ;




end Exceptions ;
