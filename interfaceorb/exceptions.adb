-----------------------------------------------------------------------
----                                                               ----
----                  AdaBroker                                    ----
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
----                  package omniRopeAndKey                       ----
----                                                               ----
----   authors : Sebastien Ponce                                   ----
----   date    : 03/04/99                                          ----
----                                                               ----
----                                                               ----
-----------------------------------------------------------------------

with Ada.Unchecked_Conversion ;
with Text_Io ; use Text_Io ;

package body Exceptions is

   -- Ada_To_C_Unsigned_Long
   -------------------------
   function Ada_To_C_Unsigned_Long is
     new Ada.Unchecked_Conversion (Corba.Unsigned_Long,
                                   Interfaces.C.Unsigned_Long) ;
   -- needed to change ada type Corba.Unsigned_Long
   -- into C type Interfaces.C.Unsigned_Long


   -- Raise_C_UNKNOWN_Exception
   ----------------------------
   procedure Raise_C_UNKNOWN_Exception (E : in Ada.Exceptions.Exception_Occurrence) is
      Member : Corba.Unknown_Members ;
      C_Minor : Interfaces.C.Unsigned_Long ;
      C_Status : Interfaces.C.Int ;
   begin
      -- transforms the arguments in a C type ...
      Corba.Get_Members (E,Member) ;
      C_Minor := Ada_To_C_Unsigned_Long (Member.Minor) ;
      C_Status := Status_To_Int (Member.Completed) ;
      -- ... and calls the C procedure
      C_Raise_C_UNKNOWN_Exception (C_Minor, C_Status) ;
--   exception
--      when E : others => Put_Line (Ada.Exceptions.Exception_Message(E)) ;
   end ;


   -- C_To_Ada_Unsigned_Long
   -------------------------
   function C_To_Ada_Unsigned_Long is
     new Ada.Unchecked_Conversion (Interfaces.C.Unsigned_Long ,
                                   Corba.Unsigned_Long) ;
   -- needed to change C type Interfaces.C.Unsigned_Long
   -- into Ada type Corba.Unsigned_Long


   -- C_Raise_Ada_UNKNOWN_Exception
   --------------------------------
   procedure C_Raise_Ada_UNKNOWN_Exception (Pd_Minor : in Interfaces.C.Unsigned_Long ;
                                            Pd_Status : in Interfaces.C.Int) is
      Ada_Pd_Minor : Corba.Unsigned_Long ;
      Ada_Pd_Status : Corba.Completion_Status ;
   begin
      -- transforms the arguments in a Ada type ...
      Ada_Pd_Minor := C_To_Ada_Unsigned_Long (Pd_Minor) ;
      Ada_Pd_Status := Int_To_Status (Pd_Status) ;
      -- ... and calls the C procedure
      Raise_Ada_UNKNOWN_Exception (Ada_Pd_Minor,Ada_Pd_Status) ;
   end ;


   -- Raise_Ada_UNKNOWN_Exception
   ------------------------------
   procedure Raise_Ada_UNKNOWN_Exception (Pd_Minor : in Corba.Unsigned_Long ;
                                          Pd_Status : in Corba.Completion_Status) is
   begin
      -- creates a new exception member with parameters
      -- and raises the exception
      Corba.Raise_Corba_Exception (Corba.Unknown'Identity,
                                   new Corba.Unknown_Members'(Minor => Pd_Minor ,
                                                              Completed => Pd_Status)) ;
   end ;


   -- Ada_To_C_Int
   ---------------
   function Ada_To_C_Int is
     new Ada.Unchecked_Conversion (Integer,
                                   Interfaces.C.Int) ;
   -- needed to change ada type Integer
   -- into C type Interfaces.C.Int


   -- C_To_Ada_Int
   ---------------
   function C_To_Ada_Int is
     new Ada.Unchecked_Conversion (Interfaces.C.Int ,
                                   Integer) ;
   -- needed to change ada type Interfaces.C.Int
   -- into C type Integer


   -- Int_To_Status
   ----------------
   function Int_To_Status (N : in Interfaces.C.Int)
                           return Corba.Completion_Status is
      Ada_N : Integer ;
   begin
      Ada_N := C_To_Ada_Int (N) ;
      return Corba.Completion_Status'Val(Ada_N) ;
   end ;


   -- Status_To_Int
   ----------------
   function Status_To_Int (Status : in Corba.Completion_Status)
                           return Interfaces.C.Int is
      Ada_Result : Integer ;
   begin
      Ada_Result := Corba.Completion_Status'Pos(Status) ;
      return Ada_To_C_Int (Ada_Result) ;
   end ;

end Exceptions ;
