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
   procedure Raise_C_UNKNOWN_Exception (Corba.Unsigned_Long Pd_Minor ;
                                        Completion_Status Pd_Status) is
      C_Pd_Minor : Interfaces.C.Unsigned_Long ;
      C_Pd_Status : Interfaces.C.Int ;
   begin
      -- transforms the arguments in a C type ...
      C_Pd_Minor := Ada_To_C_Unsigned_Long (Pd_Minor) ;
      C_Pd_Status := Status_To_Int (Pd_Status) ;
      -- ... and calls the C procedure
      C_Raise_C_UNKNOWN_Exception (C_Pd_Minor, C_Pd_Status) ;
   end ;


   -- C_To_Ada_Unsigned_Long
   -------------------------
   function C_To_Ada_Unsigned_Long is
     new Ada.Unchecked_Conversion (Interfaces.C.Unsigned_Long ;
                                   Corba.Unsigned_Long) ;
   -- needed to change C type Interfaces.C.Unsigned_Long
   -- into Ada type Corba.Unsigned_Long


   -- C_Raise_Ada_UNKNOWN_Exception
   --------------------------------
   procedure C_Raise_Ada_UNKNOWN_Exception (Interfaces.C.Unsigned_Long Pd_Minor ;
                                            Interfaces.C.Int Pd_Status) is
      Ada_Pd_Minor : Corba.Unsigned_Long ;
      Ada_Pd_Status : Completion_Status ;
   begin
      -- transforms the arguments in a Ada type ...
      Ada_Pd_Minor := C_To_Ada_Unsigned_Long (Pd_Minor) ;
      Ada_Pd_Status := Int_To_Status (Pd_Status) ;
      -- ... and calls the C procedure
      Raise_Ada_UNKNOWN_Exception (Ada_Pd_Minor,Ada_Pd_Status) ;
   end ;


   -- Raise_Ada_UNKNOWN_Exception
   ------------------------------
   procedure Raise_Ada_UNKNOWN_Exception (Corba.Unsigned_Long Pd_Minor ;
                                          Completion_Status Pd_Status) is
      Member : Unknown_Members ;
   begin
      -- creates a new exception member with parameters ...
      Member.Minor := Pd_Minor ;
      Minor.Completed := Pd_Status ;
      -- ... and raise the exception
      Raise_Corba_Exception (Unknown, Member) ;
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
     new Ada.Unchecked_Conversion (Interfaces.C.Int ;
                                   Integer) ;
   -- needed to change ada type Interfaces.C.Int
   -- into C type Integer


   -- Int_To_Status
   ----------------
   function Int_To_Status (Interfaces.C.Int N)
                           return Completion_Status is
      Ada_N : Integer ;
   begin
      Ada_N :=  Completion_Status'Val(N) ;
      return C_To_Ada_Int (Ada_N) ;
   end ;


   -- Status_To_Int
   ----------------
   function Status_To_Int (Completion_Status Status)
                           return Interfaces.C.Int is
      Ada_Result : Integer ;
   begin
      Ada_Result := Completion_Status'Pos(Status) ;
      return Ada_To_C_Int (Ada_Result) ;
   end ;

end Exceptions ;
