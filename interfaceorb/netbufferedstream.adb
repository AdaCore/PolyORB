-----------------------------------------------------------------------
----                                                               ----
----                  AdaBroker                                    ----
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
----                  package NetBufferedStream                    ----
----                                                               ----
----   authors : Sebastien Ponce                                   ----
----   date    : 02/26/99                                          ----
----                                                               ----
----                                                               ----
-----------------------------------------------------------------------


with Ada.Unchecked_Conversion ;
with Ada.Exceptions ;

package body NetBufferedStream is

   -- Ada_To_C_Unsigned_Long
   -------------------------
   function Ada_To_C_Unsigned_Long is
     new Ada.Unchecked_Conversion (Corba.Unsigned_Long,
                                   Interfaces.C.Unsigned_Long) ;
   -- needed to change ada type Corba.Unsigned_Long
   -- into C type Interfaces.C.Unsigned_Long

   -- Init
   -------
   procedure Init (Self : in Object'Class ;
                   r : in Rope.Object ;
                   Rdlock : in Corba.Boolean ;
                   WrLock : in Corba.Boolean ;
                   Bufsize : in Corba.Unsigned_Long) is
      C_R : System.Address ;
      C_RdLock : Sys_Dep.C_Boolean ;
      C_WrLock : Sys_Dep.C_Boolean ;
      C_Bufsize : Interfaces.C.Unsigned_Long ;
   begin
      -- transforms the arguments into a C type ...
      C_R := R'Address ;
      C_RdLock := Sys_Dep.Boolean_Ada_To_C (RdLock) ;
      C_WrLock := Sys_Dep.Boolean_Ada_To_C (WrLock) ;
      C_Bufsize := Ada_To_C_Unsigned_Long (Bufsize) ;
      -- ... and calls the C procedure
      C_Init (Self, C_R, C_RdLock, C_WrLock, C_Bufsize) ;
   end ;


   -- Ada_To_C_Char
   ----------------
   function Ada_To_C_Char is
     new Ada.Unchecked_Conversion (Corba.Char,
                                   Interfaces.C.Char) ;
   -- needed to change ada type Corba.Char
   -- into C type Interfaces.C.Char

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

   --------------- For Corba.String ---------------------

   procedure Marshall (A : in Corba.String ;
                       S : in out Object'Class) is
   begin
      null ;
   end ;


   procedure UnMarshall (A : out Corba.String ;
                         S : in out Object'Class) is
   begin
      null ;
   end ;
   ----------------------------------------------------

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
