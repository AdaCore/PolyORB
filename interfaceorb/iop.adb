-----------------------------------------------------------------------
----                                                               ----
----                  AdaBroker                                    ----
----                                                               ----
----  This package corresponds to the C class IOP defined in file  ----
----  IOP.h. It provides the type Tagged_Profile_List and some     ----
----  methods to marshall and unmarshall it.                       ----
----                                                               ----
----                                                               ----
----                  package iop                                  ----
----                                                               ----
----   authors : Sebastien Ponce                                   ----
----   date    : 03/09/99                                          ----
----                                                               ----
----                                                               ----
-----------------------------------------------------------------------

with System.Address_To_Access_Conversions ;
with Ada.Unchecked_Conversion ;

package body Iop is

   -- Marshall
   -----------
   procedure Marshall (A : in Iop.Tagged_Profile_List ;
                       S : in out Netbufferedstream.Object) is
      C_A : System.Address ;
      C_S : System.Address ;
   begin
      -- transforms the arguments in a C type ...
      C_A := System.Address (A) ;
      C_S := S'Address ;
      -- ... and calls the C procedure
      C_Marshall (C_A,C_S) ;
   end ;


   -- UnMarshall
   -------------
   procedure UnMarshall (A : out IOP.Tagged_Profile_List ;
                         S : in out Netbufferedstream.Object) is
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


   -- Marshall
   -----------
   procedure Marshall (A : in Iop.Tagged_Profile_List ;
                       S : in out Membufferedstream.Object) is
      C_A : System.Address ;
      C_S : System.Address ;
   begin
      -- transforms the arguments in a C type ...
      C_A := System.Address (A) ;
      C_S := S'Address ;
      -- ... and calls the C procedure
      C_Marshall2 (C_A,C_S) ;
   end ;


   -- UnMarshall
   -------------
   procedure UnMarshall (A : out IOP.Tagged_Profile_List ;
                         S : in out Membufferedstream.Object) is
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


   -- Ada_To_C_Unsigned_Long
   -------------------------
   function Ada_To_C_Unsigned_Long is
     new Ada.Unchecked_Conversion (Corba.Unsigned_Long,
                                   Interfaces.C.Unsigned_Long) ;
   -- needed to change ada type Corba.Unsigned_Long
   -- into C type Interfaces.C.Unsigned_Long


   -- C_To_Ada_Unsigned_Long
   -------------------------
   function C_To_Ada_Unsigned_Long is
     new Ada.Unchecked_Conversion (Interfaces.C.Unsigned_Long ,
                                   Corba.Unsigned_Long) ;
   -- needed to change ada type Interfaces.C.Unsigned_Long
   -- into C type Corba.Unsigned_Long


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
      C_Initialoffset := Ada_To_C_Unsigned_Long (Initialoffset) ;
      -- ... calls the C function ...
      C_Result := C_NP_alignedSize (C_A,C_Initialoffset) ;
      -- ... and transforms the result in an Ada type
      return  C_To_Ada_Unsigned_Long (C_Result) ;
   end ;


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
      return  C_To_Ada_Unsigned_Long (C_Result) ;
   end ;



end Iop ;
