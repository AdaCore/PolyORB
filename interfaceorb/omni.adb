-----------------------------------------------------------------------
----                                                               ----
----                  AdaBroker                                    ----
----                                                               ----
----     This package is wrapped around the C class omni_C2Ada     ----
----   declared in omni_C2Ada.hh;                                  ----
----     It provides the 2 functions of omni_C2Ada and their       ----
----   equivalent in ADA.                                          ----
----                                                               ----
----                  package omni                                 ----
----                                                               ----
----   authors : Sebastien Ponce, Fabien Azavant                   ----
----   date    : 02/08/99                                          ----
----                                                               ----
----                                                               ----
-----------------------------------------------------------------------

with Ada.Exceptions ;
with System.Address_To_Access_Conversions ;

Package body Omni is

   function Object_To_String (Obj : in OmniObject.Object)
                              return String is
      C_Obj : System.Address ;
      C_Result : Interfaces.C.Strings.Chars_Ptr ;
   begin
      -- transforms the arguments in a C type ...
      C_Obj := Obj'Address ;
      -- ... calls the C function ...
      C_Result := C_Object_To_String (C_Obj) ;
      -- ... and transforms the result in Ada type
      return Interfaces.C.Strings.Value (C_Result) ;
   end ;


   -- Address_To_Octet
   -------------------
   package Address_To_OmniObject is
     new System.Address_To_Access_Conversions (OmniObject.Object) ;
   -- needed to interface System.Address and Corba.Octet


   function String_To_Object (Str : in String)
                              return OmniObject.Object is
      C_Str : Interfaces.C.Strings.Chars_Ptr ;
      C_Result : System.Address ;
      Ada_Result_Ptr : Address_To_OmniObject.Object_Pointer ;
   begin
      -- transforms the arguments in a C type ...
      C_Str := Interfaces.C.Strings.New_String (Str) ;
               -- desallocation in e few lines
      -- ... calls the C function ...
      C_Result := C_String_To_Object (C_Str) ;
               -- desallocation of C_Str
      Interfaces.C.Strings.Free (C_Str) ;
      -- ... and transforms the result in Ada type
      Ada_Result_Ptr :=  Address_To_OmniObject.To_Pointer (C_Result) ;
      return Ada_Result_Ptr.all ;
   end ;




   -- Align_To
   -----------
   function Align_To(P: in Corba.Unsigned_Long ;
                     Align : in Alignment_T)
                     return Corba.Unsigned_Long is
   begin
      Ada.Exceptions.Raise_Exception(Corba.AdaBroker_Not_Implemented_Yet'Identity,
                                     "Omni.Align_To") ;
      return Corba.Unsigned_Long(0) ;
   end ;


end Omni ;
