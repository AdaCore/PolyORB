-----------------------------------------------------------------------
----                                                               ----
----                  AdaBroker                                    ----
----                                                               ----
----     This package is wrapped around the C++ class              ----
----   Ada_OmniRopeAndKey declared in Ada_OmniRopeAndKey.          ----
----     It provides the same functions as this package plus       ----
----   the Ada version of thouse where arguments types are         ----
----   to be change.                                               ----
----     It includes a Init function since a Ada class has no      ----
----   constructor.                                                ----
----                                                               ----
----                                                               ----
----                  package body omniRopeAndKey                  ----
----                                                               ----
----   authors : Sebastien Ponce, Fabien Azavant                   ----
----   date    : 02/18/99                                          ----
----                                                               ----
----                                                               ----
-----------------------------------------------------------------------

--with Ada ;
with Ada.Exceptions ;
with System.Address_To_Access_Conversions ;
with Ada.Unchecked_Conversion ;

package body OmniRopeAndKey is

   -- Ada_To_C_Unsigned_Long
   -------------------------
   function Ada_To_C_Unsigned_Long is
     new Ada.Unchecked_Conversion (Corba.Unsigned_Long,
                                   Interfaces.C.Unsigned_Long) ;
   -- needed to change ada type Corba.Unsigned_Long
   -- into C type Interfaces.C.Unsigned_Long

   -- Init
   -------
   procedure Init (Self : in out Object'Class ;
                   R : in Rope.Object ;
                   K : in Corba.Octet ;
                   Ksize : in Corba.Unsigned_Long) is
      C_R : System.Address ;
      C_K : System.Address ;
      C_Ksize : Interfaces.C.Unsigned_Long ;
   begin
      -- transforms the arguments in a C type ...
      C_R := R'Address ;
      C_K := K'Address ;
      C_Ksize := Ada_To_C_Unsigned_Long(Ksize) ;
      -- ... and calls the C procedure
      C_Init (Self,C_R,C_K,C_Ksize) ;
   end;


   -- Init
   -------
   procedure Init (Self : in out Object'Class) is
   begin
      C_Init2 (Self) ;
      -- just call the C function
   end;


   -- Address_To_Rope
   ------------------
   package Address_To_Rope is
     new System.Address_To_Access_Conversions (Rope.Object) ;
   -- needed to interface System.Address and Rope.Object


   -- Get_Rope
   -----------
   function Get_Rope (Self : in Object'Class)
                      return Rope.Object is
      C_Result : System.Address ;
      Ada_Result_Ptr : Address_To_Rope.Object_Pointer ;
   begin
      -- calls the C function ...
      C_Result := C_Get_Rope (Self) ;
      -- ... and transforms the result in Ada type
      Ada_Result_Ptr := Address_To_Rope.To_Pointer (C_Result) ;
      return Ada_Result_Ptr.All ;
   end;


   -- Address_To_Octet
   -------------------
   package Address_To_Octet is
     new System.Address_To_Access_Conversions (Corba.Octet) ;
   -- needed to interface System.Address and Corba.Octet


   -- Get_Key
   ----------
   function Get_Key (Self : in Object'Class)
                     return CORBA.Octet is
      C_Result : System.Address ;
      Ada_Result_Ptr : Address_To_Octet.Object_Pointer ;
   begin
      -- calls the C function ...
      C_Result := C_Get_Key (Self) ;
      -- ... and transforms the result in Ada type
      Ada_Result_Ptr := Address_To_Octet.To_Pointer (C_Result) ;
      return Ada_Result_Ptr.all ;
   end;


   -- C_To_Ada_Unsigned_Long
   -------------------------
   function C_To_Ada_Unsigned_Long is
     new Ada.Unchecked_Conversion (Interfaces.C.Unsigned_Long,
                                   Corba.Unsigned_Long) ;
   -- needed to change C type Interfaces.C.Unsigned_Long into
   -- ada type Corba.Unsigned_Long


   -- Key_Size
   -----------
   function Key_Size (Self : in Object'Class)
                      return CORBA.Unsigned_Long is
      C_Result : Interfaces.C.Unsigned_Long ;
   begin
      -- calls the C function ...
      C_Result := C_Key_Size (Self) ;
      -- ... and transforms the result in Ada type
      return C_To_Ada_Unsigned_Long (C_Result) ;
   end;

   -- C_Raise_Ada_Exception
   ------------------------
   procedure C_Raise_Ada_Exception (Self : in Object'Class ;
                                    Msg : in Interfaces.C.Strings.Chars_Ptr) is
      Ada_Msg : String := Interfaces.C.Strings.Value (Msg) ;
   begin
      -- argument already tranformed in a Ada type ...
      -- ... so calls the Ada procedure
      Raise_Ada_Exception (Self,Ada_Msg) ;
   end ;


   procedure Raise_Ada_Exception (Self : in Object'Class ;
                                  Msg : in String) is
   begin
      Ada.Exceptions.Raise_Exception (Corba.No_Initialisation_Error'Identity,Msg) ;
   end ;

end OmniRopeAndKey ;











