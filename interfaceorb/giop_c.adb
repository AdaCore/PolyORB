-----------------------------------------------------------------------
----                                                               ----
----                  AdaBroker                                    ----
----                                                               ----
----     This package is wrapped around the C++ class              ----
----   Ada_Giop_c declared in Ada_Giop_c.hh.                       ----
----     It provides the same functions as this package plus       ----
----   the Ada version of thouse where arguments types are         ----
----   to be change.                                               ----
----     It includes the definition of the function                ----
----   RequestHeaderSize declared in giopDriver.h but not          ----
----   present in Ada_Giop_c since it is a static function.        ----
----                                                               ----
----                  package body Giop_C                          ----
----                                                               ----
----   authors : Sebastien Ponce, Fabien Azavant                   ----
----   date    : 02/28/99                                          ----
----                                                               ----
----                                                               ----
-----------------------------------------------------------------------


with Ada.Unchecked_Conversion ;
with Ada.Exceptions ;
with Ada.Strings.Unbounded ;

package body Giop_C is

   -- Init
   -------
   procedure Init (Self : in Object'Class ;
                   R : in Rope.Object) is
      C_R : System.Address ;
   begin
      -- transforms the arguments into a C type ...
      C_R := R'Address ;
      -- ... and calls the C procedure
      C_Init (Self, C_R) ;
   end;


   -- Ada_To_C_Unsigned_Long
   -------------------------
   function Ada_To_C_Unsigned_Long is
     new Ada.Unchecked_Conversion (Corba.Unsigned_Long,
                                   Interfaces.C.Unsigned_Long) ;
   -- needed to change ada type Corba.Unsigned_Long
   -- into C type Interfaces.C.Unsigned_Long


   -- Initialize_Request
   ---------------------
   procedure Initialize_Request (Self : in Object'Class ;
                                 Objkey : in Corba.Octet ;
                                 Objkeysize : in Corba.Unsigned_Long ;
                                 Opname : in CORBA.STRING ;
                                 MsgSize : in Corba.Unsigned_Long ;
                                 Oneway : in CORBA.Boolean) is
      C_Objkey : System.Address ;
      C_Objkeysize : Interfaces.C.Unsigned_Long ;
      Ada_Opname : String := Ada.Strings.Unbounded.To_String (Ada.Strings.Unbounded.Unbounded_String (Opname)) ;
      C_Opname : Interfaces.C.Strings.Chars_Ptr ;
      C_MsgSize : Interfaces.C.Unsigned_Long ;
      C_Oneway : Sys_Dep.C_Boolean ;
   begin
      -- transforms the arguments into a C type ...
      C_Objkey := Objkey'Address ;
      C_Objkeysize := Ada_To_C_Unsigned_Long (Objkeysize) ;
      C_Opname := Interfaces.C.Strings.New_String (Ada_Opname) ;
                 -- desallocation in a few lines
      C_MsgSize := Ada_To_C_Unsigned_Long (MsgSize) ;
      C_Oneway := Sys_Dep.Boolean_Ada_To_C (Oneway) ;
      -- ... and calls the C procedure
      C_Initialize_Request (Self,
                            C_Objkey,
                            C_Objkeysize,
                            C_Opname,
                            C_MsgSize,
                            C_Oneway) ;
                 -- desallocation of C_Opname
      Interfaces.C.Strings.Free (C_Opname) ;
   end ;


   -- Receive_Reply
   ----------------
   function Receive_Reply (Self : in Object'Class)
                           return Giop.Reply_Status_Type is
      C_Result : Interfaces.C.Int ;
   begin
      -- calls the C function ...
      C_Result := C_Receive_Reply (Self) ;
      -- ... and transforms the result into an Ada type
      return Giop.C_Int_To_Reply_Status_Type (C_Result) ;
   end;


   -- Request_Completed
   --------------------
   procedure Request_Completed (Self : in Object'Class ;
                                Skip_Msg : in CORBA.Boolean := False) is
      C_Skip_Msg : Sys_Dep.C_Boolean ;
   begin
      -- transforms the arguments into a C type ...
      C_Skip_Msg := Sys_Dep.Boolean_Ada_To_C (Skip_Msg) ;
      -- ... and calls the C procedure
      C_Request_Completed (Self, C_Skip_Msg) ;
   end ;


   -- C_To_Ada_Unsigned_Long
   -------------------------
   function C_To_Ada_Unsigned_Long is
     new Ada.Unchecked_Conversion (Interfaces.C.Unsigned_Long,
                                   Corba.Unsigned_Long) ;
   -- needed to change C type Interfaces.C.Unsigned_Long
   -- into ada type Corba.Unsigned_Long

   -- Request_Header_Size
   ----------------------
   function Request_Header_Size (Objkeysize : in Corba.Unsigned_long ;
                                 Opnamesize : in Corba.Unsigned_long)
                                 return Corba.Unsigned_long is
      C_Objkeysize : Interfaces.C.Unsigned_Long ;
      C_Opnamesize : Interfaces.C.Unsigned_Long ;
      C_Result : Interfaces.C.Unsigned_Long ;
   begin
      -- transforms the arguments into a C type ...
      C_Objkeysize := Ada_To_C_Unsigned_Long (Objkeysize) ;
      C_Opnamesize := Ada_To_C_Unsigned_Long (Opnamesize) ;
      -- ... calls the C procedure ...
      C_Result := C_Request_Header_Size (C_Objkeysize, C_Opnamesize) ;
      -- ... and transforms the result into an Ada type
      return  C_To_Ada_Unsigned_Long (C_Result) ;
   end;


end Giop_C ;

