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
----                  package Giop_C                               ----
----                                                               ----
----   authors : Sebastien Ponce, Fabien Azavant                   ----
----   date    : 02/28/99                                          ----
----                                                               ----
----                                                               ----
-----------------------------------------------------------------------


with Interfaces.C ;
with Interfaces.CPP ;
with Interfaces.C.Strings ;
with System ;
with Corba ;
with Giop ;
with Sys_Dep ;
with Rope ;

package Giop_C is

   type Object is tagged record
      Table : Interfaces.CPP.Vtable_Ptr ;
   end record;

   pragma CPP_Class (Object) ;
   pragma CPP_Vtable (Object,Table,1) ;
   -- This object is wrapped around Ada_Giop_c
   -- (see Ada_Giop_c.hh)

   type Object_Ptr is access all Object ;
   -- just to give a name to pointers on Object


   procedure C_Init (Self : in Object'Class ;
                     R : in System.Address) ;
   pragma Import (C,C_Init,"Init__10Ada_Giop_cP4Rope") ;
   -- wrapper around  Ada_Giop_c procedure Init
   -- (see Ada_Giop_c.hh)

   procedure Init (Self : in Object'Class ;
                   R : in Rope.Object) ;
   -- Ada equivalent of C function C_Init


   procedure C_Initialize_Request (Self : in Object'Class ;
                                   Objkey : in System.Address ;
                                   Objkeysize : in Interfaces.C.Unsigned_Long ;
                                   Opname : in Interfaces.C.Strings.Chars_Ptr ;
                                   MsgSize : in Interfaces.C.Unsigned_Long ;
                                   Oneway : in Sys_Dep.C_Boolean) ;
   pragma Import (C,C_Initialize_Request,"InitialiseRequest__10Ada_Giop_cPCvUiPCcUiUib") ;
   -- wrapper around  Ada_Giop_c procedure Initialize_Request
   -- (see Ada_Giop_c.hh)

   procedure Initialize_Request (Self : in Object'Class ;
                                 Objkey : in Corba.Octet ;
                                 Objkeysize : in Corba.Unsigned_Long ;
                                 Opname : in CORBA.STRING ;
                                 MsgSize : in Corba.Unsigned_Long ;
                                 Oneway : in CORBA.Boolean) ;
   -- Ada equivalent of C function C_Initialize_Request


   function C_Receive_Reply (Self : in Object'Class)
                             return Interfaces.C.Int ;
   pragma Import (C,C_Receive_Reply,"ReceiveReply__10Ada_Giop_c") ;
   -- wrapper around  Ada_Giop_c procedure Receive_Reply
   -- (see Ada_Giop_c.hh)

   function Receive_Reply (Self : in Object'Class)
                           return Giop.Reply_Status_Type ;
   -- Ada equivalent of C function C_Receive_Reply


   procedure C_Request_Completed (Self : in Object'Class ;
                                  Skip_Msg : in Sys_Dep.C_Boolean) ;
   pragma Import (C,C_Request_Completed,"RequestCompleted__10Ada_Giop_cb") ;
   -- wrapper around  Ada_Giop_c procedure Request_Completed
   -- (see Ada_Giop_c.hh)

   procedure Request_Completed (Self : in Object'Class ;
                                Skip_Msg : in CORBA.Boolean := False) ;
   -- Ada equivalent of C function C_Receive_Reply


   function C_Request_Header_Size (Objkeysize : in Interfaces.C.Unsigned_long ;
                                   Opnamesize : in Interfaces.C.Unsigned_long)
                                   return Interfaces.C.Unsigned_long ;
   pragma Import (C,C_Request_Header_Size,"RequestHeaderSize__6GIOP_CUiUi") ;
   -- wrapper around GIOP_C procedure Request_Header_Size
   -- (see giopDriver.h)

   function Request_Header_Size (Objkeysize : in Corba.Unsigned_long ;
                                 Opnamesize : in Corba.Unsigned_long)
                                 return Corba.Unsigned_long ;
   -- Ada equivalent of C function C_Request_Header_Size


private

   function Constructor return Object'Class;
   pragma CPP_Constructor (Constructor);
   pragma Import (CPP,Constructor,"__10Ada_Giop_c");
   -- wrapped around the C constructor of Ada_Giop_c


   procedure C_Raise_Ada_Exception (Self : in Object'Class ;
                                    Msg : in Interfaces.C.Strings.Chars_Ptr) ;
   pragma Export (CPP,C_Raise_Ada_Exception,"raise_ada_exception__10Ada_Giop_cPCc") ;
   -- This function allows C code to raise Ada exception
   -- (see Ada_Giop_c.hh)

   procedure Raise_Ada_Exception (Self : in Object'Class ;
                                  Msg : in String) ;
   -- Ada equivalent of C function C_Raise_Ada_Exception

end Giop_C ;

