-----------------------------------------------------------------------
----                                                               ----
----                  AdaBroker                                    ----
----                                                               ----
----     This package is wrapped around the C++ class              ----
----   Ada_Giop_s declared in Ada_Giop_s.hh.                       ----
----     It provides the same functions as this package plus       ----
----   the Ada version of thouse where arguments types are         ----
----   to be change.                                               ----
----     It does not include an Init function since it is          ----
----   useless for AdaBroker. (AdaBroker never creates a Giop_s    ----
----   object)                                                     ----
----                                                               ----
----                  package Giop_S                               ----
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

package Giop_S is

   type Object is tagged record
      Table : Interfaces.CPP.Vtable_Ptr ;
   end record;

   pragma CPP_Class (Object) ;
   pragma CPP_Vtable (Object,Table,1) ;
   -- This object is wrapped around Ada_Giop_s
   -- (see Ada_Giop_s.hh)

   type Object_Ptr is access all Object ;
   -- just to give a name to pointers on Object


   procedure C_Request_Received (Self : in Object'Class ;
                                 Skip : in Sys_Dep.C_Boolean) ;
   pragma Import (C,C_Request_Received,"RequestReceived__10Ada_Giop_sb") ;
   -- wrapper around  Ada_Giop_s procedure RequestReceived
   -- (see Ada_Giop_s.hh)

   procedure Request_Received (Self : in Object'Class ;
                               Skip : in Boolean) ;
   -- Ada equivalent of C function C_Init


   procedure C_Initialize_Reply (Self : in Object'Class ;
                                 Status : in Interfaces.C.int ;
                                 MsgSize : in Interfaces.C.Unsigned_Long);
   pragma Import (C,C_Initialize_Reply,"InitialiseReply__10Ada_Giop_sUlUi") ;
   -- wrapper around  Ada_Giop_s procedure InitialiseReply
   -- (see Ada_Giop_s.hh)

   procedure Initialize_Reply (Self : in Object'Class ;
                               Status : in Giop.Reply_Status_Type ;
                               MsgSize : in Corba.Unsigned_long);
   -- Ada equivalent of C function C_Init


   procedure Reply_Completed (Self : in Object'Class);
   pragma Import (C,Reply_Completed,"ReplyCompleted__10Ada_Giop_s") ;
   -- wrapper around  Ada_Giop_s procedure InitialiseReply
   -- (see Ada_Giop_s.hh)
   -- no Ada equivalent since there is no arguments


private

   function Constructor return Object'Class;
   pragma CPP_Constructor (Constructor);
   pragma Import (CPP,Constructor,"__10Ada_Giop_s");
   -- wrapped around the C constructor of Ada_Giop_s

end Giop_S ;

