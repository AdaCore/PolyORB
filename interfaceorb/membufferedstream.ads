-----------------------------------------------------------------------
----                                                               ----
----                  AdaBroker                                    ----
----                                                               ----
----     This package is wrapped around a C++ class whose name     ----
----   is Ada_memBufferedStream. (see Ada_memBufferedStream.hh)    ----
----     It provides two types of methods : the C functions        ----
----   of the Ada_memBufferedStream class and their equivalent     ----
----   in Ada. (he first ones have a C_ prefix.)                   ----
----     In addition, there is a raise_ada_exception function      ----
----   that allows C functions to raise the ada No_Initialisation  ----
----   exception.                                                  ----
----     At last, there is only one Init procedure in place of     ----
----   two in Ada_memBufferedStream since the second one is        ----
----   useless for AdaBroker.                                      ----
----                                                               ----
----                                                               ----
----                  package MemBufferedStream                    ----
----                                                               ----
----   authors : Sebastien Ponce                                   ----
----   date    : 02/26/99                                          ----
----                                                               ----
----                                                               ----
-----------------------------------------------------------------------


with Interfaces.C ;
with Interfaces.CPP ;
with Interfaces.C.Strings ;
with System ;
with Corba ;
with Sys_Dep ;
with Ada.Unchecked_Deallocation ;

package MemBufferedStream is

   type Object is tagged record
      Table : Interfaces.CPP.Vtable_Ptr ;
   end record ;

   pragma CPP_Class (Object);
   pragma CPP_Vtable (Object,Table,1);
   -- This object is wrapped around Ada_MemBufferedStream
   -- (see Ada_MemBufferedStream.hh)

   type Object_Ptr is access all Object ;
   -- just to give a name to pointers on Object

   procedure Free is new Ada.Unchecked_Deallocation(Object, Object_Ptr) ;
   -- to deallocate pointers

   procedure C_Init (Self : in Object'Class ;
                     Bufsize : in Interfaces.C.Unsigned_Long) ;
   pragma Import (C,C_Init,"__17MemBufferedStreamUi") ;
   -- wrapper around Ada_MemBufferedStream function Init
   -- (see Ada_MemBufferedStream.hh)
   -- name was changed to avoid conflict

   procedure Init (Self : in Object'Class ;
                   Bufsize : in Corba.Unsigned_Long) ;
   -- Ada equivalent of C procedure C_Init


   procedure C_Marshall_1 (A : in Interfaces.C.Char ;
                           S : in out System.Address) ;
   pragma Import (C,C_Marshall_1,"marshall__21Ada_memBufferedStreamUcR17MemBufferedStream") ;
   -- wrapper around Ada_MemBufferedStream function marshall
   -- (see Ada_MemBufferedStream.hh)
   -- name was changed to avoid conflict

   procedure Marshall (A : in Corba.Char ;
                       S : in out Object'Class);
   -- Ada equivalent of C procedure C_Marshall_1


   procedure C_UnMarshall_1 (A : out System.Address ;
                             S : in out System.Address) ;
   pragma Import (C,C_UnMarshall_1,"unmarshall__21Ada_memBufferedStreamRUcR17MemBufferedStream") ;
   -- wrapper around Ada_MemBufferedStream function marshall
   -- (see Ada_MemBufferedStream.hh)
   -- name was changed to avoid conflict

   procedure UnMarshall (A : out Corba.Char ;
                         S : in out Object'Class);
   -- Ada equivalent of C procedure C_UnMarshall_1


   procedure C_Marshall_2 (A : in Sys_Dep.C_Boolean ;
                           S : in out System.Address) ;
   pragma Import (C,C_Marshall_2,"marshall__21Ada_memBufferedStreambR17MemBufferedStream") ;
   -- wrapper around Ada_MemBufferedStream function marshall
   -- (see Ada_MemBufferedStream.hh)
   -- name was changed to avoid conflict

   procedure Marshall (A : in Corba.Boolean ;
                       S : in out Object'Class);
   -- Ada equivalent of C procedure C_Marshall_2


   procedure C_UnMarshall_2 (A : out System.Address ;
                             S : in out System.Address) ;
   pragma Import (C,C_UnMarshall_2,"unmarshall__21Ada_memBufferedStreamRbR17MemBufferedStream") ;
   -- wrapper around Ada_MemBufferedStream function marshall
   -- (see Ada_MemBufferedStream.hh)
   -- name was changed to avoid conflict

   procedure UnMarshall (A : out Corba.Boolean ;
                         S : in out Object'Class);
   -- Ada equivalent of C procedure C_UnMarshall_2


   procedure C_Marshall_3 (A : in Interfaces.C.Short ;
                           S : in out System.Address) ;
   pragma Import (C,C_Marshall_3,"marshall__21Ada_memBufferedStreamsR17MemBufferedStream") ;
   -- wrapper around Ada_MemBufferedStream function marshall
   -- (see Ada_MemBufferedStream.hh)
   -- name was changed to avoid conflict

   procedure Marshall (A : in Corba.Short ;
                       S : in out Object'Class);
   -- Ada equivalent of C procedure C_Marshall_3


   procedure C_UnMarshall_3 (A : out System.Address ;
                             S : in out System.Address) ;
   pragma Import (C,C_UnMarshall_3,"unmarshall__21Ada_memBufferedStreamRsR17MemBufferedStream") ;
   -- wrapper around Ada_MemBufferedStream function marshall
   -- (see Ada_MemBufferedStream.hh)
   -- name was changed to avoid conflict

   procedure UnMarshall (A : out Corba.Short ;
                         S : in out Object'Class);
   -- Ada equivalent of C procedure C_UnMarshall_3


   procedure C_Marshall_4 (A : in Interfaces.C.Unsigned_Short ;
                           S : in out System.Address) ;
   pragma Import (C,C_Marshall_4,"marshall__21Ada_memBufferedStreamUsR17MemBufferedStream") ;
   -- wrapper around Ada_MemBufferedStream function marshall
   -- (see Ada_MemBufferedStream.hh)
   -- name was changed to avoid conflict

   procedure Marshall (A : in Corba.Unsigned_Short ;
                       S : in out Object'Class);
   -- Ada equivalent of C procedure C_Marshall_4


   procedure C_UnMarshall_4 (A : out System.Address ;
                             S : in out System.Address) ;
   pragma Import (C,C_UnMarshall_4,"unmarshall__21Ada_memBufferedStreamRUsR17MemBufferedStream") ;
   -- wrapper around Ada_MemBufferedStream function marshall
   -- (see Ada_MemBufferedStream.hh)
   -- name was changed to avoid conflict

   procedure UnMarshall (A : out Corba.Unsigned_Short ;
                         S : in out Object'Class);
   -- Ada equivalent of C procedure C_UnMarshall_4


   procedure C_Marshall_5 (A : in Interfaces.C.Long ;
                           S : in out System.Address) ;
   pragma Import (C,C_Marshall_5,"marshall__21Ada_memBufferedStreamlR17MemBufferedStream") ;
   -- wrapper around Ada_MemBufferedStream function marshall
   -- (see Ada_MemBufferedStream.hh)
   -- name was changed to avoid conflict

   procedure Marshall (A : in Corba.Long ;
                       S : in out Object'Class);
   -- Ada equivalent of C procedure C_Marshall_5


   procedure C_UnMarshall_5 (A : out System.Address ;
                             S : in out System.Address) ;
   pragma Import (C,C_UnMarshall_5,"unmarshall__21Ada_memBufferedStreamRlR17MemBufferedStream") ;
   -- wrapper around Ada_MemBufferedStream function marshall
   -- (see Ada_MemBufferedStream.hh)
   -- name was changed to avoid conflict

   procedure UnMarshall (A : out Corba.Long ;
                         S : in out Object'Class);
   -- Ada equivalent of C procedure C_UnMarshall_5


   procedure C_Marshall_6 (A : in Interfaces.C.Unsigned_Long ;
                           S : in out System.Address) ;
   pragma Import (C,C_Marshall_6,"marshall__21Ada_memBufferedStreamUlR17MemBufferedStream") ;
   -- wrapper around Ada_MemBufferedStream function marshall
   -- (see Ada_MemBufferedStream.hh)
   -- name was changed to avoid conflict

   procedure Marshall (A : in Corba.Unsigned_Long ;
                       S : in out Object'Class);
   -- Ada equivalent of C procedure C_Marshall_6


   procedure C_UnMarshall_6 (A : out System.Address ;
                             S : in out System.Address) ;
   pragma Import (C,C_UnMarshall_6,"unmarshall__21Ada_memBufferedStreamRUlR17MemBufferedStream") ;
   -- wrapper around Ada_MemBufferedStream function marshall
   -- (see Ada_MemBufferedStream.hh)
   -- name was changed to avoid conflict

   procedure UnMarshall (A : out Corba.Unsigned_Long ;
                         S : in out Object'Class);
   -- Ada equivalent of C procedure C_UnMarshall_6


   procedure C_Marshall_7 (A : in Interfaces.C.C_Float ;
                           S : in out System.Address) ;
   pragma Import (C,C_Marshall_7,"marshall__21Ada_memBufferedStreamfR17MemBufferedStream") ;
   -- wrapper around Ada_MemBufferedStream function marshall
   -- (see Ada_MemBufferedStream.hh)
   -- name was changed to avoid conflict

   procedure Marshall (A : in Corba.Float ;
                       S : in out Object'Class);
   -- Ada equivalent of C procedure C_Marshall_7


   procedure C_UnMarshall_7 (A : out System.Address ;
                             S : in out System.Address) ;
   pragma Import (C,C_UnMarshall_7,"unmarshall__21Ada_memBufferedStreamRfR17MemBufferedStream") ;
   -- wrapper around Ada_MemBufferedStream function marshall
   -- (see Ada_MemBufferedStream.hh)
   -- name was changed to avoid conflict

   procedure UnMarshall (A : out Corba.Float ;
                         S : in out Object'Class);
   -- Ada equivalent of C procedure C_UnMarshall_7


   procedure C_Marshall_8 (A : in Interfaces.C.Double ;
                           S : in out System.Address) ;
   pragma Import (C,C_Marshall_8,"marshall__21Ada_memBufferedStreamdR17MemBufferedStream") ;
   -- wrapper around Ada_MemBufferedStream function marshall
   -- (see Ada_MemBufferedStream.hh)
   -- name was changed to avoid conflict

   procedure Marshall (A : in Corba.Double ;
                       S : in out Object'Class);
   -- Ada equivalent of C procedure C_Marshall_8


   procedure C_UnMarshall_8 (A : out System.Address ;
                             S : in out System.Address) ;
   pragma Import (C,C_UnMarshall_8,"unmarshall__21Ada_memBufferedStreamRdR17MemBufferedStream") ;
   -- wrapper around Ada_MemBufferedStream function marshall
   -- (see Ada_MemBufferedStream.hh)
   -- name was changed to avoid conflict

   procedure UnMarshall (A : out Corba.Double ;
                         S : in out Object'Class);
   -- Ada equivalent of C procedure C_UnMarshall_8


   --------------- For Corba.String ---------------------

   procedure Marshall (A : in Corba.String ;
                       S : in out Object'Class);

   procedure UnMarshall (A : out Corba.String ;
                         S : in out Object'Class);
   ----------------------------------------------------


private

   function Constructor return Object'Class;
   pragma CPP_Constructor (Constructor);
   pragma Import (CPP,Constructor,"__21Ada_memBufferedStream");
   -- wrapped around the C constructor of Rope

end MemBufferedStream ;

