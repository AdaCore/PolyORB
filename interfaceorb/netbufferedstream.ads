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


with Interfaces.C ;
with Interfaces.CPP ;
with Interfaces.C.Strings ;
with System ;
with Corba ;
with Sys_Dep ;
with Rope ;

package NetBufferedStream is

   type Object is tagged record
      Table : Interfaces.CPP.Vtable_Ptr ;
   end record ;

   pragma CPP_Class (Object);
   pragma CPP_Vtable (Object,Table,1);
   -- This object is wrapped around Ada_netBufferedStream
   -- (see Ada_netBufferedStream.hh)

   type Object_Access is access Object ;
   -- just to give a name to pointers on Object

   procedure C_Init (Self : in Object'Class ;
                     r : in System.Address ;
                     RdLock : in Sys_Dep.C_Boolean ;
                     WrLock : in Sys_Dep.C_Boolean ;
                     Bufsize : in Interfaces.C.Unsigned_Long) ;
   pragma Import (C,C_Init,"__17NetBufferedStreamP4RopebT2Ui") ;
   -- wrapper around Ada_netBufferedStream function Init
   -- (see Ada_netBufferedStream.hh)
   -- name was changed to avoid conflict

   procedure Init (Self : in Object'Class ;
                   r : in Rope.Object ;
                   Rdlock : in Corba.Boolean ;
                   WrLock : in Corba.Boolean ;
                   Bufsize : in Corba.Unsigned_Long) ;
   -- Ada equivalent of C procedure C_Init


   procedure C_Marshall_1 (A : in Interfaces.C.Char ;
                           S : in out System.Address) ;
   pragma Import (C,C_Marshall_1,"marshall__21Ada_netBufferedStreamUcR17NetBufferedStream") ;
   -- wrapper around Ada_netBufferedStream function marshall
   -- (see Ada_netBufferedStream.hh)
   -- name was changed to avoid conflict

   procedure Marshall (A : in Corba.Char ;
                       S : in out Object'Class);
   -- Ada equivalent of C procedure C_Marshall_1


   procedure C_UnMarshall_1 (A : out System.Address ;
                             S : in out System.Address) ;
   pragma Import (C,C_UnMarshall_1,"unmarshall__21Ada_netBufferedStreamRUcR17NetBufferedStream") ;
   -- wrapper around Ada_netBufferedStream function marshall
   -- (see Ada_netBufferedStream.hh)
   -- name was changed to avoid conflict

   procedure UnMarshall (A : out Corba.Char ;
                         S : in out Object'Class);
   -- Ada equivalent of C procedure C_UnMarshall_1


   procedure C_Marshall_2 (A : in Sys_Dep.C_Boolean ;
                           S : in out System.Address) ;
   pragma Import (C,C_Marshall_2,"marshall__21Ada_netBufferedStreambR17NetBufferedStream") ;
   -- wrapper around Ada_netBufferedStream function marshall
   -- (see Ada_netBufferedStream.hh)
   -- name was changed to avoid conflict

   procedure Marshall (A : in Corba.Boolean ;
                       S : in out Object'Class);
   -- Ada equivalent of C procedure C_Marshall_2


   procedure C_UnMarshall_2 (A : out System.Address ;
                             S : in out System.Address) ;
   pragma Import (C,C_UnMarshall_2,"unmarshall__21Ada_netBufferedStreamRbR17NetBufferedStream") ;
   -- wrapper around Ada_netBufferedStream function marshall
   -- (see Ada_netBufferedStream.hh)
   -- name was changed to avoid conflict

   procedure UnMarshall (A : out Corba.Boolean ;
                         S : in out Object'Class);
   -- Ada equivalent of C procedure C_UnMarshall_2


   procedure C_Marshall_3 (A : in Interfaces.C.Short ;
                           S : in out System.Address) ;
   pragma Import (C,C_Marshall_3,"marshall__21Ada_netBufferedStreamsR17NetBufferedStream") ;
   -- wrapper around Ada_netBufferedStream function marshall
   -- (see Ada_netBufferedStream.hh)
   -- name was changed to avoid conflict

   procedure Marshall (A : in Corba.Short ;
                       S : in out Object'Class);
   -- Ada equivalent of C procedure C_Marshall_3


   procedure C_UnMarshall_3 (A : out System.Address ;
                             S : in out System.Address) ;
   pragma Import (C,C_UnMarshall_3,"unmarshall__21Ada_netBufferedStreamRsR17NetBufferedStream") ;
   -- wrapper around Ada_netBufferedStream function marshall
   -- (see Ada_netBufferedStream.hh)
   -- name was changed to avoid conflict

   procedure UnMarshall (A : out Corba.Short ;
                         S : in out Object'Class);
   -- Ada equivalent of C procedure C_UnMarshall_3


   procedure C_Marshall_4 (A : in Interfaces.C.Unsigned_Short ;
                           S : in out System.Address) ;
   pragma Import (C,C_Marshall_4,"marshall__21Ada_netBufferedStreamUsR17NetBufferedStream") ;
   -- wrapper around Ada_netBufferedStream function marshall
   -- (see Ada_netBufferedStream.hh)
   -- name was changed to avoid conflict

   procedure Marshall (A : in Corba.Unsigned_Short ;
                       S : in out Object'Class);
   -- Ada equivalent of C procedure C_Marshall_4


   procedure C_UnMarshall_4 (A : out System.Address ;
                             S : in out System.Address) ;
   pragma Import (C,C_UnMarshall_4,"unmarshall__21Ada_netBufferedStreamRUsR17NetBufferedStream") ;
   -- wrapper around Ada_netBufferedStream function marshall
   -- (see Ada_netBufferedStream.hh)
   -- name was changed to avoid conflict

   procedure UnMarshall (A : out Corba.Unsigned_Short ;
                         S : in out Object'Class);
   -- Ada equivalent of C procedure C_UnMarshall_4


   procedure C_Marshall_5 (A : in Interfaces.C.Long ;
                           S : in out System.Address) ;
   pragma Import (C,C_Marshall_5,"marshall__21Ada_netBufferedStreamlR17NetBufferedStream") ;
   -- wrapper around Ada_netBufferedStream function marshall
   -- (see Ada_netBufferedStream.hh)
   -- name was changed to avoid conflict

   procedure Marshall (A : in Corba.Long ;
                       S : in out Object'Class);
   -- Ada equivalent of C procedure C_Marshall_5


   procedure C_UnMarshall_5 (A : out System.Address ;
                             S : in out System.Address) ;
   pragma Import (C,C_UnMarshall_5,"unmarshall__21Ada_netBufferedStreamRlR17NetBufferedStream") ;
   -- wrapper around Ada_netBufferedStream function marshall
   -- (see Ada_netBufferedStream.hh)
   -- name was changed to avoid conflict

   procedure UnMarshall (A : out Corba.Long ;
                         S : in out Object'Class);
   -- Ada equivalent of C procedure C_UnMarshall_5


   procedure C_Marshall_6 (A : in Interfaces.C.Unsigned_Long ;
                           S : in out System.Address) ;
   pragma Import (C,C_Marshall_6,"marshall__21Ada_netBufferedStreamUlR17NetBufferedStream") ;
   -- wrapper around Ada_netBufferedStream function marshall
   -- (see Ada_netBufferedStream.hh)
   -- name was changed to avoid conflict

   procedure Marshall (A : in Corba.Unsigned_Long ;
                       S : in out Object'Class);
   -- Ada equivalent of C procedure C_Marshall_6


   procedure C_UnMarshall_6 (A : out System.Address ;
                             S : in out System.Address) ;
   pragma Import (C,C_UnMarshall_6,"unmarshall__21Ada_netBufferedStreamRUlR17NetBufferedStream") ;
   -- wrapper around Ada_netBufferedStream function marshall
   -- (see Ada_netBufferedStream.hh)
   -- name was changed to avoid conflict

   procedure UnMarshall (A : out Corba.Unsigned_Long ;
                         S : in out Object'Class);
   -- Ada equivalent of C procedure C_UnMarshall_6


   procedure C_Marshall_7 (A : in Interfaces.C.C_Float ;
                           S : in out System.Address) ;
   pragma Import (C,C_Marshall_7,"marshall__21Ada_netBufferedStreamfR17NetBufferedStream") ;
   -- wrapper around Ada_netBufferedStream function marshall
   -- (see Ada_netBufferedStream.hh)
   -- name was changed to avoid conflict

   procedure Marshall (A : in Corba.Float ;
                       S : in out Object'Class);
   -- Ada equivalent of C procedure C_Marshall_7


   procedure C_UnMarshall_7 (A : out System.Address ;
                             S : in out System.Address) ;
   pragma Import (C,C_UnMarshall_7,"unmarshall__21Ada_netBufferedStreamRfR17NetBufferedStream") ;
   -- wrapper around Ada_netBufferedStream function marshall
   -- (see Ada_netBufferedStream.hh)
   -- name was changed to avoid conflict

   procedure UnMarshall (A : out Corba.Float ;
                         S : in out Object'Class);
   -- Ada equivalent of C procedure C_UnMarshall_7


   procedure C_Marshall_8 (A : in Interfaces.C.Double ;
                           S : in out System.Address) ;
   pragma Import (C,C_Marshall_8,"marshall__21Ada_netBufferedStreamdR17NetBufferedStream") ;
   -- wrapper around Ada_netBufferedStream function marshall
   -- (see Ada_netBufferedStream.hh)
   -- name was changed to avoid conflict

   procedure Marshall (A : in Corba.Double ;
                       S : in out Object'Class);
   -- Ada equivalent of C procedure C_Marshall_8


   procedure C_UnMarshall_8 (A : out System.Address ;
                             S : in out System.Address) ;
   pragma Import (C,C_UnMarshall_8,"unmarshall__21Ada_netBufferedStreamRdR17NetBufferedStream") ;
   -- wrapper around Ada_netBufferedStream function marshall
   -- (see Ada_netBufferedStream.hh)
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


   function C_Is_Reusing_Existing_Connection (Self : in Object'Class)
                                              return Sys_Dep.C_Boolean;
   pragma Import (C,C_Is_Reusing_Existing_Connection,"isReUsingExistingConnection__CQ26Strand4Sync") ;
   -- wrapper around     _CORBA_Boolean isReUsingExistingConnection() const;
   -- (see rope.h L 395)

   function Is_Reusing_Existing_Connection (Self : in Object'Class)
                                            return CORBA.Boolean;
   -- Ada equivalent of C procedure C_Is_Reusing_Existing_Connection


private

   function Constructor return Object'Class;
   pragma CPP_Constructor (Constructor);
   pragma Import (CPP,Constructor,"__21Ada_netBufferedStream");
   -- wrapped around the C constructor of Rope

end NetBufferedStream ;






