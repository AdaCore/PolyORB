--  This package is wrapped around the C++ class Ada_Giop_s declared in
--  Ada_Giop_s.hh.  It provides the same functions as this package plus the
--  Ada version of thouse where arguments types are to be change.  It does
--  not include an Init function since it is useless for
--  AdaBroker. (AdaBroker never creates a Giop_s object)

with Interfaces.C;
with Interfaces.CPP;

with CORBA;

with GIOP;
with Sys_Dep;
with NetBufferedStream;

package GIOP_S is

   type Object is new NetBufferedStream.Object with record
      Table1 : Interfaces.CPP.Vtable_Ptr;
   end record;
   --  Table1 (Ada) : needed to interface C++ and Ada

   pragma Cpp_Class  (Object);
   pragma Cpp_Vtable (Object, Table1, 1);
   --  This type is both a C and an Ada class it is wrapped around
   --  Ada_Giop_s (see Ada_Giop_s.hh)


   type Object_Ptr is access all Object;
   --  Type pointer on type Object


   function Reply_Header_Size return CORBA.Unsigned_Long;
   --  Compute the size of the header for a reply


   procedure Request_Received
     (Self : in out Object'Class;
      Skip : in Boolean := False);
   --  Informs the ORB that the request was received (see giopDriver.h L150
   --  for more details)


   procedure Initialize_Reply
     (Self    : in out Object'Class;
      Status  : in GIOP.Reply_Status_Type;
      MsgSize : in CORBA.Unsigned_Long);
   --  Initialisation of a reply (see giopDriver.h L150 for more details)


   procedure Reply_Completed (Self : in out Object'Class);
   pragma Import (CPP, Reply_Completed, "ReplyCompleted__10Ada_Giop_s");
   --  Wrapper around Ada_Giop_s procedure InitialiseReply (see
   --  Ada_Giop_s.hh) it is both a C and an Ada procedure.  it informs the
   --  ORB that the reply was completed (see giopDriver.h L150 for more
   --  details)

private

   function Constructor return Object'Class;
   pragma Cpp_Constructor (Constructor);
   pragma Import (CPP, Constructor, "__10Ada_Giop_s");
   --  Default constructor of the C class. Actually, this constructor does
   --  nothing.

end GIOP_S;

