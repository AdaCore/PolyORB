pragma Style_Checks ("NM32766");
pragma Wide_Character_Encoding (Brackets);
pragma Warnings (Off, "use of an anonymous access type allocator");

---------------------------------------------------
--  This file has been generated automatically from
--  /Users/heathdorn/Documents/Playground/Agents/RefactorTeam/code_refactor/PolyORB/idls/Misc/PortableInterceptor.idl
--  by IAC (IDL to Ada Compiler) 20.0w (rev. 41a9b833).
---------------------------------------------------
--  NOTE: If you modify this file by hand, your
--  changes will be lost when you re-run the
--  IDL to Ada compiler.
---------------------------------------------------

with CORBA;
pragma Elaborate_All (CORBA);
with PolyORB.Any;

package PortableInterceptor.Helper is

   TC_ForwardRequest : CORBA.TypeCode.Object;

   function From_Any
     (Item : CORBA.Any)
     return PortableInterceptor.ForwardRequest_Members;

   function To_Any
     (Item : PortableInterceptor.ForwardRequest_Members)
     return CORBA.Any;

   procedure Raise_ForwardRequest
     (Members : PortableInterceptor.ForwardRequest_Members);

   pragma No_Return (Raise_ForwardRequest);

   TC_ReplyStatus : CORBA.TypeCode.Object;

   function From_Any
     (Item : CORBA.Any)
     return PortableInterceptor.ReplyStatus;

   function To_Any
     (Item : PortableInterceptor.ReplyStatus)
     return CORBA.Any;

   TC_SlotId : CORBA.TypeCode.Object;

   function From_Any
     (Item : CORBA.Any)
     return PortableInterceptor.SlotId;

   function To_Any
     (Item : PortableInterceptor.SlotId)
     return CORBA.Any;

   TC_InvalidSlot : CORBA.TypeCode.Object;

   function From_Any
     (Item : CORBA.Any)
     return PortableInterceptor.InvalidSlot_Members;

   function To_Any
     (Item : PortableInterceptor.InvalidSlot_Members)
     return CORBA.Any;

   procedure Raise_InvalidSlot
     (Members : PortableInterceptor.InvalidSlot_Members);

   pragma No_Return (Raise_InvalidSlot);

   TC_ServerId : CORBA.TypeCode.Object;

   function From_Any
     (Item : CORBA.Any)
     return PortableInterceptor.ServerId;

   function To_Any
     (Item : PortableInterceptor.ServerId)
     return CORBA.Any;

   TC_ORBId : CORBA.TypeCode.Object;

   function From_Any
     (Item : CORBA.Any)
     return PortableInterceptor.ORBId;

   function To_Any
     (Item : PortableInterceptor.ORBId)
     return CORBA.Any;

   TC_AdapterName : CORBA.TypeCode.Object;

   function From_Any
     (Item : CORBA.Any)
     return PortableInterceptor.AdapterName;

   function To_Any
     (Item : PortableInterceptor.AdapterName)
     return CORBA.Any;

   TC_ObjectId : CORBA.TypeCode.Object;

   function From_Any
     (Item : CORBA.Any)
     return PortableInterceptor.ObjectId;

   function To_Any
     (Item : PortableInterceptor.ObjectId)
     return CORBA.Any;

   TC_AdapterManagerId : CORBA.TypeCode.Object;

   function From_Any
     (Item : CORBA.Any)
     return PortableInterceptor.AdapterManagerId;

   function To_Any
     (Item : PortableInterceptor.AdapterManagerId)
     return CORBA.Any;

   TC_AdapterState : CORBA.TypeCode.Object;

   function From_Any
     (Item : CORBA.Any)
     return PortableInterceptor.AdapterState;

   function To_Any
     (Item : PortableInterceptor.AdapterState)
     return CORBA.Any;

   
   package Internals is

      procedure Initialize_ForwardRequest;

      function Wrap
        (X : access PortableInterceptor.ReplyStatus)
        return PolyORB.Any.Content'Class;

      procedure Initialize_ReplyStatus;

      function Wrap
        (X : access PortableInterceptor.SlotId)
        return PolyORB.Any.Content'Class;

      procedure Initialize_SlotId;

      procedure Initialize_InvalidSlot;

      function Wrap
        (X : access PortableInterceptor.ServerId)
        return PolyORB.Any.Content'Class;

      procedure Initialize_ServerId;

      function Wrap
        (X : access PortableInterceptor.ORBId)
        return PolyORB.Any.Content'Class;

      procedure Initialize_ORBId;

      procedure Initialize_AdapterName;

      procedure Initialize_ObjectId;

      function Wrap
        (X : access PortableInterceptor.AdapterManagerId)
        return PolyORB.Any.Content'Class;

      procedure Initialize_AdapterManagerId;

      function Wrap
        (X : access PortableInterceptor.AdapterState)
        return PolyORB.Any.Content'Class;

      procedure Initialize_AdapterState;

   end Internals;

end PortableInterceptor.Helper;
