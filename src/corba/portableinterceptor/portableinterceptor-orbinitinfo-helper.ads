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
with CORBA.Object;
with PolyORB.Any;

package PortableInterceptor.ORBInitInfo.Helper is

   TC_ORBInitInfo : CORBA.TypeCode.Object;

   function Unchecked_To_Local_Ref
     (The_Ref : CORBA.Object.Ref'Class)
     return PortableInterceptor.ORBInitInfo.Local_Ref;

   function To_Local_Ref
     (The_Ref : CORBA.Object.Ref'Class)
     return PortableInterceptor.ORBInitInfo.Local_Ref;

   TC_ObjectId : CORBA.TypeCode.Object;

   function From_Any
     (Item : CORBA.Any)
     return PortableInterceptor.ORBInitInfo.ObjectId;

   function To_Any
     (Item : PortableInterceptor.ORBInitInfo.ObjectId)
     return CORBA.Any;

   TC_DuplicateName : CORBA.TypeCode.Object;

   function From_Any
     (Item : CORBA.Any)
     return PortableInterceptor.ORBInitInfo.DuplicateName_Members;

   function To_Any
     (Item : PortableInterceptor.ORBInitInfo.DuplicateName_Members)
     return CORBA.Any;

   procedure Raise_DuplicateName
     (Members : PortableInterceptor.ORBInitInfo.DuplicateName_Members);

   pragma No_Return (Raise_DuplicateName);

   TC_InvalidName : CORBA.TypeCode.Object;

   function From_Any
     (Item : CORBA.Any)
     return PortableInterceptor.ORBInitInfo.InvalidName_Members;

   function To_Any
     (Item : PortableInterceptor.ORBInitInfo.InvalidName_Members)
     return CORBA.Any;

   procedure Raise_InvalidName
     (Members : PortableInterceptor.ORBInitInfo.InvalidName_Members);

   pragma No_Return (Raise_InvalidName);

   
   package Internals is

      procedure Initialize_ORBInitInfo;

      function Wrap
        (X : access PortableInterceptor.ORBInitInfo.ObjectId)
        return PolyORB.Any.Content'Class;

      procedure Initialize_ObjectId;

      procedure Initialize_DuplicateName;

      procedure Initialize_InvalidName;

   end Internals;

end PortableInterceptor.ORBInitInfo.Helper;
