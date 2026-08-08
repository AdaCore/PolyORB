pragma Style_Checks ("NM32766");
pragma Warnings (Off, "use of an anonymous access type allocator");
pragma Warnings (Off, "unnecessary with of ancestor");

---------------------------------------------------
--  This file has been generated automatically from
--  /Users/heathdorn/Documents/Playground/Agents/RefactorTeam/code_refactor/PolyORB/idls/Misc/PortableInterceptor.idl
--  by IAC (IDL to Ada Compiler) 20.0w (rev. 41a9b833).
---------------------------------------------------
--  NOTE: If you modify this file by hand, your
--  changes will be lost when you re-run the
--  IDL to Ada compiler.
---------------------------------------------------

with PolyORB.Std;
with PolyORB.Any;
with PolyORB.Utils.Strings;
with PolyORB.Utils.Strings.Lists;
with PolyORB.Initialization;

package body PortableInterceptor.Interceptor.Helper is

   
   package body Internals is

      Interceptor_Initialized : PolyORB.Std.Boolean :=
        False;

      ----------------------------
      -- Initialize_Interceptor --
      ----------------------------

      procedure Initialize_Interceptor is
         Name_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("Interceptor");
         Id_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("IDL:omg.org/PortableInterceptor/Interceptor:1.0");
      begin
         if not Interceptor_Initialized
         then
            Interceptor_Initialized :=
              True;
            PortableInterceptor.Interceptor.Helper.TC_Interceptor :=
              CORBA.TypeCode.Internals.To_CORBA_Object
                 (PolyORB.Any.TypeCode.Tcf_Object);
            CORBA.Internals.Add_Parameter
              (TC_Interceptor,
               CORBA.To_Any
                 (Name_Ü));
            CORBA.Internals.Add_Parameter
              (TC_Interceptor,
               CORBA.To_Any
                 (Id_Ü));
            CORBA.TypeCode.Internals.Disable_Ref_Counting
              (TC_Interceptor);
            CORBA.TypeCode.Internals.Freeze
              (TC_Interceptor);
         end if;
      end Initialize_Interceptor;

   end Internals;

   ----------------------------
   -- Unchecked_To_Local_Ref --
   ----------------------------

   function Unchecked_To_Local_Ref
     (The_Ref : CORBA.Object.Ref'Class)
     return PortableInterceptor.Interceptor.Local_Ref
   is
      Result : PortableInterceptor.Interceptor.Local_Ref;
   begin
      Set
        (Result,
         CORBA.Object.Object_Of
           (The_Ref));
      return Result;
   end Unchecked_To_Local_Ref;

   ------------------
   -- To_Local_Ref --
   ------------------

   function To_Local_Ref
     (The_Ref : CORBA.Object.Ref'Class)
     return PortableInterceptor.Interceptor.Local_Ref
   is
   begin
      if (CORBA.Object.Is_Nil
        (The_Ref)
         or else CORBA.Object.Is_A
           (The_Ref,
            Repository_Id))
      then
         return Unchecked_To_Local_Ref
           (The_Ref);
      end if;
      CORBA.Raise_Bad_Param
        (CORBA.Default_Sys_Member);
   end To_Local_Ref;

   -----------------------------
   -- Deferred_Initialization --
   -----------------------------

   procedure Deferred_Initialization is
   begin
      PortableInterceptor.Interceptor.Helper.Internals.Initialize_Interceptor;
   end Deferred_Initialization;

begin
   declare
      use PolyORB.Utils.Strings;
      use PolyORB.Utils.Strings.Lists;
   begin
      PolyORB.Initialization.Register_Module
        (PolyORB.Initialization.Module_Info'
           (Name => +"PortableInterceptor.Interceptor.Helper",
            Conflicts => PolyORB.Utils.Strings.Lists.Empty,
            Depends => +"any",
            Provides => PolyORB.Utils.Strings.Lists.Empty,
            Implicit => False,
            Init => Deferred_Initialization'Access,
            Shutdown => null));
   end;
end PortableInterceptor.Interceptor.Helper;
