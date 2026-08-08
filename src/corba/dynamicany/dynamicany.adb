pragma Style_Checks ("NM32766");
pragma Warnings (Off, "use of an anonymous access type allocator");
pragma Warnings (Off, "unnecessary with of ancestor");

---------------------------------------------------
--  This file has been generated automatically from
--  /Users/heathdorn/Documents/Playground/Agents/RefactorTeam/code_refactor/PolyORB/idls/Misc/DynamicAny.idl
--  by IAC (IDL to Ada Compiler) 20.0w (rev. 41a9b833).
---------------------------------------------------
--  NOTE: If you modify this file by hand, your
--  changes will be lost when you re-run the
--  IDL to Ada compiler.
---------------------------------------------------

with PolyORB.Exceptions;

package body DynamicAny is

   -----------------
   -- Get_Members --
   -----------------

   procedure Get_Members
     (From : Ada.Exceptions.Exception_Occurrence;
      To : out DynamicAny.MustTruncate_Members)
   is
   begin
      PolyORB.Exceptions.User_Get_Members
        (From,
         To);
   end Get_Members;

end DynamicAny;
