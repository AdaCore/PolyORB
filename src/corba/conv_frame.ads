pragma Style_Checks ("NM32766");
pragma Wide_Character_Encoding (Brackets);
pragma Warnings (Off, "use of an anonymous access type allocator");

---------------------------------------------------
--  This file has been generated automatically from
--  /Users/heathdorn/Documents/Playground/Agents/RefactorTeam/code_refactor/PolyORB/idls/Interop/CONV_FRAME.idl
--  by IAC (IDL to Ada Compiler) 20.0w (rev. 41a9b833).
---------------------------------------------------
--  NOTE: If you modify this file by hand, your
--  changes will be lost when you re-run the
--  IDL to Ada compiler.
---------------------------------------------------

--  Source: 
--  /Users/heathdorn/Documents/Playground/Agents/RefactorTeam/code_refactor/PolyORB/idls/Interop/CONV_FRAME.idl

--  // File: CONV_FRAME.idl
--  // From CORBA 3.0: Chapter 13, Interoperability Architecture

--  // PolyORB:WAGCORBA This file has been update to take into acocunt OMG
--  // Issue 5232 (anonymous sequence types are deprecated).

--  #ifndef _CONV_FRAME_IDL
--  #define _CONV_FRAME_IDL

--  #ifdef _PRE_3_0_COMPILER_
--  #pragma prefix "omg.org"
--  #endif

--  module CONV_FRAME {

--  #ifndef _PRE_3_0_COMPILER_
--      typeprefix CONV_FRAME "omg.org";
--  #endif

--      typedef unsigned long CodeSetId;
--      typedef sequence<CodeSetId> CodeSetIdSeq;

--      struct CodeSetComponent {
--          CodeSetId               native_code_set;
--          CodeSetIdSeq            conversion_code_sets;
--      };
--      struct CodeSetComponentInfo {
--          CodeSetComponent        ForCharData;
--          CodeSetComponent        ForWcharData;
--      };

--      // CodeSet Service Context information

--      struct CodeSetContext {
--          CodeSetId               char_data;
--          CodeSetId               wchar_data;
--      };
--  };
--  #endif // _CONV_FRAME_IDL


--  End source: 
--  /Users/heathdorn/Documents/Playground/Agents/RefactorTeam/code_refactor/PolyORB/idls/Interop/CONV_FRAME.idl
--   -- 41 lines

---------------------------------------------------

with PolyORB.Std;
with CORBA;
pragma Elaborate_All (CORBA);
with CORBA.Sequences.Unbounded;

package CONV_FRAME is

   Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/CONV_FRAME:1.0";

   type CodeSetId is
     new CORBA.Unsigned_Long;

   CodeSetId_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/CONV_FRAME/CodeSetId:1.0";

   package IDL_SEQUENCE_CONV_FRAME_CodeSetId is
     new CORBA.Sequences.Unbounded
        (CONV_FRAME.CodeSetId);

   type CodeSetIdSeq is
     new CONV_FRAME.IDL_SEQUENCE_CONV_FRAME_CodeSetId.Sequence;

   CodeSetIdSeq_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/CONV_FRAME/CodeSetIdSeq:1.0";

   type CodeSetComponent is
     record
         native_code_set : CONV_FRAME.CodeSetId;
         conversion_code_sets : CONV_FRAME.CodeSetIdSeq;
      end record;

   CodeSetComponent_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/CONV_FRAME/CodeSetComponent:1.0";

   type CodeSetComponentInfo is
     record
         ForCharData : CONV_FRAME.CodeSetComponent;
         ForWcharData : CONV_FRAME.CodeSetComponent;
      end record;

   CodeSetComponentInfo_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/CONV_FRAME/CodeSetComponentInfo:1.0";

   type CodeSetContext is
     record
         char_data : CONV_FRAME.CodeSetId;
         wchar_data : CONV_FRAME.CodeSetId;
      end record;

   CodeSetContext_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/CONV_FRAME/CodeSetContext:1.0";

end CONV_FRAME;
