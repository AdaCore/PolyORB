----------------------------------------
--                                    --
--       ----  ---     --  ----       --
--       -      -     - -  -  -       --
--       -      -    ----  -  -       --
--       ----  ---  -   -  ----       --
--                                    --
----------------------------------------
--  CORBA                             --
--  Interface for                     --
--  Ada'95 distributed systems annex  --
--  Objects                           --
----------------------------------------
--  Copyright (c) 1999                --
--  Icole nationale supirieure des    --
--  tilicommunications                --
----------------------------------------

--  An abstraction for the production of the text of
--  an Ada 95 compilation unit.
--  $Id: //depot/ciao/main/ciao-ada_source_streams.adb#3 $

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Ada.Unchecked_Deallocation;
with Ada.Text_IO;

package body CIAO.Ada_Source_Streams is

   --  Semantic dependencies

   type Dependency_Node is record
      Library_Unit : String_Ptr;
      Use_It : Boolean := False;
      Elab_Control : Elab_Control_Pragma := None;
      Next : Dependency;
   end record;

   function Is_Ancestor
     (U1 : String;
      U2 : String)
     return Boolean;
   --  True if library unit U1 is an ancestor of U2.

   function Is_Ancestor
     (U1 : String;
      U2 : String)
     return Boolean
   is
      use Ada.Characters.Handling;

      LU1 : constant String
        := To_Lower (U1) & ".";
      LU2 : constant String
        := To_Lower (U2);
   begin
      return True
        and then LU1'Length <= LU2'Length
        and then LU1 = LU2
        (LU2'First .. LU2'First + LU1'Length - 1);
   end Is_Ancestor;

   procedure Add_With
     (Unit   : in out Compilation_Unit;
      Dep    : String;
      Use_It : Boolean := False;
      Elab_Control : Elab_Control_Pragma := None)
   is
      Dep_Node : Dependency := Unit.Context_Clause;
      LU_Name : constant String
        := Unit.Library_Unit_Name.all;
   begin
      if False
        or else Dep = LU_Name
        or else Is_Ancestor (Dep, LU_Name)
      then
         --  No need to with oneself or one's ancestor.
         return;
      end if;

      if True
        and then Unit.Kind = Unit_Spec
        and then Is_Ancestor (LU_Name, Dep)
      then
         --  All hope abandon he who trieth to make a unit
         --  spec depend upon its child.

         raise Program_Error;
      end if;

      while Dep_Node /= null and then Dep_Node.Library_Unit.all /= Dep loop
         Dep_Node := Dep_Node.Next;
      end loop;

      if Dep_Node = null then
         Dep_Node := new Dependency_Node'
           (Library_Unit => new String'(Dep),
            Use_It => Use_It,
            Elab_Control => Elab_Control,
            Next => Unit.Context_Clause);
         Unit.Context_Clause := Dep_Node;
      else
         Dep_Node.Use_It := Dep_Node.Use_It or else Use_It;
         if Elab_Control = Elaborate_All
           or else Dep_Node.Elab_Control = Elaborate_All then
            Dep_Node.Elab_Control := Elaborate_All;
         elsif Elab_Control = Elaborate
           or else Dep_Node.Elab_Control = Elaborate then
            Dep_Node.Elab_Control := Elaborate;
         else
            Dep_Node.Elab_Control := None;
         end if;
      end if;

   end Add_With;

   procedure Add_Elaborate_Body (Unit : in out Compilation_Unit) is
   begin
      pragma Assert (Unit.Kind = Unit_Spec);
      Unit.Diversions (Visible_Declarations).Empty := False;
      Unit.Elaborate_Body := True;
   end Add_Elaborate_Body;

   procedure Suppress_Warning_Message (Unit : in out Compilation_Unit) is
   begin
      Unit.No_Warning := True;
   end Suppress_Warning_Message;

   --  Source streams (global)

   procedure Divert
     (CU     : in out Compilation_Unit;
      Whence : Diversion) is
   begin
      if not (False
        or else Whence = Visible_Declarations
        or else (Whence = Private_Declarations and then CU.Kind = Unit_Spec)
        or else (Whence = Elaboration and then CU.Kind = Unit_Body)
        or else (Whence = Generic_Formals and then CU.Kind = Unit_Spec))
      then
         raise Program_Error;
      end if;

      CU.Current_Diversion := Whence;
   end Divert;

   function New_Package
     (Name : String;
      Kind : Unit_Kind)
     return Compilation_Unit
   is
      The_Package : Compilation_Unit;
   begin
      The_Package.Library_Unit_Name := new String'(Name);
      The_Package.Kind := Kind;

      return The_Package;
   end New_Package;


   procedure Generate
     (Unit : in Compilation_Unit;
      Is_Generic_Instanciation : Boolean := False;
      To_Stdout : Boolean := False)
   is

      --  Helper subprograms for Generate

      function Ada_File_Name
        (Full_Name : String;
         Part      : Unit_Kind := Unit_Spec)
        return String;
      --  The name of the file that contains Unit.

      function Is_Empty return Boolean;
      --  True if, and only if, any of Unit's diversions
      --  is not empty.

      function Ada_File_Name
        (Full_Name : String;
         Part      : Unit_Kind := Unit_Spec)
        return String
      is
         Extension : constant array (Unit_Kind) of Character
           := (Unit_Spec => 's',
               Unit_Body => 'b');
         Result : String := Full_Name & ".ad?";
      begin
         for I in Result'First .. Result'Last - 4 loop
            if Result (I) = '.' then
               Result (I) := '-';
            else
               Result (I) := To_Lower (Result (I));
            end if;
         end loop;

         Result (Result'Last) := Extension (Part);
         return Result;
      end Ada_File_Name;

      function Is_Empty return Boolean is
      begin
         for I in Unit.Diversions'Range loop
            if not Unit.Diversions (I).Empty then
               return False;
            end if;
         end loop;

         return True;
      end Is_Empty;

      use Ada.Text_IO;

      procedure Emit_Standard_Header
        (File        : in File_Type;
         User_Edited : in Boolean := False);

      procedure Emit_Source_Code
        (File : in File_Type);

      procedure Emit_Standard_Header
        (File        : in File_Type;
         User_Edited : in Boolean := False)
      is
      begin
         Put_Line (File, "-------------------------------------------------------");
         Put_Line (File, "--  This file has been generated automatically by CIAO.");
         if not User_Edited then
            Put_Line (File, "--");
            Put_Line (File, "--  Do NOT hand-modify this file, as your");
            Put_Line (File, "--  changes will be lost when you re-run the");
            Put_Line (File, "--  IDL to Ada compiler.");
         end if;
         Put_Line (File, "-------------------------------------------------------");
         New_Line (File);
      end Emit_Standard_Header;

      procedure Emit_Source_Code
        (File : in File_Type)
      is
         Dep_Node : Dependency := Unit.Context_Clause;
      begin
         while Dep_Node /= null loop
            Put (File, "with " & Dep_Node.Library_Unit.all & ";");
            if Dep_Node.Use_It then
               Put_Line (File, " use " & Dep_Node.Library_Unit.all & ";");
            else
               New_Line (File);
            end if;
            case Dep_Node.Elab_Control is
               when Elaborate_All =>
                  Put_Line (File, "pragma Elaborate_All ("
                            & Dep_Node.Library_Unit.all & ");");
               when Elaborate =>
                  Put_Line (File, "pragma Elaborate ("
                            & Dep_Node.Library_Unit.all & ");");
               when None =>
                  null;
            end case;
            Dep_Node := Dep_Node.Next;
         end loop;

         if Unit.Context_Clause /= null then
            New_Line (File);
         end if;

         if not Unit.Diversions (Generic_Formals).Empty then
            Put_Line (File, "generic");
            Put (File, To_String
                 (Unit.Diversions (Generic_Formals).Library_Item));
            New_Line (File);
         end if;

         Put (File, "package ");
         if Unit.Kind = Unit_Body then
            Put (File, "body ");
         end if;
         Put_Line (File, Unit.Library_Unit_Name.all & " is");

         if Unit.Elaborate_Body then
            New_Line (File);
            Put_Line (File, "   pragma Elaborate_Body;");
         end if;

         if not Unit.Diversions (Visible_Declarations).Empty then
            Put (File, To_String
                 (Unit.Diversions (Visible_Declarations).Library_Item));
         end if;

         if not Unit.Diversions (Private_Declarations).Empty then
            New_Line (File);
            Put_Line (File, "private");
            Put (File, To_String
                 (Unit.Diversions (Private_Declarations).Library_Item));
         end if;

         if not Unit.Diversions (Elaboration).Empty then
            New_Line (File);
            Put_Line (File, "begin");
            Put (File, To_String (Unit.Diversions (Elaboration).Library_Item));
         end if;

         if not Is_Generic_Instanciation then
            New_Line (File);
            Put_Line (File, "end " & Unit.Library_Unit_Name.all & ";");
         end if;
      end Emit_Source_Code;

   begin
      if Is_Empty then
         return;
      end if;

      if To_Stdout then
         Emit_Standard_Header (Current_Output, Unit.No_Warning);
         Emit_Source_Code (Current_Output);
      else
         declare
            File_Name : String
              := Ada_File_Name (Unit.Library_Unit_Name.all, Unit.Kind);
            File : File_Type;
         begin
            Create (File, Out_File, File_Name);
            Emit_Standard_Header (File, Unit.No_Warning);
            Emit_Source_Code (File);
            Close (File);
         end;
      end if;
   end Generate;

   --  Source code streams (diversion specific)

   procedure Set_Empty (Unit : in out Compilation_Unit) is
   begin
      Unit.Diversions (Unit.Current_Diversion).Empty := True;
   end Set_Empty;

   procedure Put
     (Unit : in out Compilation_Unit;
      Text : String)
   is
      Indent_String : constant String
        (1 .. Indent_Size
           * Unit.Diversions (Unit.Current_Diversion).Indent_Level)
          := (others => ' ');
   begin
      Unit.Diversions (Unit.Current_Diversion).Empty := False;
      if Unit.Diversions (Unit.Current_Diversion).At_BOL then
         Append
           (Unit.Diversions (Unit.Current_Diversion).Library_Item,
            Indent_String);
         Unit.Diversions (Unit.Current_Diversion).At_BOL := False;
      end if;
      Append (Unit.Diversions (Unit.Current_Diversion).Library_Item, Text);
   end Put;

   procedure Put_Line
     (Unit : in out Compilation_Unit;
      Line : String) is
   begin
      Put (Unit, Line);
      New_Line (Unit);
   end Put_Line;

   procedure New_Line (Unit : in out Compilation_Unit) is
   begin
      Append (Unit.Diversions (Unit.Current_Diversion).Library_Item, LF);
      Unit.Diversions (Unit.Current_Diversion).At_BOL := True;
   end New_Line;

   procedure Inc_Indent (Unit : in out Compilation_Unit) is
   begin
      Unit.Diversions (Unit.Current_Diversion).Indent_Level
      := Unit.Diversions (Unit.Current_Diversion).Indent_Level + 1;
   end Inc_Indent;

   procedure Dec_Indent (Unit : in out Compilation_Unit) is
   begin
      Unit.Diversions (Unit.Current_Diversion).Indent_Level
        := Unit.Diversions (Unit.Current_Diversion).Indent_Level - 1;
   end Dec_Indent;

   --  Finalization

   procedure Free is
      new Ada.Unchecked_Deallocation (String, String_Ptr);
   procedure Free is
      new Ada.Unchecked_Deallocation (Dependency_Node, Dependency);

   procedure Finalize (Object : in out Dependency_Node);

   procedure Finalize (Object : in out Dependency_Node) is
   begin
      if Object.Next /= null then
         Finalize (Object.Next.all);
         Free (Object.Next);
      end if;
      Free (Object.Library_Unit);
   end Finalize;

   procedure Finalize (Object : in out Compilation_Unit) is
   begin
      if Object.Context_Clause /= null then
         Finalize (Object.Context_Clause.all);
         Free (Object.Context_Clause);
      end if;
   end Finalize;

end CIAO.Ada_Source_Streams;
