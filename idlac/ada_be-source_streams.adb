--  A stream type suitable for generation of Ada source code.
--  $Id: //depot/adabroker/main/idlac/ada_be-source_streams.adb#4 $

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Ada.Unchecked_Deallocation;
with Ada.Text_IO;

package body Ada_Be.Source_Streams is

   procedure Set_Empty (Unit : in out Compilation_Unit) is
   begin
      Unit.Empty := True;
   end Set_Empty;

   procedure Put
     (Unit : in out Compilation_Unit;
      Text : String)
   is
      Indent_String : constant String
        (1 .. Unit.Indent_Level * Indent_Size)
        := (others => ' ');
   begin
      Unit.Empty := False;
      if Unit.At_BOL then
         Append (Unit.Library_Item, Indent_String);
         Unit.At_BOL := False;
      end if;
      Append (Unit.Library_Item, Text);
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
      Append (Unit.Library_Item, LF);
      Unit.At_BOL := True;
   end New_Line;

   procedure Inc_Indent (Unit : in out Compilation_Unit) is
   begin
      Unit.Indent_Level := Unit.Indent_Level + 1;
   end Inc_Indent;

   procedure Dec_Indent (Unit : in out Compilation_Unit) is
   begin
      Unit.Indent_Level := Unit.Indent_Level - 1;
   end Dec_Indent;

   procedure Add_With
     (Unit   : in out Compilation_Unit;
      Dep    : String;
      Use_It : Boolean := False;
      Elaborate : Boolean := False)
   is
      Dep_Node : Dependency := Unit.Context_Clause;
   begin
      while Dep_Node /= null and then Dep_Node.Library_Unit.all /= Dep loop
         Dep_Node := Dep_Node.Next;
      end loop;

      if Dep_Node = null then
         Dep_Node := new Dependency_Node'
           (Library_Unit => new String'(Dep),
            Use_It => Use_It,
            Elaborate => Elaborate,
            Next => Unit.Context_Clause);
         Unit.Context_Clause := Dep_Node;
      else
         Dep_Node.Use_It := Dep_Node.Use_It or else Use_It;
         Dep_Node.Elaborate := Dep_Node.Elaborate or else Elaborate;
      end if;

   end Add_With;

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
     (Unit : Compilation_Unit;
      Is_Generic_Instanciation : Boolean := False)
   is
      function Ada_File_Name
        (Full_Name : String;
         Part      : Unit_Kind := Unit_Spec)
        return String;

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

      use Ada.Text_IO;

      File : File_Type;
      File_Name : String
        := Ada_File_Name (Unit.Library_Unit_Name.all, Unit.Kind);
      Dep_Node : Dependency := Unit.Context_Clause;
   begin
      if Unit.Empty then
         return;
      end if;

      Create (File, Out_File, File_Name);
      while Dep_Node /= null loop
         Put (File, "with " & Dep_Node.Library_Unit.all & ";");
         if Dep_Node.Use_It then
            Put_Line (File, " use " & Dep_Node.Library_Unit.all & ";");
         else
            New_Line (File);
         end if;
         if Dep_Node.Elaborate then
            Put_Line (File, "pragma Elaborate_All ("
                      & Dep_Node.Library_Unit.all & ");");
         end if;
         Dep_Node := Dep_Node.Next;
      end loop;

      if Unit.Context_Clause /= null then
         New_Line (File);
      end if;

      Put (File, "package ");
      if Unit.Kind = Unit_Body then
         Put (File, "body ");
      end if;
      Put_Line (File, Unit.Library_Unit_Name.all & " is");
      Put (File, To_String (Unit.Library_Item));

      if not Is_Generic_Instanciation then
         Put_Line (File, "end " & Unit.Library_Unit_Name.all & ";");
      end if;

      Close (File);
   end Generate;

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

end Ada_Be.Source_Streams;
