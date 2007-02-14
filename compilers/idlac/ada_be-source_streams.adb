------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                A D A _ B E . S O U R C E _ S T R E A M S                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2001-2007, Free Software Foundation, Inc.          --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
-- not, write to the Free Software Foundation, 51 Franklin Street, Fifth    --
-- Floor, Boston, MA 02111-1301, USA.                                       --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;

with GNAT.OS_Lib;
with GNAT.Directory_Operations;

with Ada_Be.Debug;
pragma Elaborate_All (Ada_Be.Debug);

with Output;
with Platform;

package body Ada_Be.Source_Streams is

   Flag : constant Natural := Ada_Be.Debug.Is_Active ("ada_be.source_streams");
   procedure O is new Ada_Be.Debug.Output (Flag);

   --  User-defined diversion identifiers are allocated on a system-wide basis.

   Diversions_Allocation : array (Diversion) of Boolean
     := (Predefined_Diversions'Range => True, others => False);

   --  Semantic dependencies

   type Dependency_Node is record
      Library_Unit : String_Ptr;
      Use_It : Boolean;
      Elab_Control : Elab_Control_Pragma := None;
      No_Warnings : Boolean;
      Next : Dependency;
   end record;

   Output_Directory : Unbounded_String;

   -----------------------
   -- Local subprograms --
   -----------------------

   function Is_Empty (Unit : Compilation_Unit) return Boolean;
   --  True if, and only if, all of Unit's diversions are empty

   function Is_Ancestor
     (U1 : String;
      U2 : String)
     return Boolean;
   --  True if library unit U1 is an ancestor of U2.

   function Find_Dep
     (Unit            : String;
      Context_Clauses : Dependency) return Dependency;
   --  Retrieve the node for Unit in the given context clauses list (null if
   --  not found).

   -----------------
   -- Is_Ancestor --
   -----------------

   function Is_Ancestor (U1 : String; U2 : String) return Boolean is
      LU1 : constant String := To_Lower (U1) & ".";
      LU2 : constant String := To_Lower (U2);
   begin
      return True
        and then LU1'Length <= LU2'Length
        and then LU1 = LU2
        (LU2'First .. LU2'First + LU1'Length - 1);
   end Is_Ancestor;

   --------------
   -- Add_With --
   --------------

   procedure Add_With
     (Unit         : in out Compilation_Unit;
      Dep          :        String;
      Use_It       :        Boolean             := False;
      Elab_Control :        Elab_Control_Pragma := None;
      No_Warnings  :        Boolean             := False)
   is
      Dep_Node : Dependency;
      LU_Name : constant String := Unit.Library_Unit_Name.all;
   begin
      if False
        or else Dep = "Standard"
        or else Dep = LU_Name
        or else Unit.Comment_Out_Mode
      then
         --  No need to with oneself. If Dep is an ancestor of Unit, register
         --  it (even though no 'with' clause will be emitted) for the sake of
         --  elaboration control. If in comment out mode, ignore dependency as
         --  well.
         return;
      end if;

      pragma Debug (O ("Adding depend of " & LU_Name
                       & " (" & Unit_Kind'Image (Unit.Kind) & ")"
                       & " upon " & Dep));

      if True
        and then Unit.Kind = Unit_Spec
        and then Is_Ancestor (LU_Name, Dep)
      then
         --  All hope abandon he who trieth to make a package spec depend upon
         --  its child.
         pragma Debug (O ("The declaration of " & LU_Name
                          & " cannot depend on " & Dep));

         raise Program_Error;
      end if;

      Dep_Node := Find_Dep (Dep, Unit.Context_Clause);

      if Dep_Node = null then
         Unit.Context_Clause := new Dependency_Node'
           (Library_Unit => new String'(Dep),
            Use_It       => Use_It,
            Elab_Control => Elab_Control,
            No_Warnings  => No_Warnings,
            Next         => Unit.Context_Clause);

      else
         Dep_Node.Use_It      := Dep_Node.Use_It or else Use_It;
         Dep_Node.No_Warnings := Dep_Node.No_Warnings and then No_Warnings;

         if Elab_Control = Elaborate_All
           or else Dep_Node.Elab_Control = Elaborate_All
         then
            Dep_Node.Elab_Control := Elaborate_All;

         elsif Elab_Control = Elaborate
           or else Dep_Node.Elab_Control = Elaborate
         then
            Dep_Node.Elab_Control := Elaborate;

         else
            Dep_Node.Elab_Control := None;
         end if;
      end if;

   end Add_With;

   ------------------------
   -- Add_Elaborate_Body --
   ------------------------

   procedure Add_Elaborate_Body
     (U_Spec : in out Compilation_Unit;
      U_Body : Compilation_Unit) is
   begin
      pragma Assert (U_Spec.Kind = Unit_Spec);
      if not Is_Empty (U_Body) then
         U_Spec.Diversions (Visible_Declarations).Empty := False;
         U_Spec.Elaborate_Body := True;
      end if;
   end Add_Elaborate_Body;

   ------------------------------
   -- Suppress_Warning_Message --
   ------------------------------

   procedure Suppress_Warning_Message (Unit : in out Compilation_Unit) is
   begin
      Unit.No_Warning := True;
   end Suppress_Warning_Message;

   ----------
   -- Name --
   ----------

   function Name (CU : Compilation_Unit) return String is
   begin
      return CU.Library_Unit_Name.all;
   end Name;

   -----------------------------
   -- Allocate_User_Diversion --
   -----------------------------

   function Allocate_User_Diversion
     return Diversion is
   begin
      for I in User_Diversions'Range loop
         if not Diversions_Allocation (I) then
            Diversions_Allocation (I) := True;
            return I;
         end if;
      end loop;

      --  Too many diversions open

      raise Program_Error;
   end Allocate_User_Diversion;

   -----------------------
   -- Current_Diversion --
   -----------------------

   function Current_Diversion (CU : Compilation_Unit) return Diversion is
   begin
      return CU.Current_Diversion;
   end Current_Diversion;

   ------------
   -- Divert --
   ------------

   procedure Divert
     (CU     : in out Compilation_Unit;
      Whence : Diversion) is
   begin
      if not
        (Diversions_Allocation (Whence)
         and then (False
           or else Whence in User_Diversions'Range
           or else Whence = Visible_Declarations
           or else (Whence = Private_Declarations and then CU.Kind = Unit_Spec)
           or else (Whence = Elaboration and then CU.Kind = Unit_Body)
           or else (Whence = Generic_Formals and then CU.Kind = Unit_Spec)))
      then
         raise Program_Error;
      end if;

      CU.Current_Diversion := Whence;
   end Divert;

   --------------
   -- Undivert --
   --------------

   procedure Undivert
     (CU : in out Compilation_Unit;
      D  : Diversion)
   is
      Div : Diversion_Data renames CU.Diversions (D);
      Empty_Diversion : Diversion_Data;
      pragma Warnings (Off, Empty_Diversion);
      --  Use default initialization
   begin
      if not Diversions_Allocation (D) then
         raise Program_Error;
      end if;

      if Length (Div.Library_Item) > 0 then
         if not CU.Diversions (CU.Current_Diversion).At_BOL then
            New_Line (CU);
         end if;

         --  Now we are actually at BOL

         Put (CU, To_String (Div.Library_Item));
         CU.Diversions (CU.Current_Diversion).At_BOL := Div.At_BOL;
      end if;

      if not Div.Empty then
         --  Undivert might be performed in template mode, so we need to
         --  carry manually the non-Empty status from D to Current_Diversion.

         CU.Diversions (CU.Current_Diversion).Empty := False;
      end if;

      --  Reset diversion D to empty state

      CU.Diversions (D) := Empty_Diversion;
   end Undivert;

   --------------------------
   -- New_Compilation_Unit --
   --------------------------

   procedure New_Compilation_Unit
     (CU                 : out Compilation_Unit;
      Kind               : Unit_Kind;
      Name               : String;
      Corresponding_Spec : Compilation_Unit_Access := null)
   is
      Res : Compilation_Unit (Kind);
      pragma Warnings (Off, Res);
      --  Used to provide defaults for all components, and an appropriate
      --  discriminant.
   begin
      CU := Res;
      CU.Library_Unit_Name := new String'(Name);
      for D in Predefined_Diversions loop
         CU.Diversions (D).Indent_Level := 1;
      end loop;

      if Kind = Unit_Spec then
         pragma Assert (Corresponding_Spec = null);
         null;
      else
         pragma Assert (Corresponding_Spec /= null);
         CU.Corresponding_Spec := Corresponding_Spec;
      end if;
   end New_Compilation_Unit;

   --------------
   -- Generate --
   --------------

   procedure Generate
     (Unit : Compilation_Unit;
      Is_Generic_Instanciation : Boolean := False;
      To_Stdout : Boolean := False)
   is

      function Ada_File_Name
        (Full_Name : String;
         Part      : Unit_Kind := Unit_Spec)
        return String;
      --  Name of the source file for Unit

      -------------------
      -- Ada_File_Name --
      -------------------

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

      use Output;

      procedure Emit_Standard_Header (User_Edited : Boolean := False);
      --  Generate boilerplate header. If User_Edited is False, include a
      --  warning that the file is generated automatically and should not
      --  be modified by hand.

      procedure Emit_Source_Code;
      --  Generate the source text

      --------------------------
      -- Emit_Standard_Header --
      --------------------------

      procedure Emit_Standard_Header (User_Edited : Boolean := False) is
      begin
         Write_Line ("-------------------------------------------------");
         Write_Line ("--  This file has been generated automatically");
         Write_Line ("--  by IDLAC version " & Platform.Version & ".");

         if not User_Edited then
            Write_Line ("--");
            Write_Line ("--  Do NOT hand-modify this file, as your");
            Write_Line ("--  changes will be lost when you re-run the");
            Write_Line ("--  IDL to Ada compiler.");
         end if;

         Write_Line ("-------------------------------------------------");

         --  Disable style checks (N), and set maximum line length to the
         --  largest allowed value (M32766).

         Write_Line ("pragma Style_Checks (""NM32766"");");
         Write_Eol;
      end Emit_Standard_Header;

      ----------------------
      -- Emit_Source_Code --
      ----------------------

      procedure Emit_Source_Code is
         Dep_Node      : Dependency := Unit.Context_Clause;
         Spec_Dep_Node : Dependency;
      begin
         while Dep_Node /= null loop
            if Unit.Kind = Unit_Body then
               Spec_Dep_Node := Find_Dep
                                  (Dep_Node.Library_Unit.all,
                                   Unit.Corresponding_Spec.Context_Clause);
            end if;

            if Dep_Node.Elab_Control /= None
              or else (Spec_Dep_Node = null
                and then not Is_Ancestor (Dep_Node.Library_Unit.all,
                                          Unit.Library_Unit_Name.all))
            then
               Write_Line ("with " & Dep_Node.Library_Unit.all & ";");
            end if;

            if Dep_Node.Use_It
              and then (Spec_Dep_Node = null or else not Spec_Dep_Node.Use_It)
            then
               Write_Line (" use " & Dep_Node.Library_Unit.all & ";");
            end if;

            case Dep_Node.Elab_Control is
               when Elaborate_All =>
                  Write_Line ("pragma Elaborate_All ("
                            & Dep_Node.Library_Unit.all & ");");
               when Elaborate =>
                  Write_Line ("pragma Elaborate ("
                            & Dep_Node.Library_Unit.all & ");");
               when None =>
                  null;
            end case;

            if Dep_Node.No_Warnings then
               Write_Line ("pragma Warnings (Off, "
                         & Dep_Node.Library_Unit.all & ");");
            end if;
            Dep_Node := Dep_Node.Next;
         end loop;

         if Unit.Context_Clause /= null then
            Write_Eol;
         end if;

         if not Unit.Diversions (Generic_Formals).Empty then
            Write_Line ("generic");
            Write_Str (To_String
                 (Unit.Diversions (Generic_Formals).Library_Item));
            Write_Eol;
         end if;

         Write_Str ("package ");
         if Unit.Kind = Unit_Body then
            Write_Str ("body ");
         end if;
         Write_Line (Unit.Library_Unit_Name.all & " is");

         if Unit.Kind = Unit_Spec and then Unit.Elaborate_Body then
            Write_Eol;
            Write_Line ("   pragma Elaborate_Body;");
         end if;

         if not Unit.Diversions (Visible_Declarations).Empty then
            Write_Str (To_String
                 (Unit.Diversions (Visible_Declarations).Library_Item));
         end if;

         if not Unit.Diversions (Private_Declarations).Empty then
            Write_Eol;
            Write_Line ("private");
            Write_Str (To_String
                 (Unit.Diversions (Private_Declarations).Library_Item));
         end if;

         if not Unit.Diversions (Elaboration).Empty then
            Write_Eol;
            Write_Line ("begin");
            Write_Str (To_String (Unit.Diversions (Elaboration).Library_Item));
         end if;

         if not Is_Generic_Instanciation then
            Write_Eol;
            Write_Line ("end " & Unit.Library_Unit_Name.all & ";");
         end if;
      end Emit_Source_Code;

      use GNAT.OS_Lib;

      File_Name : Unbounded_String;
      --  Name of output file

      File : File_Descriptor;
      --  Output file descriptor

      Success : Boolean;
      --  Status returned upon closing File

   --  Start of processing for Generate

   begin
      if Is_Empty (Unit) then
         return;
      end if;

      if To_Stdout then
         File := Standout;
      else
         File_Name := Output_Directory
                        & Ada_File_Name
                            (Unit.Library_Unit_Name.all, Unit.Kind);
         File := Create_File (To_String (File_Name), Fmode => Binary);
         if File = Invalid_FD then
            raise Program_Error with
              "cannot create output file " & To_String (File_Name);
         end if;
      end if;

      Set_Output (File);
      Emit_Standard_Header (Unit.No_Warning);
      Emit_Source_Code;

      if not To_Stdout then
         Close (File, Status => Success);
         if not Success then
            raise Program_Error with
              "failed to close " & To_String (File_Name);
         end if;
      end if;
   end Generate;

   ---------
   -- Put --
   ---------

   procedure Put
     (Unit : in out Compilation_Unit;
      Text : String)
   is
      Indent_String : constant String
        (1 .. Indent_Size
         * Unit.Diversions (Unit.Current_Diversion).Indent_Level)
        := (others => ' ');
      LF_Pos : Integer;
      Non_Space_Seen : Boolean := False;
      At_BOL : Boolean renames Unit.Diversions (Unit.Current_Diversion).At_BOL;
   begin
      if not Unit.Template_Mode then
         Unit.Diversions (Unit.Current_Diversion).Empty := False;
      end if;

      --  If in comment-out mode, output comment marker at beginning of line

      if Unit.Diversions (Unit.Current_Diversion).At_BOL then
         if Unit.Comment_Out_Mode then
            Append
              (Unit.Diversions (Unit.Current_Diversion).Library_Item, "--  ");
         end if;
      end if;

      --  Determine whether the provided text contains a linefeed, and if so,
      --  wheter there is any non-space character before the linefeed.

      LF_Pos := Text'First;
      while LF_Pos <= Text'Last and then Text (LF_Pos) /= ASCII.LF loop
         if Text (LF_Pos) /= ' ' then
            Non_Space_Seen := True;
         end if;
         LF_Pos := LF_Pos + 1;
      end loop;

      --  Do not output indentation if we know we are generating an empty line

      if At_BOL and then (Non_Space_Seen or else LF_Pos > Text'Last) then
         Append (Unit.Diversions (Unit.Current_Diversion).Library_Item,
                 Indent_String);
      end if;

      Append (Unit.Diversions (Unit.Current_Diversion).Library_Item,
              Text (Text'First .. LF_Pos - 1));
      At_BOL := False;

      --  LF seen?

      if LF_Pos <= Text'Last then
         New_Line (Unit);
      end if;

      --  More?

      if LF_Pos + 1 <= Text'Last then
         Put (Unit, Text (LF_Pos + 1 .. Text'Last));
      end if;
   end Put;

   --------------
   -- Put_Line --
   --------------

   procedure Put_Line
     (Unit : in out Compilation_Unit;
      Line : String) is
   begin
      Put (Unit, Line & ASCII.LF);
   end Put_Line;

   --------------
   -- New_Line --
   --------------

   procedure New_Line (Unit : in out Compilation_Unit) is
   begin
      Append (Unit.Diversions (Unit.Current_Diversion).Library_Item, LF);
      Unit.Diversions (Unit.Current_Diversion).At_BOL := True;
   end New_Line;

   ----------------
   -- Inc_Indent --
   ----------------

   procedure Inc_Indent (Unit : in out Compilation_Unit) is
   begin
      Unit.Diversions (Unit.Current_Diversion).Indent_Level
      := Unit.Diversions (Unit.Current_Diversion).Indent_Level + 1;
   end Inc_Indent;

   ----------------
   -- Dec_Indent --
   ----------------

   procedure Dec_Indent (Unit : in out Compilation_Unit) is
   begin
      Unit.Diversions (Unit.Current_Diversion).Indent_Level
      := Unit.Diversions (Unit.Current_Diversion).Indent_Level - 1;
   end Dec_Indent;

   -----------------------------
   -- Current_Diversion_Empty --
   -----------------------------

   function Current_Diversion_Empty (CU : Compilation_Unit) return Boolean is
   begin
      return CU.Diversions (CU.Current_Diversion).Empty;
   end Current_Diversion_Empty;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (Unit : Compilation_Unit) return Boolean is
   begin
      for I in Unit.Diversions'Range loop
         if not Unit.Diversions (I).Empty then
            return False;
         end if;
      end loop;
      return True;
   end Is_Empty;

   --------------------------
   -- Set_Output_Directory --
   --------------------------

   function Set_Output_Directory (Dir : String) return Boolean is
      Sep : Character renames GNAT.Directory_Operations.Dir_Separator;
   begin
      if not GNAT.OS_Lib.Is_Directory (Dir) then
         return False;
      end if;

      Output_Directory := To_Unbounded_String (Dir);
      if Dir (Dir'Last) /= Sep then
         Append (Output_Directory, Sep);
      end if;
      return True;
   end Set_Output_Directory;

   --------------------------
   -- Set_Comment_Out_Mode --
   --------------------------

   procedure Set_Comment_Out_Mode
     (Unit : in out Compilation_Unit;
      Mode : Boolean)
   is
   begin
      Unit.Comment_Out_Mode := Mode;
   end Set_Comment_Out_Mode;

   -----------------------
   -- Set_Template_Mode --
   -----------------------

   procedure Set_Template_Mode
     (Unit : in out Compilation_Unit;
      Mode : Boolean)
   is
   begin
      Unit.Template_Mode := Mode;
   end Set_Template_Mode;

   --------------
   -- Find_Dep --
   --------------

   function Find_Dep
     (Unit            : String;
      Context_Clauses : Dependency) return Dependency
   is
      D : Dependency := Context_Clauses;
   begin
      while D /= null and then D.Library_Unit.all /= Unit loop
         D := D.Next;
      end loop;
      return D;
   end Find_Dep;

end Ada_Be.Source_Streams;
