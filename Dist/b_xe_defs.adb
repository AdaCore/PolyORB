------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--                            B _ X E _ D E F S                             --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision$
--                                                                          --
--              Copyright (C) 2003 Free Software Foundation, Inc.           --
--                                                                          --
-- GARLIC is free software;  you can redistribute it and/or modify it under --
-- terms of the  GNU General Public License  as published by the Free Soft- --
-- ware Foundation;  either version 2,  or (at your option)  any later ver- --
-- sion.  GARLIC is distributed  in the hope that  it will be  useful,  but --
-- WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHANTABI- --
-- LITY or  FITNESS FOR A PARTICULAR PURPOSE.  See the  GNU General Public  --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License  distributed with GARLIC;  see file COPYING.  If  --
-- not, write to the Free Software Foundation, 59 Temple Place - Suite 330, --
-- Boston, MA 02111-1307, USA.                                              --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
--               GLADE  is maintained by ACT Europe.                        --
--               (email: glade-report@act-europe.fr)                        --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO;      use Ada.Text_IO;
with GNAT.OS_Lib;      use GNAT.OS_Lib;
with Table;

procedure B_XE_Defs is

   package Compiler_Switches is new Table.Table (
     Table_Component_Type => String_Access,
     Table_Index_Type     => Integer,
     Table_Low_Bound      => 1,
     Table_Initial        => 20,
     Table_Increment      => 100,
     Table_Name           => "XE.Compiler_Switches");

   package Binder_Switches is new Table.Table (
     Table_Component_Type => String_Access,
     Table_Index_Type     => Integer,
     Table_Low_Bound      => 1,
     Table_Initial        => 20,
     Table_Increment      => 100,
     Table_Name           => "XE.Binder_Switches");

   package Linker_Switches is new Table.Table (
     Table_Component_Type => String_Access,
     Table_Index_Type     => Integer,
     Table_Low_Bound      => 1,
     Table_Initial        => 20,
     Table_Increment      => 100,
     Table_Name           => "XE.Linker_Switches");

   type Mode_Type is (None, Compiler, Binder, Linker);
   Mode : Mode_Type := None;

   --  Store the flags once parsed
   RSH_CMD           : String_Access;
   RSH_OPT           : String_Access;
   PCS_NAME          : String_Access;
   DEF_STORAGE_DATA  : String_Access;
   DEF_STORAGE_NAME  : String_Access;
   DEF_PROTOCOL_DATA : String_Access;
   DEF_PROTOCOL_NAME : String_Access;
   Windows_NT        : Boolean := False;

   procedure Generate_Function (F : String; P : String_Access);
   --  Generate a parameterless function of name F which returns a
   --  constant string P.all. When P is null, return null string.

   function  Match     (S : String; L : String) return Boolean;
   --  Check whether S starts with L + "="

   procedure Put       (N : Natural; S : String);
   procedure Put_Line  (N : Natural; S : String);
   --  Put or Put_Line with N indentations

   function  Remove    (S : String; L : String) return String;
   --  Return S - (L + "="). Assume that Match (S, L) is true.

   -----------------------
   -- Generate_Function --
   -----------------------

   procedure Generate_Function (F : String; P : String_Access) is
   begin
      Put_Line (1, "function " & F & " return String is");
      Put_Line (1, "begin");
      Put      (2, "return """);
      if P /= null then
         Put (P.all);
      end if;
      Put_Line (""";");
      Put_Line (1, "end " & F & ";");
   end Generate_Function;

   -----------
   -- Match --
   -----------

   function Match (S : String; L : String) return Boolean
   is
      LL : constant Natural := L'Length + 1;
      SL : constant Natural := S'Length;
   begin
      if LL <= SL
        and then S (S'First .. S'First + LL - 2) = L
        and then S (S'First + LL - 1) = '='
      then
         return True;
      end if;
      return False;
   end Match;

   ---------
   -- Put --
   ---------

   procedure Put (N : Natural; S : String) is
   begin
      for I in 1 .. N loop
         Put ("   ");
      end loop;
      Put (S);
   end Put;

   --------------
   -- Put_Line --
   --------------

   procedure Put_Line (N : Natural; S : String) is
   begin
      Put (N, S);
      New_Line;
   end Put_Line;

   ------------
   -- Remove --
   ------------

   function Remove (S : String; L : String) return String
   is
      LL : constant Natural := L'Length + 1;
   begin
      return S (S'First + LL .. S'Last);
   end Remove;

begin
   for I in 1 .. Argument_Count loop
      declare
         A : constant String := Argument (I);
      begin
         if A = "-cargs" then
            Mode := Compiler;

         elsif A = "-bargs" then
            Mode := Binder;

         elsif A = "-largs" then
            Mode := Linker;

         elsif Match (A, "RSH_CMD") then
            RSH_CMD := new String'(Remove (A, "RSH_CMD"));

         elsif Match (A, "RSH_OPT") then
            RSH_OPT := new String'(Remove (A, "RSH_OPT"));

         elsif Match (A, "DEFSTORAGEDATA") then
            DEF_STORAGE_DATA := new String'(Remove (A, "DEFSTORAGEDATA"));

         elsif Match (A, "DEFSTORAGENAME") then
            DEF_STORAGE_NAME := new String'(Remove (A, "DEFSTORAGEDATA"));

         elsif Match (A, "DEFPROTOCOLDATA") then
            DEF_PROTOCOL_DATA := new String'(Remove (A, "DEFPROTOCOLDATA"));

         elsif Match (A, "DEFPROTOCOLNAME") then
            DEF_PROTOCOL_NAME := new String'(Remove (A, "DEFPROTOCOLDATA"));

         elsif Match (A, "PCS_NAME") then
            PCS_NAME := new String'(Remove (A, "PCS_NAME"));

         elsif Match (A, "PSNAME") then
            Windows_NT := (Remove (A, "DEFPROTOCOLDATA") = "Windows/NT");

         elsif Mode = None then
            Compiler_Switches.Append (new String'(A));
            if A'Length > 2
              and then A (A'First .. A'First + 1) = "-I"
            then
               Binder_Switches.Append (new String'(A));
            end if;

         elsif Mode = Compiler then
            Compiler_Switches.Append (new String'(A));

         elsif Mode = Binder then
            Binder_Switches.Append (new String'(A));

         elsif Mode = Linker then
            Linker_Switches.Append (new String'(A));
         end if;
      end;
   end loop;

   Put_Line (0, "pragma Style_Checks (Off);");
   if Windows_NT then
      Put_Line (0, "with XE_Reg;");
      Put_Line (0, "with GNAT.Strings;");
      Put_Line (0, "use type GNAT.Strings.String_Access;");
   end if;
   Put_Line (0, "with XE; use XE;");
   Put_Line (0, "package body XE_Defs is");
   Put_Line (1, "procedure Initialize is");
   Put_Line (1, "begin");

   if Windows_NT then
      Put_Line (2, "if XE_Reg.Get_Garlic_Dir /= null then");
      Put_Line (3, "Compiler_Switches.Append");
      Put_Line (4, "(new String'(""-I"" & XE_Reg.Get_Garlic_Dir.all));");
      Put_Line (2, "end if;");
   end if;

   for I in Compiler_Switches.First .. Compiler_Switches.Last loop
      Put_Line (2, "Compiler_Switches.Append");
      Put      (3, "(new String'(""");
      Put      (Compiler_Switches.Table (I).all);
      Put_Line ("""));");
   end loop;

   if Windows_NT then
      Put_Line (2, "if XE_Reg.Get_Garlic_Dir /= null then");
      Put_Line (3, "Binder_Switches.Append");
      Put_Line (4, "(new String'(""-I"" & XE_Reg.Get_Garlic_Dir.all));");
      Put_Line (2, "end if;");
   end if;

   for I in Binder_Switches.First .. Binder_Switches.Last loop
      Put_Line (2, "Binder_Switches.Append");
      Put      (3, "(new String'(""");
      Put      (Binder_Switches.Table (I).all);
      Put_Line ("""));");
   end loop;

   for I in Linker_Switches.First .. Linker_Switches.Last loop
      Put_Line (2, "Linker_Switches.Append");
      Put      (3, "(new String'(""");
      Put      (Linker_Switches.Table (I).all);
      Put_Line ("""));");
   end loop;

   Put_Line (1, "end Initialize;");
   Generate_Function ("Get_Rsh_Command", RSH_CMD);
   Generate_Function ("Get_Rsh_Options", RSH_OPT);
   Generate_Function ("Get_PCS_Name", PCS_NAME);
   Generate_Function ("Get_Def_Storage_Data", DEF_STORAGE_DATA);
   Generate_Function ("Get_Def_Storage_Name", DEF_STORAGE_NAME);
   Generate_Function ("Get_Def_Protocol_Data", DEF_PROTOCOL_DATA);
   Generate_Function ("Get_Def_Protocol_Name", DEF_PROTOCOL_NAME);
   Put_Line (0, "end XE_Defs;");
end B_XE_Defs;
