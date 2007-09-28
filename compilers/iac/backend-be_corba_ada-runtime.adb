------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--         B A C K E N D . B E _ C O R B A _ A D A . R U N T I M E          --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2005-2007, Free Software Foundation, Inc.          --
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

with GNAT.OS_Lib;    use GNAT.OS_Lib;
with GNAT.Case_Util;

with Charset; use Charset;
with Namet;   use Namet;
with Utils;   use Utils;

with Backend.BE_CORBA_Ada.Nodes;  use Backend.BE_CORBA_Ada.Nodes;
with Backend.BE_CORBA_Ada.Nutils; use Backend.BE_CORBA_Ada.Nutils;

package body Backend.BE_CORBA_Ada.Runtime is

   RUD : array (RU_Id) of Node_Id;
   RED : array (RE_Id) of Node_Id;
   --  Arrays of run-time entity and unit designators

   type Casing_Rule is record
      Size : Natural;
      From : String_Access;
      Into : String_Access;
   end record;

   Rules : array (1 .. 64) of Casing_Rule;
   Rules_Last : Natural := 0;

   procedure Apply_Casing_Rules (S : in out String);
   procedure Register_Casing_Rule (S : String);
   procedure Declare_Subunit (N : Node_Id);

   ------------------------
   -- Apply_Casing_Rules --
   ------------------------

   procedure Apply_Casing_Rules (S : in out String) is
      New_Word : Boolean := True;
      Length   : Natural := S'Length;
      S1       : constant String := To_Lower (S);
   begin
      GNAT.Case_Util.To_Mixed (S);
      for I in S'Range loop
         if New_Word then
            New_Word := False;
            for J in 1 .. Rules_Last loop
               if Rules (J).Size <= Length
                 and then S1 (I .. I + Rules (J).Size - 1) = Rules (J).From.all
               then
                  S (I .. I + Rules (J).Size - 1) := Rules (J).Into.all;
               end if;
            end loop;
         end if;
         if S (I) = '_' then
            New_Word := True;
            for J in 1 .. Rules_Last loop
               if Rules (J).Size <= Length
                 and then S1 (I .. I + Rules (J).Size - 1) = Rules (J).From.all
               then
                  S (I .. I + Rules (J).Size - 1) := Rules (J).Into.all;
               end if;
            end loop;
         end if;
         Length := Length - 1;
      end loop;
   end Apply_Casing_Rules;

   ---------------------
   -- Declare_Subunit --
   ---------------------

   procedure Declare_Subunit (N : Node_Id) is
      S : Node_Id;

   begin
      pragma Assert (Kind (N) = K_Defining_Identifier or else
                     Kind (N) = K_Selected_Component);

      S := Get_Declaration_Node (N);
      pragma Assert (Kind (S) = K_Package_Specification);

      Set_Is_Subunit_Package (S, True);
   end Declare_Subunit;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
      Position : Integer;
      Name     : Name_Id;
      Length   : Natural;
      Pkg_Spec : Node_Id;
   begin
      Register_Casing_Rule ("ASCII");
      Register_Casing_Rule ("AbstractBase");
      Register_Casing_Rule ("ARG_INOUT");
      Register_Casing_Rule ("ARG_IN");
      Register_Casing_Rule ("ARG_OUT");
      Register_Casing_Rule ("CORBA");
      Register_Casing_Rule ("NamedValue");
      Register_Casing_Rule ("NVList");
      Register_Casing_Rule ("ORB");
      Register_Casing_Rule ("PolyORB");
      Register_Casing_Rule ("PortableServer");
      Register_Casing_Rule ("ServerRequest");
      Register_Casing_Rule ("_TC");
      Register_Casing_Rule ("TC_");
      Register_Casing_Rule ("TypeCode");
      Register_Casing_Rule ("ExceptionList");
      Register_Casing_Rule ("LocalObject");
      Register_Casing_Rule ("RepositoryId");
      Register_Casing_Rule ("ScopedName");
      Register_Casing_Rule ("TypeCode");
      Register_Casing_Rule ("PolicyType");
      Register_Casing_Rule ("OObject");
      Register_Casing_Rule ("GIOP");
      Register_Casing_Rule ("CDR");
      Register_Casing_Rule ("IDL_Sequences");
      Register_Casing_Rule ("IDL_SEQUENCE_");

      --  Casing Rules for Sequence types

      Register_Casing_Rule ("AnySeq");
      Register_Casing_Rule ("FloatSeq");
      Register_Casing_Rule ("DoubleSeq");
      Register_Casing_Rule ("LongDoubleSeq");
      Register_Casing_Rule ("ShortSeq");
      Register_Casing_Rule ("UShortSeq");
      Register_Casing_Rule ("LongSeq");
      Register_Casing_Rule ("ULongSeq");
      Register_Casing_Rule ("LongLongSeq");
      Register_Casing_Rule ("ULongLongSeq");
      Register_Casing_Rule ("CharSeq");
      Register_Casing_Rule ("WCharSeq");
      Register_Casing_Rule ("StringSeq");
      Register_Casing_Rule ("WStringSeq");
      Register_Casing_Rule ("BooleanSeq");
      Register_Casing_Rule ("OctetSeq");

      --  Misc

      Register_Casing_Rule ("IR_Hooks");
      Register_Casing_Rule ("PolicyList");
      Register_Casing_Rule ("DomainManager");

      for U in RU_Id'Succ (RU_Id'First) .. RU_Id'Last loop
         Set_Str_To_Name_Buffer (RU_Id'Image (U));
         Set_Str_To_Name_Buffer (Name_Buffer (4 .. Name_Len));

         RUD (U) := New_Node (K_Defining_Identifier);

         Position := 0;
         Name     := Name_Find;
         Length   := Name_Len;
         Set_Name_Table_Info (Name, RU_Id'Pos (U));

         while Name_Len > 0 loop
            if Name_Buffer (Name_Len) = '_' then
               Name_Len := Name_Len - 1;
               Position := Integer (Get_Name_Table_Info (Name_Find));
               exit when Position > 0;

            else
               Name_Len := Name_Len - 1;
            end if;
         end loop;

         --  When there is a parent, remove parent unit name from unit
         --  name to get real identifier.

         if Position > 0 then
            Set_Str_To_Name_Buffer
              (Name_Buffer (Name_Len + 2 .. Length));
            Name := Name_Find;
         end if;

         Get_Name_String (Name);
         Apply_Casing_Rules (Name_Buffer (1 .. Name_Len));
         Set_Name (RUD (U), Name_Find);
         Pkg_Spec := New_Node (K_Package_Specification);
         Set_Is_Runtime_Package (Pkg_Spec, True);
         Set_Declaration_Node (RUD (U), Pkg_Spec);

         if Position > 0 then
            RUD (U) := Make_Selected_Component
              (RUD (RU_Id'Val (Position)),
               RUD (U));
         end if;

         Set_Name_Table_Info
           (To_Spec_Name (Fully_Qualified_Name (RUD (U))), Int (RUD (U)));
      end loop;

      Declare_Subunit (RUD (RU_PolyORB_Std_ASCII));
      Declare_Subunit (RUD (RU_CORBA_Internals));
      Declare_Subunit (RUD (RU_CORBA_TypeCode));
      Declare_Subunit (RUD (RU_CORBA_TypeCode_Internals));
      Declare_Subunit (RUD (RU_CORBA_NVList_Internals));
      Declare_Subunit (RUD (RU_PolyORB_Any_TypeCode));
      Declare_Subunit (RUD (RU_PolyORB_Any_TypeCode_Internals));
      Declare_Subunit (RUD (RU_CORBA_Object_Internals));
      Declare_Subunit (RUD (RU_PortableServer_Internals));
      Declare_Subunit (RUD (RU_CORBA_ExceptionList_Internals));
      Declare_Subunit
        (RUD (RU_PolyORB_Representations_CDR_Common_Fixed_Point));
      Declare_Subunit
        (RUD (RU_PolyORB_Buffers_Optimization_Fixed_Point));
      Declare_Subunit
        (RUD (RU_CORBA_IDL_Sequences_IDL_SEQUENCE_Any));
      Declare_Subunit
        (RUD (RU_CORBA_IDL_Sequences_IDL_SEQUENCE_Boolean));
      Declare_Subunit
        (RUD (RU_CORBA_IDL_Sequences_IDL_SEQUENCE_Char));
      Declare_Subunit
        (RUD (RU_CORBA_IDL_Sequences_IDL_SEQUENCE_Wide_Char));
      Declare_Subunit
        (RUD (RU_CORBA_IDL_Sequences_IDL_SEQUENCE_Octet));
      Declare_Subunit
        (RUD (RU_CORBA_IDL_Sequences_IDL_SEQUENCE_Short));
      Declare_Subunit
        (RUD (RU_CORBA_IDL_Sequences_IDL_SEQUENCE_Unsigned_Short));
      Declare_Subunit
        (RUD (RU_CORBA_IDL_Sequences_IDL_SEQUENCE_Long));
      Declare_Subunit
        (RUD (RU_CORBA_IDL_Sequences_IDL_SEQUENCE_Unsigned_Long));
      Declare_Subunit
        (RUD (RU_CORBA_IDL_Sequences_IDL_SEQUENCE_Long_Long));
      Declare_Subunit
        (RUD (RU_CORBA_IDL_Sequences_IDL_SEQUENCE_Unsigned_Long_Long));
      Declare_Subunit
        (RUD (RU_CORBA_IDL_Sequences_IDL_SEQUENCE_Float));
      Declare_Subunit
        (RUD (RU_CORBA_IDL_Sequences_IDL_SEQUENCE_Double));
      Declare_Subunit
        (RUD (RU_CORBA_IDL_Sequences_IDL_SEQUENCE_Long_Double));
      Declare_Subunit
        (RUD (RU_CORBA_IDL_Sequences_IDL_SEQUENCE_String));
      Declare_Subunit
        (RUD (RU_CORBA_IDL_Sequences_IDL_SEQUENCE_Wide_String));

      for E in RE_Id loop
         Set_Str_To_Name_Buffer (RE_Id'Image (E));
         Set_Str_To_Name_Buffer (Name_Buffer (4 .. Name_Len));
         Apply_Casing_Rules (Name_Buffer (1 .. Name_Len));

         while Name_Buffer (Name_Len) in '0' .. '9'
           or else Name_Buffer (Name_Len) = '_'
         loop
            Name_Len := Name_Len - 1;
         end loop;

         if E = RE_Add then
            Set_Str_To_Name_Buffer (Quoted ("+"));
         end if;

         if E = RE_And then
            Set_Str_To_Name_Buffer (Quoted ("&"));
         end if;

         Name := Name_Find;

         RED (E) := Make_Defining_Identifier (Name);

         if Present (RUD (RE_Unit_Table (E))) then
            RED (E) := Make_Selected_Component
              (RUD (RE_Unit_Table (E)), RED (E));
         end if;
      end loop;
   end Initialize;

   --------
   -- RE --
   --------

   function RE
     (Id     : RE_Id;
      Withed : Boolean := True)
     return Node_Id
   is
   begin
      return Copy_Expanded_Name (RED (Id), Withed);
   end RE;

   --------------------------
   -- Register_Casing_Rule --
   --------------------------

   procedure Register_Casing_Rule (S : String) is
   begin
      Rules_Last := Rules_Last + 1;
      Rules (Rules_Last).Size := S'Length;
      Rules (Rules_Last).Into := new String'(S);
      Rules (Rules_Last).From := new String'(S);
      To_Lower (Rules (Rules_Last).From.all);
   end Register_Casing_Rule;

   --------
   -- RU --
   --------

   function RU
     (Id : RU_Id;
      Withed : Boolean := True)
     return Node_Id
   is
      Result : Node_Id;
   begin
      --  This is a runtime unit and not a runtime entity, so it's
      --  parent unit does not have to be "withed"

      Result := Copy_Expanded_Name (RUD (Id), False);
      if Withed then
         Add_With_Package (Result);
      end if;
      return Result;
   end RU;

end Backend.BE_CORBA_Ada.Runtime;
