------------------------------------------------------------------------------
--                                                                          --
--                          GNATDIST COMPONENTS                             --
--                                                                          --
--                                   X E                                    --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            1.14                              --
--                                                                          --
--           Copyright (C) 1996 Free Software Foundation, Inc.              --
--                                                                          --
-- GNATDIST is  free software;  you  can redistribute  it and/or  modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 2,  or  (at your option) any later --
-- version. GNATDIST is distributed in the hope that it will be useful, but --
-- WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHANTABI- --
-- LITY or FITNESS  FOR A PARTICULAR PURPOSE.  See the  GNU General  Public --
-- License  for more details.  You should  have received a copy of the  GNU --
-- General Public License distributed with  GNATDIST; see file COPYING.  If --
-- not, write to the Free Software Foundation, 59 Temple Place - Suite 330, --
-- Boston, MA 02111-1307, USA.                                              --
--                                                                          --
--              GNATDIST is maintained by ACT Europe.                       --
--            (email:distribution@act-europe.gnat.com).                     --
--                                                                          --
------------------------------------------------------------------------------
with Fname;        use Fname;
with Output;       use Output;
with Osint;        use Osint;
with Namet;        use Namet;
with GNAT.Os_Lib;  use GNAT.Os_Lib;
with XE_Utils;     use XE_Utils;
with XE_Defs;      use XE_Defs;

package body XE is

   First_Stamp : Boolean := True;

   -----------------------------
   -- Maybe_Most_Recent_Stamp --
   -----------------------------

   procedure Maybe_Most_Recent_Stamp (Stamp : Time_Stamp_Type) is
   begin
      if First_Stamp or else Stamp > Most_Recent_Stamp then
         First_Stamp := False;
         Most_Recent_Stamp := Stamp;
      end if;
   end Maybe_Most_Recent_Stamp;

   --------------------
   -- Load_All_Units --
   --------------------

   procedure Load_All_Units (From : Unit_Name_Type) is
      File : File_Name_Type;
      Lib  : File_Name_Type;
      Text : Text_Buffer_Ptr;
   begin
      if Verbose_Mode then
         Write_Program_Name;
         Write_Str  (": loading all units from ");
         Write_Name (From);
         Write_Eol;
      end if;
      File := File_Name_Of_Body (From);
      if Full_Source_Name (From) = No_Name then
         File := File_Name_Of_Spec (From);
         if Full_Source_Name (File) = No_Name then
            Write_Program_Name;
            Write_Str (": no spec or body found for unit ");
            Write_Name (From);
            Write_Eol;
            raise Fatal_Error;
         end if;
      end if;
      Lib  := Lib_File_Name (File);
      Text := Read_Library_Info (Lib);
      if Text = null then
         Write_Program_Name;
         Write_Str  (": ");
         Write_Name (Lib);
         Write_Str  (" not found");
         Write_Eol;
         raise Fatal_Error;
      end if;
      Read_ALI (Scan_ALI (Lib, Text));
   end Load_All_Units;

   ----------------------
   -- Create_Partition --
   ----------------------

   procedure Create_Partition
     (Name : in Partition_Name_Type) is
      PID  : PID_Type;
   begin
      if Verbose_Mode then
         Write_Program_Name;
         Write_Str  (": create ");
         Write_Name (Name);
         Write_Eol;
      end if;

      Partitions.Increment_Last;
      PID := Partitions.Last;
      Set_PID (Name, PID);
      Partitions.Table (PID).Name := Name;
   end Create_Partition;

   --------------------
   -- Copy_Partition --
   --------------------

   procedure Copy_Partition
     (Name : in Partition_Name_Type;
      Many : in Int) is
      PID  : PID_Type;
      CPID : PID_Type;
      CUID : CUID_Type;
   begin
      if Verbose_Mode then
         Write_Program_Name;
         Write_Str  (": create ");
         Write_Name (Name);
         Write_Str  (" (");
         Write_Int  (Many);
         if Many > 1 then
            Write_Str (" copies)");
         else
            Write_Str (" copy)");
         end if;
         Write_Eol;
      end if;

      CPID := Get_PID (Name);
      if CPID = Null_PID or else
        CPID = Wrong_PID then
         Write_Program_Name;
         Write_Str (": gnatdist is going crazy");
         Write_Eol;
         raise Fatal_Error;
      end if;
      for I in 1 .. Many loop
         Partitions.Increment_Last;
         PID := Partitions.Last;
         Set_PID (Name, PID);
         Partitions.Table (PID).Name := Name;
         CUID := Partitions.Table (CPID).First_Unit;
         while CUID /= Null_CUID loop
            Add_Conf_Unit (CUnit.Table (CUID).CUname, PID);
            CUID := CUnit.Table (CUID).Next;
         end loop;
      end loop;
   end Copy_Partition;

   -------------------
   -- Add_Conf_Unit --
   -------------------

   procedure Add_Conf_Unit
     (CU : in CUnit_Name_Type;
      To : in PID_Type) is
   begin

      if Verbose_Mode then
         Write_Program_Name;
         Write_Str  (": configuring unit ");
         Write_Name (CU);
         Write_Str  (" on partition ");
         Write_Name (Partitions.Table (To).Name);
         Write_Eol;
      end if;

      --  The configured unit name should not be a partition name.
      if Get_PID (CU) = Wrong_PID then
         Write_Program_Name;
         Write_Str  (": symbol ");
         Write_Name (CU);
         Write_Str  (" is already used");
         Write_Eol;
         raise Parsing_Error;
      end if;

      --  Mark this configured unit as already partitioned.
      Set_PID (CU, To);

      --  The same unit can be multiply declared especially if
      --  this unit is a normal package.
      CUnit.Increment_Last;
      CUnit.Table (CUnit.Last).Partition := To;
      CUnit.Table (CUnit.Last).CUname    := CU;

      --  Update partition single linked list of configured units.
      if Partitions.Table (To).First_Unit = Null_CUID then
         Partitions.Table (To).First_Unit := CUnit.Last;
      else
         CUnit.Table (Partitions.Table (To).Last_Unit).Next := CUnit.Last;
      end if;
      Partitions.Table (To).Last_Unit := CUnit.Last;

   end Add_Conf_Unit;

   ------------
   -- Is_Set --
   ------------

   function Is_Set (Partition : PID_Type) return Boolean is
   begin
      return Partitions.Table (Partition).Last_Unit /= Null_CUID;
   end Is_Set;

   -------------
   -- Get_PID --
   -------------

   function Get_PID (N : Name_Id) return PID_Type is
      Info : Int;
   begin
      Info := Get_Name_Table_Info (N);
      case Info is
         when 0 | PID_Null =>
            return Null_PID;
         when PID_First .. PID_Last =>
            return PID_Type (Info);
         when others =>
            return Wrong_PID;
      end case;
   end Get_PID;

   -------------
   -- Set_PID --
   -------------

   procedure Set_PID (N : Name_Id; P : PID_Type) is
   begin
      Set_Name_Table_Info (N, Int (P));
   end Set_PID;

   -----------------
   -- Get_Unit_Id --
   -----------------

   function Get_Unit_Id (N : Name_Id) return Unit_Id is
      Info : Int;
   begin
      Info := Get_Name_Table_Info (N);
      case Info is
         when Int (Unit_Id'First) .. Int (Unit_Id'Last) =>
            null;
         when others =>
            Info := Int (No_Unit_Id);
      end case;
      return Unit_Id (Info);
   end Get_Unit_Id;

   -----------------
   -- Set_Unit_Id --
   -----------------

   procedure Set_Unit_Id (N : Name_Id; U : Unit_Id) is
   begin
      Set_Name_Table_Info (N, Int (U));
   end Set_Unit_Id;

   ----------------
   -- Get_ALI_Id --
   ----------------

   function Get_ALI_Id (N : Name_Id) return ALI_Id is
      Info : Int;
   begin
      Info := Get_Name_Table_Info (N);
      case Info is
         when Int (ALI_Id'First) .. Int (ALI_Id'Last) =>
            null;
         when others =>
            Info := Int (No_ALI_Id);
      end case;
      return ALI_Id (Info);
   end Get_ALI_Id;

   ----------------
   -- Set_ALI_Id --
   ----------------

   procedure Set_ALI_Id (N : Name_Id; A : ALI_Id) is
   begin
      Set_Name_Table_Info (N, Int (A));
   end Set_ALI_Id;

   -------------
   -- Get_CUID --
   -------------

   function Get_CUID (N : Name_Id) return CUID_Type is
      Info : Int;
   begin
      Info := Get_Name_Table_Info (N);
      case Info is
         when 0 | CUID_Null =>
            return Null_CUID;
         when CUID_First .. CUID_Last =>
            return CUID_Type (Info);
         when others =>
            return Wrong_CUID;
      end case;
   end Get_CUID;

   -------------
   -- Set_CUID --
   -------------

   procedure Set_CUID (N : Name_Id; U : CUID_Type) is
   begin
      Set_Name_Table_Info (N, Int (U));
   end Set_CUID;

   ---------------
   -- Get_Token --
   ---------------

   function Get_Token (N : Name_Id) return Token_Type is
      Info : Int;
   begin
      Info := Get_Name_Table_Info (N);
      case Info is
         when Tkn_First .. Tkn_Last =>
            return Token_Type'Val
              (Info - Int (Wrong_Token) +
               Int (Token_Type'Pos (Token_Type'First)));
         when others =>
            return Tok_Unknown;
      end case;
   end Get_Token;

   ---------------
   -- Set_Token --
   ---------------

   procedure Set_Token (N : String; T : Token_Type) is
      Name  : Name_Id;
      Index : Int;
   begin
      Index := Int (Wrong_Token) +
               Int (Token_Type'Pos (T) -
                    Token_Type'Pos (Token_Type'First));
      Name_Len := N'Length;
      Name_Buffer (1 .. Name_Len) := N;
      Name := Name_Find;
      Set_Name_Table_Info (Name, Index);
      Reserved (T) := True;
   end Set_Token;

   -------------------
   -- Get_Attribute --
   -------------------

   function Get_Attribute (N : Name_Id) return Attribute_Type is
      Info : Int;
   begin
      Info := Get_Name_Table_Info (N);
      case Info is
         when Attr_First .. Attr_Last =>
            return Attribute_Type'Val
              (Info - Int (Wrong_Attribute) +
               Int (Attribute_Type'Pos (Attribute_Type'First)));
         when others =>
            return Att_Unknown;
      end case;
   end Get_Attribute;

   -------------------
   -- Set_Attribute --
   -------------------

   procedure Set_Attribute (N : String; A : Attribute_Type) is
      Name  : Name_Id;
      Index : Int;
   begin
      Index := Int (Wrong_Attribute) +
               Int (Attribute_Type'Pos (A) -
                    Attribute_Type'Pos (Attribute_Type'First));
      Name_Len := N'Length;
      Name_Buffer (1 .. Name_Len) := N;
      Name := Name_Find;
      Set_Name_Table_Info (Name, Index);
   end Set_Attribute;

   ----------------
   -- Get_Pragma --
   ----------------

   function Get_Pragma (N : Name_Id) return Pragma_Type is
      Info : Int;
   begin
      Info := Get_Name_Table_Info (N);
      case Info is
         when Prag_First .. Prag_Last =>
            return Pragma_Type'Val
              (Info - Int (Wrong_Pragma) +
               Int (Pragma_Type'Pos (Pragma_Type'First)));
         when others =>
            return Pgm_Unknown;
      end case;
   end Get_Pragma;

   ----------------
   -- Set_Pragma --
   ----------------

   procedure Set_Pragma (N : String; P : Pragma_Type) is
      Name  : Name_Id;
      Index : Int;
   begin
      Index := Int (Wrong_Pragma) +
               Int (Pragma_Type'Pos (P) -
                    Pragma_Type'Pos (Pragma_Type'First));
      Name_Len := N'Length;
      Name_Buffer (1 .. Name_Len) := N;
      Name := Name_Find;
      Set_Name_Table_Info (Name, Index);
   end Set_Pragma;

   -------------------------
   -- Get_Predefined_Type --
   -------------------------

   function Get_Predefined_Type (N : Name_Id) return Predefined_Type is
      Info : Int;
   begin
      Info := Get_Name_Table_Info (N);
      case Info is
         when Pre_Type_First .. Pre_Type_Last =>
            return Predefined_Type'Val
              (Info - Int (Wrong_Pre_Type) +
               Int (Predefined_Type'Pos (Predefined_Type'First)));
         when others =>
            return Pre_Type_Unknown;
      end case;
   end Get_Predefined_Type;

   -------------------------
   -- Set_Predefined_Type --
   -------------------------

   procedure Set_Predefined_Type (N : String; P : Predefined_Type) is
      Name  : Name_Id;
      Index : Int;
   begin
      Index := Int (Wrong_Pre_Type) +
               Int (Predefined_Type'Pos (P) -
                    Predefined_Type'Pos (Predefined_Type'First));
      Name_Len := N'Length;
      Name_Buffer (1 .. Name_Len) := N;
      Name := Name_Find;
      Set_Name_Table_Info (Name, Index);
   end Set_Predefined_Type;

end XE;






