------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--                              X E _ B A C K                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision$                             --
--                                                                          --
--         Copyright (C) 1996,1997 Free Software Foundation, Inc.           --
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
--                 GLADE  is maintained by ACT Europe.                      --
--                 (email: glade-report@act-europe.fr)                      --
--                                                                          --
------------------------------------------------------------------------------

with Namet;            use Namet;
with Osint;            use Osint;
with Output;           use Output;
with XE;               use XE;
with XE_Utils;         use XE_Utils;

package body XE_Back is

   procedure Build_New_Channel
     (Channel   : in Variable_Id);
   --  Retrieve the two partitions and attributes previously parsed in
   --  order to build the channel.

   procedure Build_New_Host
     (Subprogram : in Subprogram_Id;
      Host_Entry : out HID_Type);

   procedure Build_New_Partition
     (Partition : in Variable_Id);
   --  Retrieve ada units and attributes previously parsed in order to
   --  build the partition.

   procedure Build_New_Variable
     (Variable : in Variable_Id);

   procedure Set_Channel_Attribute
     (Attribute : in Attribute_Id;
      Channel   : in CID_Type);

   procedure Set_Partition_Attribute
     (Attribute : in Attribute_Id;
      Partition : in PID_Type);

   procedure Set_Pragma_Statement
     (Subprogram : in Subprogram_Id);

   procedure Set_Type_Attribute
     (Pre_Type : in Type_Id);

   ---------------------------
   -- Add_Channel_Partition --
   ---------------------------

   procedure Add_Channel_Partition
     (Partition : in Partition_Name_Type; To : in CID_Type) is
      PID  : PID_Type := Get_PID (Partition);

      procedure Update_Channel_Partition
        (Channel_Partition : in out Channel_Partition_Type;
         Partition         : in PID_Type;
         Channel           : in CID_Type);
      --  Link Channel into the list of partition channels. The head of
      --  this list is First_Channel (Partitions) and the tail is
      --  Last_Channel. Next elements are Next_Channel (Channels).

      procedure Update_Channel_Partition
        (Channel_Partition : in out Channel_Partition_Type;
         Partition         : in PID_Type;
         Channel           : in CID_Type) is
         CID : CID_Type;
      begin
         Channel_Partition.My_Partition := Partition;
         Channel_Partition.Next_Channel := Null_CID;

         CID := Partitions.Table (Partition).Last_Channel;
         if CID = Null_CID then
            Partitions.Table (Partition).First_Channel := Channel;
            Partitions.Table (Partition).Last_Channel  := Channel;
         else
            if Channels.Table (CID).Lower.My_Partition = Partition then
               Channels.Table (CID).Lower.Next_Channel := Channel;
            else
               Channels.Table (CID).Upper.Next_Channel := Channel;
            end if;
            Partitions.Table (Partition).Last_Channel := Channel;
         end if;

      end Update_Channel_Partition;

   begin
      if Verbose_Mode then
         Write_Program_Name;
         Write_Str  (": add partition ");
         Write_Name (Partition);
         Write_Str  (" to channel ");
         Write_Name (Channels.Table (To).Name);
         Write_Eol;
      end if;
      if Channels.Table (To).Lower = Null_Channel_Partition then
         Update_Channel_Partition (Channels.Table (To).Lower, PID, To);
      elsif PID > Channels.Table (To).Lower.My_Partition then
         Update_Channel_Partition (Channels.Table (To).Upper, PID, To);
      else
         Channels.Table (To).Upper := Channels.Table (To).Lower;
         Update_Channel_Partition (Channels.Table (To).Lower, PID, To);
      end if;
   end Add_Channel_Partition;

   -------------------
   -- Add_Conf_Unit --
   -------------------

   procedure Add_Conf_Unit
     (CU : in CUnit_Name_Type;
      To : in PID_Type) is
   begin

      if Get_PID (CU) = To then
         return;
      end if;

      if Verbose_Mode then
         Write_Program_Name;
         Write_Str  (": configuring unit ");
         Write_Name (CU);
         Write_Str  (" on partition ");
         Write_Name (Partitions.Table (To).Name);
         Write_Eol;
      end if;

      --  Mark this configured unit as already partitioned.
      Set_PID (CU, To);

      --  The same unit can be multiply declared especially if
      --  this unit is a normal package.
      CUnit.Increment_Last;
      CUnit.Table (CUnit.Last).Partition := To;
      CUnit.Table (CUnit.Last).CUname    := CU;
      CUnit.Table (CUnit.Last).My_ALI    := No_ALI_Id;
      CUnit.Table (CUnit.Last).My_Unit   := No_Unit_Id;
      CUnit.Table (CUnit.Last).Next      := Null_CUID;

      --  Update partition single linked list of configured units.
      if Partitions.Table (To).First_Unit = Null_CUID then
         Partitions.Table (To).First_Unit := CUnit.Last;
      else
         CUnit.Table (Partitions.Table (To).Last_Unit).Next := CUnit.Last;
      end if;
      Partitions.Table (To).Last_Unit := CUnit.Last;

   end Add_Conf_Unit;

   --------------------
   -- Already_Loaded --
   --------------------

   function Already_Loaded (Unit : Name_Id) return Boolean is
   begin
      Get_Name_String (Unit);
      Name_Len := Name_Len + 1;
      Name_Buffer (Name_Len) := '%';
      Name_Len := Name_Len + 1;
      Name_Buffer (Name_Len) := 'b';
      if Get_Name_Table_Info (Name_Find) /= 0 then
         return True;
      end if;
      Name_Buffer (Name_Len) := 's';
      if Get_Name_Table_Info (Name_Find) /= 0 then
         return True;
      end if;
      return False;
   end Already_Loaded;

   ----------
   -- Back --
   ----------

   procedure Back is
      Node : Node_Id;
   begin

      First_Configuration_Declaration (Configuration_Node, Node);
      while Node /= Null_Node loop
         if Is_Variable (Node) then
            Build_New_Variable (Variable_Id (Node));

         elsif Is_Configuration (Node) then
            Configuration := Get_Node_Name (Node);

         elsif Is_Type (Node) then
            Set_Type_Attribute (Type_Id (Node));

         elsif Is_Statement (Node) then
            Set_Pragma_Statement
              (Get_Subprogram_Call (Statement_Id (Node)));

         end if;
         Next_Configuration_Declaration (Node);
      end loop;

      if Main_Partition = Null_PID then
         Write_SLOC (Node_Id (Configuration_Node));
         Write_Str  ("non-dist. app. main subprogram has not been declared");
         Write_Eol;
         raise Parsing_Error;
      end if;
   end Back;

   -----------------------
   -- Build_New_Channel --
   -----------------------

   procedure Build_New_Channel
     (Channel   : in Variable_Id) is
      Channel_Name   : Name_Id := Get_Node_Name (Node_Id (Channel));
      Channel_Type   : Type_Id := Get_Variable_Type (Channel);
      Partition_Name : Name_Id;
      Partition_Node : Node_Id;
      Component_Node : Component_Id;
      Channel_ID     : CID_Type;
   begin

      --  Create a new entry in Channels.Table.

      Create_Channel (Channel_Name, Node_Id (Channel), Channel_ID);

      --  Scan Channel_Name partition pair and Channel_Name attributes.

      First_Variable_Component (Variable_Id (Channel), Component_Node);
      while Component_Node /= Null_Component loop

         --  This is a partition (upper or lower).

         if Get_Attribute_Kind (Component_Node) = Attribute_Unknown then
            if Is_Component_Initialized (Component_Node) then

               --  Append this partition to the pair.
               Partition_Node := Get_Component_Value (Component_Node);
               Partition_Name := Get_Node_Name (Partition_Node);
               Add_Channel_Partition (Partition_Name, Channel_ID);

            end if;
         else
            Set_Channel_Attribute
              (Attribute_Id (Component_Node), Channel_ID);
         end if;

         Next_Variable_Component (Component_Node);

      end loop;

   end Build_New_Channel;

   --------------------
   -- Build_New_Host --
   --------------------

   procedure Build_New_Host
     (Subprogram : in Subprogram_Id;
      Host_Entry : out HID_Type) is
      Host : HID_Type;
      Name : Name_Id;
      Node : Node_Id;
   begin

      Node := Node_Id (Subprogram);
      Name := Get_Node_Name (Node);
      Host := Get_HID (Name);

      if Host = Null_HID then
         Name := Get_Node_Name (Node);
         Create_Host (Name, Node, Host);
         Hosts.Table (Host).Static   := False;
         Hosts.Table (Host).Import   := Ada_Import;
         Hosts.Table (Host).External := Name;
      end if;

      Host_Entry := Host;

   end Build_New_Host;

   -------------------------
   -- Build_New_Partition --
   -------------------------

   procedure Build_New_Partition
     (Partition : in Variable_Id) is
      Partition_Name : Name_Id := Get_Node_Name (Node_Id (Partition));
      Partition_Type : Type_Id := Get_Variable_Type (Partition);
      Ada_Unit_Name  : Name_Id;
      Ada_Unit_Node  : Node_Id;
      Component_Node : Component_Id;
      Partition_ID   : PID_Type;
   begin

      --  Create a new entry into Partitions.Table.

      Create_Partition (Partition_Name, Node_Id (Partition), Partition_ID);

      --  Scan Partition_Name ada unit list and Partition_Name attributes.

      First_Variable_Component (Variable_Id (Partition), Component_Node);
      while Component_Node /= Null_Component loop

         if Get_Attribute_Kind (Component_Node) = Attribute_Unknown then

            --  This is a configured ada unit.

            --  Append this unit to the partition list.
            Ada_Unit_Node := Get_Component_Value (Component_Node);
            Ada_Unit_Name := Get_Node_Name (Ada_Unit_Node);
            Add_Conf_Unit (Ada_Unit_Name, Partition_ID);

         else

            --  This information is a partition attribute.

            Set_Partition_Attribute
              (Attribute_Id (Component_Node), Partition_ID);

         end if;

         Next_Variable_Component (Component_Node);

      end loop;

   end Build_New_Partition;

   ------------------------
   -- Build_New_Variable --
   ------------------------

   procedure Build_New_Variable
     (Variable : in Variable_Id) is
      Var_Type : Type_Id;
      Pre_Type : Predefined_Type;
   begin
      Var_Type := Get_Variable_Type (Variable);
      Pre_Type := Get_Type_Kind (Var_Type);
      case Pre_Type is

         when Pre_Type_Partition =>
            Build_New_Partition (Variable);

         when Pre_Type_Channel   =>
            Build_New_Channel (Variable);

         when others =>
            null;

      end case;
   end Build_New_Variable;

   --------------------
   -- Create_Channel --
   --------------------

   procedure Create_Channel
     (Name : in Channel_Name_Type;
      Node : in Node_Id;
      CID  : out CID_Type) is
      Channel : CID_Type;
   begin
      if Verbose_Mode then
         Write_Program_Name;
         Write_Str  (": create channel ");
         Write_Name (Name);
         Write_Eol;
      end if;

      Channels.Increment_Last;
      Channel := Channels.Last;
      Set_CID (Name, Channel);
      Channels.Table (Channel).Name            := Name;
      Channels.Table (Channel).Node            := Node;
      Channels.Table (Channel).Lower           := Null_Channel_Partition;
      Channels.Table (Channel).Upper           := Null_Channel_Partition;
      Channels.Table (Channel).Filter          := No_Filter_Name;
      CID := Channel;
   end Create_Channel;

   -----------------
   -- Create_Host --
   -----------------

   procedure Create_Host
     (Name : in Host_Name_Type;
      Node : in Node_Id;
      HID  : out HID_Type) is
      Host : HID_Type;
   begin
      if Verbose_Mode then
         Write_Program_Name;
         Write_Str  (": create host ");
         Write_Name (Name);
         Write_Eol;
      end if;

      Hosts.Increment_Last;
      Host := Hosts.Last;
      Hosts.Table (Host).Name            := Name;
      Hosts.Table (Host).Node            := Node;
      Hosts.Table (Host).Static          := True;
      Hosts.Table (Host).Import          := None_Import;
      Hosts.Table (Host).External        := No_Name;
      Set_HID (Name, Host);
      HID := Host;
   end Create_Host;

   ----------------------
   -- Create_Partition --
   ----------------------

   procedure Create_Partition
     (Name : in Partition_Name_Type;
      Node : in Node_Id;
      PID  : out PID_Type) is
      Partition : PID_Type;
   begin
      if Verbose_Mode then
         Write_Program_Name;
         Write_Str  (": create partition ");
         Write_Name (Name);
         Write_Eol;
      end if;

      Partitions.Increment_Last;
      Partition := Partitions.Last;
      Set_PID (Name, Partition);
      Partitions.Table (Partition).Name            := Name;
      Partitions.Table (Partition).Node            := Node;
      Partitions.Table (Partition).Host            := Null_HID;
      Partitions.Table (Partition).Storage_Dir     := No_Storage_Dir;
      Partitions.Table (Partition).Command_Line    := No_Command_Line;
      Partitions.Table (Partition).Main_Subprogram := No_Name;
      Partitions.Table (Partition).Filter          := No_Filter_Name;
      Partitions.Table (Partition).Termination     := Unknown_Termination;
      Partitions.Table (Partition).First_Unit      := Null_CUID;
      Partitions.Table (Partition).Last_Unit       := Null_CUID;
      Partitions.Table (Partition).First_Channel   := Null_CID;
      Partitions.Table (Partition).Last_Channel    := Null_CID;
      Partitions.Table (Partition).To_Build        := True;
      Partitions.Table (Partition).Most_Recent     := Configuration_File;
      Partitions.Table (Partition).Task_Pool       := No_Task_Pool;
      PID := Partition;
   end Create_Partition;

   -----------------------
   -- Get_Absolute_Exec --
   -----------------------

   function Get_Absolute_Exec (P : in PID_Type) return Name_Id is
      Dir  : Name_Id := Partitions.Table (P).Storage_Dir;
      Name : Name_Id renames Partitions.Table (P).Name;
   begin

      if Dir = No_Storage_Dir then
         Dir := Partitions.Table (Default_Partition).Storage_Dir;
      end if;

      if Dir = No_Storage_Dir then

         --  No storage dir means current directory

         return PWD_Id & Name;

      else
         Get_Name_String (Dir);
         if Name_Buffer (1) /= Separator and then
           Name_Buffer (1) /= '/' then

            --  The storage dir is relative

            return PWD_Id & Dir & Dir_Sep_Id & Name;

         end if;

         --  Write the dir as it has been written

         return Dir & Dir_Sep_Id & Name;

      end if;

   end Get_Absolute_Exec;

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

   -------------
   -- Get_CID --
   -------------

   function Get_CID (N : Name_Id) return CID_Type is
      Info : Int;
   begin
      Info := Get_Name_Table_Info (N);
      if Info in Int (CID_Type'First) .. Int (CID_Type'Last) then
         return CID_Type (Info);
      else
         return Null_CID;
      end if;
   end Get_CID;

   ----------------------
   -- Get_Command_Line --
   ----------------------

   function Get_Command_Line    (P : in PID_Type) return Command_Line_Type is
      Cmd : Command_Line_Type := Partitions.Table (P).Command_Line;
   begin

      if Cmd = No_Command_Line then
         Cmd := Partitions.Table (Default_Partition).Command_Line;
      end if;

      return Cmd;

   end Get_Command_Line;

   --------------
   -- Get_CUID --
   --------------

   function Get_CUID (N : Name_Id) return CUID_Type is
      Info : Int;
   begin
      Info := Get_Name_Table_Info (N);
      if Info in Int (CUID_Type'First) .. Int (CUID_Type'Last) then
         return CUID_Type (Info);
      else
         return Null_CUID;
      end if;
   end Get_CUID;

   ----------------
   -- Get_Filter --
   ----------------

   function Get_Filter          (C : CID_Type) return Name_Id is
      F : Name_Id := Channels.Table (C).Filter;
   begin

      if F = No_Filter_Name then
         F := Channels.Table (Default_Channel).Filter;
      end if;
      return F;

   end Get_Filter;

   ----------------
   -- Get_Filter --
   ----------------

   function Get_Filter          (P : PID_Type) return Name_Id is
      F : Name_Id := Partitions.Table (P).Filter;
   begin

      if F = No_Filter_Name then
         F := Partitions.Table (Default_Partition).Filter;
      end if;
      return F;

   end Get_Filter;

   -------------
   -- Get_HID --
   -------------

   function Get_HID (N : Name_Id) return HID_Type is
      Info : Int;
   begin
      Info := Get_Name_Table_Info (N);
      if Info in Int (HID_Type'First) .. Int (HID_Type'Last) then
         return HID_Type (Info);
      else
         return Null_HID;
      end if;
   end Get_HID;

   --------------
   -- Get_Host --
   --------------

   function Get_Host            (P : in PID_Type) return Name_Id is
      H : HID_Type := Partitions.Table (P).Host;
   begin

      if H = Null_HID then
         H := Default_Host;
      end if;

      if H /= Null_HID then
         if not Hosts.Table (H).Static then
            if Hosts.Table (H).Import = Shell_Import then
               return  Str_To_Id ("""`") &
                       Hosts.Table (H).External &
                       Str_To_Id (" ") &
                       Partitions.Table (P).Name &
                       Str_To_Id ("`""");
            elsif Hosts.Table (H).Import = Ada_Import then
               return  Hosts.Table (H).External &
                       Str_To_Id ("(") &
                       Partitions.Table (P).Name &
                       Str_To_Id (")");
            end if;
            raise Parsing_Error;
         else
            return Str_To_Id ("""") &
                   Hosts.Table (H).Name &
                   Str_To_Id ("""");
         end if;

      else
         return No_Name;

      end if;

   end Get_Host;

   -------------------------
   -- Get_Main_Subprogram --
   -------------------------

   function Get_Main_Subprogram
     (P : in PID_Type)
      return Main_Subprogram_Type is
      Main : Main_Subprogram_Type := Partitions.Table (P).Main_Subprogram;
   begin

      if Main = No_Main_Subprogram then
         Main := Partitions.Table (Default_Partition).Main_Subprogram;
      end if;

      return Main;

   end Get_Main_Subprogram;

   -----------------------
   -- Get_Partition_Dir --
   -----------------------

   function Get_Partition_Dir (P : in PID_Type) return File_Name_Type is
   begin
      return DSA_Dir &
        Dir_Sep_Id & Configuration &
        Dir_Sep_Id & Partitions.Table (P).Name;
   end Get_Partition_Dir;

   -------------
   -- Get_PID --
   -------------

   function Get_PID (N : Name_Id) return PID_Type is
      Info : Int;
   begin
      Info := Get_Name_Table_Info (N);
      if Info in Int (PID_Type'First) .. Int (PID_Type'Last) then
         return PID_Type (Info);
      else
         return Null_PID;
      end if;
   end Get_PID;

   -----------------------
   -- Get_Relative_Exec --
   -----------------------

   function Get_Relative_Exec (P : in PID_Type) return Name_Id is
      Dir  : Name_Id := Partitions.Table (P).Storage_Dir;
      Name : Name_Id renames Partitions.Table (P).Name;
   begin

      if Dir = No_Storage_Dir then
         Dir := Partitions.Table (Default_Partition).Storage_Dir;
      end if;

      if Dir = No_Storage_Dir then

         --  No storage dir means current directory

         return Name;

      else

         --  The storage dir is relative

         return Dir & Dir_Sep_Id & Name;

      end if;

   end Get_Relative_Exec;

   ---------------------
   -- Get_Storage_Dir --
   ---------------------

   function Get_Storage_Dir (P : in PID_Type) return Storage_Dir_Name_Type is
      Storage_Dir : Storage_Dir_Name_Type := Partitions.Table (P).Storage_Dir;
   begin

      if Storage_Dir = No_Storage_Dir then
         Storage_Dir := Partitions.Table (Default_Partition).Storage_Dir;
      end if;

      return Storage_Dir;

   end Get_Storage_Dir;

   -------------------
   -- Get_Task_Pool --
   -------------------

   function Get_Task_Pool (P : PID_Type) return Task_Pool_Type is
      Task_Pool : Task_Pool_Type := Partitions.Table (P).Task_Pool;
   begin

      if Task_Pool = No_Task_Pool then
         Task_Pool := Partitions.Table (Default_Partition).Task_Pool;
      end if;

      return Task_Pool;

   end Get_Task_Pool;

   ---------------------
   -- Get_Termination --
   ---------------------

   function Get_Termination
     (P : in PID_Type)
      return Termination_Type is
   begin
      return Partitions.Table (P).Termination;
   end Get_Termination;

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

   --------------------
   -- Get_Unit_Sfile --
   --------------------

   function Get_Unit_Sfile (U : in Unit_Id) return File_Name_Type is
   begin
      Get_Name_String (Unit.Table (U).Sfile);
      Name_Len := Name_Len - 4;
      return Name_Find;
   end Get_Unit_Sfile;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
      P : PID_Type;
      C : CID_Type;
   begin
      Partitions.Increment_Last;
      P := Partitions.Last;
      Partitions.Table (P).Name
        := Get_Node_Name (Node_Id (Partition_Type_Node));

      Partitions.Table (P).Main_Subprogram  := No_Main_Subprogram;
      Partitions.Table (P).Host             := Null_HID;
      Partitions.Table (P).Storage_Dir      := No_Storage_Dir;
      Partitions.Table (P).Command_Line     := No_Command_Line;
      Partitions.Table (P).Termination      := Unknown_Termination;
      Partitions.Table (P).Filter           := No_Filter_Name;

      Name_Len := 1;

      Name_Buffer (1 .. 1) := "3";
      No_Task_Pool (1)  := Name_Find;

      Name_Buffer (1 .. 1) := "6";
      No_Task_Pool (2)  := Name_Find;

      Name_Buffer (1 .. 1) := "9";
      No_Task_Pool (3)  := Name_Find;

      Partitions.Table (P).Task_Pool := No_Task_Pool;

      Default_Partition := P;

      Channels.Increment_Last;
      C := Channels.Last;
      Channels.Table (C).Name
        := Get_Node_Name (Node_Id (Channel_Type_Node));

      Channels.Table (C).Filter             := No_Filter_Name;
      Default_Channel := C;

   end Initialize;

   ------------
   -- Is_Set --
   ------------

   function Is_Set (Partition : PID_Type) return Boolean is
   begin
      return Partitions.Table (Partition).Last_Unit /= Null_CUID;
   end Is_Set;

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
      if Already_Loaded (From) then
         return;
      end if;
      File := From & ADB_Suffix;
      if Full_Source_Name (File) = No_Name then
         File := From & ADS_Suffix;
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
         Write_Missing_File (Lib);
         raise Fatal_Error;
      end if;
      Read_ALI (Scan_ALI (Lib, Text));
   end Load_All_Units;

   -----------------------
   -- More_Recent_Stamp --
   -----------------------

   procedure More_Recent_Stamp (P : in PID_Type; F : in File_Name_Type) is
   begin
      if F > Partitions.Table (P).Most_Recent then
         if Debug_Mode then
            Write_Program_Name;
            Write_Str   (": ");
            Write_Name  (F);
            Write_File_Stamp (F);
            Write_Str   (", ");
            Write_Name  (Partitions.Table (P).Name);
            Write_Str   ("'s most recent file (previously ");
            Write_File_Stamp (Partitions.Table (P).Most_Recent);
            Write_Str   (")");
            Write_Eol;
         end if;
         Partitions.Table (P).Most_Recent := F;
      end if;
   end More_Recent_Stamp;

   ----------------
   -- Set_ALI_Id --
   ----------------

   procedure Set_ALI_Id (N : Name_Id; A : ALI_Id) is
   begin
      Set_Name_Table_Info (N, Int (A));
   end Set_ALI_Id;

   ---------------------------
   -- Set_Channel_Attribute --
   ---------------------------

   procedure Set_Channel_Attribute
     (Attribute : in Attribute_Id;
      Channel   : in CID_Type) is

      --  Could be a variable or a subprogram.
      Attr_Item : Node_Id;
      Attr_Kind : Attribute_Type;

   begin

      --  Apply attribute to a channel.

      Attr_Kind := Get_Attribute_Kind (Component_Id (Attribute));
      Attr_Item := Get_Component_Value (Component_Id (Attribute));

      --  No attribute was really assigned.

      if Attr_Item = Null_Node then
         return;
      end if;

      case Attr_Kind is
         when Attribute_CFilter =>

            --  Only string literals are allowed here.

            if not Is_Variable (Attr_Item) or else
              Get_Variable_Type (Variable_Id (Attr_Item)) /=
              String_Type_Node then
               Write_SLOC (Node_Id (Attribute));
               Write_Name (Channels.Table (Channel).Name);
               Write_Str ("'s filter attribute must be ");
               Write_Str ("a string literal");
               Write_Eol;
               raise Parsing_Error;
            end if;

            --  Does it apply to all channels ? Therefore, check
            --  that this has not already been done.

            if Channel = Null_CID and then
               Channels.Table (Default_Channel).Filter = No_Filter_Name then
               Channels.Table (Default_Channel).Filter
                 := Get_Node_Name (Attr_Item);

            --  Apply to one channel. Check that it has not already
            --  been done.

            elsif Channel /= Null_CID and then
              Channels.Table (Channel).Filter = No_Filter_Name then
               Channels.Table (Channel).Filter
                 := Get_Node_Name (Attr_Item);

            --  This operation has already been done !

            else
               Write_SLOC (Attr_Item);
               if Channel = Null_CID then
                  Write_Str ("predefined type Channel");
               else
                  Write_Name (Channels.Table (Channel).Name);
               end if;
               Write_Str ("'s filter attribute has been assigned twice");
               Write_Eol;
               raise Parsing_Error;
            end if;

         when others =>
            raise Fatal_Error;

      end case;

   end Set_Channel_Attribute;

   -------------
   -- Set_CID --
   -------------

   procedure Set_CID (N : Name_Id; C : CID_Type) is
   begin
      Set_Name_Table_Info (N, Int (C));
   end Set_CID;

   -------------
   -- Set_CUID --
   -------------

   procedure Set_CUID (N : Name_Id; U : CUID_Type) is
   begin
      Set_Name_Table_Info (N, Int (U));
   end Set_CUID;

   --------------
   -- Set_Host --
   --------------

   procedure Set_HID (N : in Name_Id; H : in HID_Type) is
   begin
      Set_Name_Table_Info (N, Int (H));
   end Set_HID;

   -----------------------------
   -- Set_Partition_Attribute --
   -----------------------------

   procedure Set_Partition_Attribute
     (Attribute : in Attribute_Id;
      Partition : in PID_Type) is

      --  Could be a variable or a subprogram.
      Attr_Item : Node_Id;
      Attr_Kind : Attribute_Type;
      Item_Type : Type_Id;
      Comp_Node : Component_Id;
      PID       : PID_Type;
      Ada_Unit  : Name_Id;

      Host      : HID_Type;

      procedure Write_Error_Message
        (Node : in Node_Id;
         Part : in PID_Type;
         Attr : in String);

      procedure Write_Error_Message
        (Node : in Node_Id;
         Part : in PID_Type;
         Attr : in String) is
      begin
         Write_SLOC (Node);
         Write_Name (Partitions.Table (Partition).Name);
         Write_Str ("'s ");
         Write_Str (Attr);
         Write_Str (" attribute has been assigned twice");
         Write_Eol;
         raise Parsing_Error;
      end Write_Error_Message;

   begin

      --  If this attribute applies to partition type itself, it may not
      --  have a value. No big deal, we use defaults.

--       if Partition = Null_PID and then
--         not Is_Component_Initialized (Component_Id (Attribute)) then
--          return;
--       end if;

      --  Apply attribute to a partition.

      Attr_Kind := Get_Attribute_Kind (Component_Id (Attribute));
      Attr_Item := Get_Component_Value (Component_Id (Attribute));

      --  No attribute was really assigned.

      if Attr_Item = Null_Node then
         return;
      end if;

      PID := Partition;

--       if Partition = Null_PID then
--          PID := Default_Partition;
--       else
--          PID := Partition;
--       end if;

      case Attr_Kind is
         when Attribute_PFilter =>

            --  Only string literals are allowed here.

            if not Is_Variable (Attr_Item) or else
              Get_Variable_Type (Variable_Id (Attr_Item)) /=
              String_Type_Node then
               Write_SLOC (Node_Id (Attribute));
               Write_Name (Partitions.Table (PID).Name);
               Write_Str ("'s filter attribute must be ");
               Write_Str ("a string literal");
               Write_Eol;
               raise Parsing_Error;
            end if;

            --  Does it apply to all partitions ? Therefore, check
            --  that this has not already been done.

            if PID = Default_Partition and then
               Partitions.Table (PID).Filter = No_Filter_Name
            then
               Partitions.Table (PID).Filter := Get_Node_Name (Attr_Item);

            --  Apply to one partition. Check that it has not already
            --  been done.

            elsif PID /= Default_Partition then
               Write_SLOC (Node_Id (Attribute));
               Write_Str ("a partition filter attribute applies only to ");
               Write_Eol;
               Write_SLOC (Node_Id (Attribute));
               Write_Str ("predefined type Partition");
               Write_Eol;
               raise Parsing_Error;

            --  This operation has already been done !

            else
               Write_Error_Message (Node_Id (Attribute), PID, "filter");
            end if;

         when Attribute_Storage_Dir =>

            --  Only strings are allowed here.

            if not Is_Variable (Attr_Item) or else
              Get_Variable_Type (Variable_Id (Attr_Item)) /=
              String_Type_Node then
               Write_SLOC (Node_Id (Attribute));
               Write_Name (Partitions.Table (PID).Name);
               Write_Str ("'s storage_dir attribute must be ");
               Write_Str ("a string litteral");
               Write_Eol;
               raise Parsing_Error;
            end if;

            --  Check that it has not already been assigned.

            if Partitions.Table (PID).Storage_Dir = No_Storage_Dir then
               Partitions.Table (PID).Storage_Dir := Get_Node_Name (Attr_Item);
            else
               Write_Error_Message (Node_Id (Attribute), PID, "storage_dir");
            end if;

         when Attribute_Host =>

            Item_Type := Get_Variable_Type (Variable_Id (Attr_Item));
            case Get_Type_Kind (Item_Type) is
               when Pre_Type_String =>
                  Hosts.Increment_Last;
                  Host := Hosts.Last;
                  Hosts.Table (Host).Name := Get_Node_Name (Attr_Item);
                  Hosts.Table (Host).Static := True;

               when Pre_Type_Ada_Unit =>
                  Attr_Item :=
                    Node_Id (Get_Variable_Value (Variable_Id (Attr_Item)));
                  Build_New_Host (Subprogram_Id (Attr_Item), Host);

               when others =>
                  Write_SLOC (Node_Id (Attribute));
                  Write_Name (Partitions.Table (PID).Name);
                  Write_Str  ("'s host attribute must of string type");
                  Write_Eol;
                  raise Parsing_Error;
            end case;

            --  Check that it has not already been assigned.

            if Partitions.Table (PID).Host = Null_HID then
               Partitions.Table (PID).Host := Host;
            else
               Write_Error_Message (Node_Id (Attribute), PID, "host");
            end if;

         when Attribute_Main =>

            --  Check that it has not already been assigned.

            if Partitions.Table (PID).Main_Subprogram = No_Main_Subprogram then
               Partitions.Table (PID).Main_Subprogram
                 := Get_Node_Name (Node_Id (Attr_Item));

               --  We are not sure at this point that this unit
               --  has been configured on partition.

               Ada_Unit := Get_Node_Name (Node_Id (Attr_Item));
               Add_Conf_Unit (Ada_Unit, PID);

            else
               Write_Error_Message (Node_Id (Attribute), PID, "main");
            end if;

         when Attribute_Command_Line =>

            --  Only strings are allowed.

            if not Is_Variable (Attr_Item) or else
              Get_Variable_Type (Variable_Id (Attr_Item)) /=
              String_Type_Node then
               Write_SLOC (Node_Id (Attribute));
               Write_Name (Partitions.Table (PID).Name);
               Write_Str ("'s command line attribute must be string litteral");
               Write_Eol;
               raise Parsing_Error;
            end if;

            --  Check that this has not already been assigned.

            if Partitions.Table (PID).Command_Line = No_Command_Line then
               Partitions.Table (PID).Command_Line
                 := Get_Node_Name (Node_Id (Attr_Item));
            else
               Write_Error_Message (Node_Id (Attribute), PID, "command_Line");
            end if;

         when Attribute_Termination =>

            --  Only booleans are allowed.

            if not Is_Variable (Attr_Item) or else
              Get_Variable_Type (Variable_Id (Attr_Item)) /=
              Integer_Type_Node then
               Write_SLOC (Node_Id (Attribute));
               Write_Name (Partitions.Table (PID).Name);
               Write_Str ("'s termination attribute must be ");
               Write_Str ("of termination type");
               Write_Eol;
               raise Parsing_Error;
            end if;

            --  Check that it has not already been assigned.

            if Partitions.Table (PID).Termination = Unknown_Termination then
               Partitions.Table (PID).Termination :=
                 Termination_Type (Get_Scalar_Value (Variable_Id (Attr_Item)));
            else
               Write_Error_Message (Node_Id (Attribute), PID, "termination");
            end if;

         when Attribute_Leader =>

            --  Internal attribute. Don't check anything.

            if Partition /= Null_PID then
               Main_Partition := Partition;
            end if;

         when Attribute_Task_Pool =>

            First_Variable_Component (Variable_Id (Attr_Item), Comp_Node);
            for B in Partitions.Table (PID).Task_Pool'Range loop
               Partitions.Table (PID).Task_Pool (B)
                 := Get_Node_Name (Get_Component_Value (Comp_Node));
               Next_Variable_Component (Comp_Node);
            end loop;

         when Attribute_CFilter | Attribute_Unknown =>
            raise Fatal_Error;

      end case;

   end Set_Partition_Attribute;

   -------------
   -- Set_PID --
   -------------

   procedure Set_PID (N : Name_Id; P : PID_Type) is
   begin
      Set_Name_Table_Info (N, Int (P));
   end Set_PID;

   --------------------------
   -- Set_Pragma_Statement --
   --------------------------

   procedure Set_Pragma_Statement
     (Subprogram  : in Subprogram_Id) is

      Pragma_Kind : Pragma_Type;
      Parameter   : Parameter_Id;
      Method      : Import_Method_Type;
      Value       : Variable_Id;
      Host        : HID_Type;

   begin

      --  Apply pragma statement.

      Pragma_Kind := Get_Pragma_Kind (Subprogram);
      First_Subprogram_Parameter (Subprogram, Parameter);

      case Pragma_Kind is

         when Pragma_Import =>
            Value := Get_Variable_Value (Variable_Id (Parameter));
            Method := Convert (Get_Scalar_Value (Value));
            Next_Subprogram_Parameter (Parameter);
            Value := Get_Variable_Value (Variable_Id (Parameter));
            Value := Get_Variable_Value (Variable_Id (Value));

            --  We are not sure that this function has been already
            --  declared as an host function.

            Build_New_Host (Subprogram_Id (Value), Host);

            --  Apply Import pragma ...

            Hosts.Table (Host).Import := Method;
            Next_Subprogram_Parameter (Parameter);
            Value := Get_Variable_Value (Variable_Id (Parameter));
            Hosts.Table (Host).External := Get_Node_Name (Node_Id (Value));

         when Pragma_Starter =>
            Value := Get_Variable_Value (Variable_Id (Parameter));
            Default_Starter := Convert (Get_Scalar_Value (Value));

         when Pragma_Boot_Server =>
            Value := Get_Variable_Value (Variable_Id (Parameter));
            Default_Protocol_Name := Get_Node_Name (Node_Id (Value));
            Next_Subprogram_Parameter (Parameter);
            Value := Get_Variable_Value (Variable_Id (Parameter));
            Default_Protocol_Data := Get_Node_Name (Node_Id (Value));

         when Pragma_Version =>
            Value := Get_Variable_Value (Variable_Id (Parameter));
            Default_Version_Check := (Get_Scalar_Value (Value) /= 0);

         when Pragma_Reg_Filter =>
            Value := Get_Variable_Value (Variable_Id (Parameter));
            Default_Registration_Filter := Get_Node_Name (Node_Id (Value));

         when Pragma_Unknown =>
            null;

      end case;

   end Set_Pragma_Statement;

   ------------------------
   -- Set_Type_Attribute --
   ------------------------

   procedure Set_Type_Attribute
     (Pre_Type : in Type_Id) is
      Component_Node : Component_Id;
      Pre_Type_Id    : Predefined_Type;
   begin

      if Get_Component_List_Size (Pre_Type) /= 0 then
         Pre_Type_Id := Get_Type_Kind (Pre_Type);
         First_Type_Component (Pre_Type, Component_Node);
         while Component_Node /= Null_Component loop
            if Get_Attribute_Kind (Component_Node) /= Attribute_Unknown
              and then Is_Component_Initialized (Component_Node) then
               case Pre_Type_Id is
                  when Pre_Type_Partition =>
                     Set_Partition_Attribute
                       (Attribute_Id (Component_Node), Default_Partition);

                  when Pre_Type_Channel   =>
                     Set_Channel_Attribute
                       (Attribute_Id (Component_Node), Default_Channel);

                  when others =>
                     null;
               end case;
            end if;
            Next_Type_Component (Component_Node);
         end loop;
      end if;

   end Set_Type_Attribute;

   -----------------
   -- Set_Unit_Id --
   -----------------

   procedure Set_Unit_Id (N : Name_Id; U : Unit_Id) is
   begin
      Set_Name_Table_Info (N, Int (U));
   end Set_Unit_Id;

   -------------------------
   --  Show_Configuration --
   -------------------------

   procedure Show_Configuration is

      Main         : Main_Subprogram_Type;
      Host         : HID_Type;
      Storage_Dir  : Storage_Dir_Name_Type;
      Command_Line : Command_Line_Type;
      Task_Pool    : Task_Pool_Type;

   begin
      Write_Str (" ------------------------------");
      Write_Eol;
      Write_Str (" ---- Configuration report ----");
      Write_Eol;
      Write_Str (" ------------------------------");
      Write_Eol;
      Write_Str ("Configuration :");
      Write_Eol;
      Write_Str ("   Name        : ");
      Write_Name (Configuration);
      Write_Eol;

      Write_Str ("   Main        : ");
      Write_Name (Main_Subprogram);
      Write_Eol;

      Write_Str ("   Starter     : ");
      case Default_Starter is
         when Ada_Import =>
            Write_Str ("Ada code");
         when Shell_Import =>
            Write_Str ("shell script");
         when None_Import =>
            Write_Str ("none");
      end case;
      Write_Eol;

      if Default_Protocol_Name /= No_Name then
         Write_Str  ("   Protocol    : ");
         Write_Name (Default_Protocol_Name);
         Write_Str  ("://");
         Write_Name (Default_Protocol_Data);
         Write_Eol;
      end if;
      Write_Eol;

      for P in Partitions.First + 1 .. Partitions.Last loop
         declare
            I : Partition_Type renames Partitions.Table (P);
            U : CUID_Type;
         begin
            Write_Str ("Partition ");
            Write_Name (I.Name);
            Write_Eol;

            Main := Get_Main_Subprogram (P);
            if Main /= No_Main_Subprogram then
               Write_Str ("   Main        : ");
               Write_Name (Main);
               Write_Eol;
            end if;

            Host := I.Host;
            if Host = Null_HID then
               Host := Default_Host;
            end if;

            if Host /= Null_HID then
               Write_Str ("   Host        : ");
               if Hosts.Table (Host).Static then
                  Write_Name (Hosts.Table (Host).Name);
               else
                  Write_Str ("function call :: ");
                  Write_Name (Hosts.Table (Host).External);
                  case Hosts.Table (Host).Import is
                     when None_Import =>
                        null;
                     when Ada_Import =>
                        Write_Str (" (ada)");
                     when Shell_Import =>
                        Write_Str (" (shell)");
                  end case;
               end if;
               Write_Eol;
            end if;

            Storage_Dir := Get_Storage_Dir (P);
            if Storage_Dir /= No_Storage_Dir then
               Write_Str ("   Storage     : ");
               Write_Name (Storage_Dir);
               Write_Eol;
            end if;

            Command_Line := Get_Command_Line (P);
            if Command_Line /= No_Command_Line then
               Write_Str ("   Command     : ");
               Write_Name (Command_Line);
               Write_Eol;
            end if;

            Task_Pool := Get_Task_Pool (P);
            if Task_Pool /= No_Task_Pool then
               Write_Str ("   Task Pool   : ");
               for B in Task_Pool'Range loop
                  Write_Name (Task_Pool (B));
                  Write_Str (" ");
               end loop;
               Write_Eol;
            end if;

            if Get_Termination (P) /= Unknown_Termination then
               Write_Str ("   Termination : ");
               case Get_Termination (P) is
                  when Local_Termination =>
                     Write_Str ("local");
                  when Global_Termination =>
                     Write_Str ("global");
                  when Deferred_Termination =>
                     Write_Str ("deferred");
                  when Unknown_Termination =>
                     null;
               end case;
               Write_Eol;
            end if;

            if I.First_Unit /= Null_CUID then
               Write_Str ("   Units       : ");
               Write_Eol;
               U := I.First_Unit;
               while U /= Null_CUID loop
                  Write_Str ("             - ");
                  Write_Name (CUnit.Table (U).CUname);
                  if Unit.Table (CUnit.Table (U).My_Unit).RCI then
                     Write_Str (" (rci) ");
                  else
                     Write_Str (" (normal)");
                  end if;
                  Write_Eol;
                  U := CUnit.Table (U).Next;
               end loop;
               Write_Eol;
            end if;
         end;
      end loop;
      Write_Str (" -------------------------------");
      Write_Eol;
      if Channels.First + 1 <= Channels.Last then
         Write_Eol;
         declare
            P : PID_Type;
            F : Name_Id;
         begin
            for C in Channels.First + 1 .. Channels.Last loop
               Write_Str  ("Channel ");
               Write_Name (Channels.Table (C).Name);
               Write_Eol;
               if Debug_Mode then
                  Write_Str  ("   Number      : ");
                  Write_Int (Int (C));
                  Write_Eol;
               end if;
               Write_Str     ("   Partition 1 : ");
               P := Channels.Table (C).Lower.My_Partition;
               Write_Name (Partitions.Table (P).Name);
               Write_Eol;
               if Debug_Mode then
                  Write_Str ("   Channel     : ");
                  Write_Int (Int (Channels.Table (C).Lower.Next_Channel));
                  Write_Eol;
               end if;
               Write_Str     ("   Partition 2 : ");
               P := Channels.Table (C).Upper.My_Partition;
               Write_Name (Partitions.Table (P).Name);
               Write_Eol;
               if Debug_Mode then
                  Write_Str ("   Channel     : ");
                  Write_Int (Int (Channels.Table (C).Upper.Next_Channel));
                  Write_Eol;
               end if;
               F := Get_Filter (C);
               if F /= No_Filter_Name then
                  Write_Str  ("   Filter      : ");
                  Write_Name (F);
                  Write_Eol;
               end if;
               Write_Eol;
            end loop;
         end;
         Write_Str (" -------------------------------");
         Write_Eol;
      end if;

   end Show_Configuration;

end XE_Back;
