------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--                              X E _ B A C K                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision$
--                                                                          --
--         Copyright (C) 1996-2000 Free Software Foundation, Inc.           --
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

with ALI;              use ALI;
with ALI.Util;         use ALI.Util;
with GNAT.OS_Lib;      use GNAT.OS_Lib;
with Namet;            use Namet;
with Osint;
with Output;           use Output;
with Types;            use Types;
with XE;               use XE;
with XE_Defs;          use XE_Defs;
with XE_Utils;         use XE_Utils;

package body XE_Back is

   subtype Node_Id is XE.Node_Id;

   Min_Width : constant := 11;

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
   --  Dispatching procedure to create entities of different types.

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

   procedure Show_Partition
     (PID : in PID_Type);
   --  Output the different attributes of a partition.

   procedure Write_Field
     (Indent : in Natural;
      Field  : in String;
      Width  : in Natural := Min_Width);
   --  Output field with at least Width characters and indent it.

   ---------------------------
   -- Add_Channel_Partition --
   ---------------------------

   procedure Add_Channel_Partition
     (Partition : in Partition_Name_Type;
      To        : in CID_Type)
   is
      PID  : PID_Type;

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
         Channel           : in CID_Type)
      is
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
      if Debug_Mode then
         Message ("add partition", Partition,
                  "to channel", Channels.Table (To).Name);
      end if;
      PID := Get_PID (Partition);
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

      if Debug_Mode then
         Message ("configuring unit", CU,
                  "on partition", Partitions.Table (To).Name);
      end if;

      --  Mark this configured unit as already partitioned.
      Set_PID (CU, To);

      --  The same unit can be multiply declared especially if
      --  this unit is a normal package.
      CUnits.Increment_Last;
      CUnits.Table (CUnits.Last).Partition   := To;
      CUnits.Table (CUnits.Last).CUname      := CU;
      CUnits.Table (CUnits.Last).My_ALI      := No_ALI_Id;
      CUnits.Table (CUnits.Last).My_Unit     := No_Unit_Id;
      CUnits.Table (CUnits.Last).Most_Recent := No_File;
      CUnits.Table (CUnits.Last).Next        := Null_CUID;

      --  Update partition single linked list of configured units.
      if Partitions.Table (To).First_Unit = Null_CUID then
         Partitions.Table (To).First_Unit := CUnits.Last;
      else
         CUnits.Table (Partitions.Table (To).Last_Unit).Next := CUnits.Last;
      end if;
      Partitions.Table (To).Last_Unit := CUnits.Last;
   end Add_Conf_Unit;

   ------------------
   -- Add_Location --
   ------------------

   procedure Add_Location
     (First : in out LID_Type;
      Last  : in out LID_Type;
      Major : in Name_Id;
      Minor : in Name_Id)
   is
      LID : LID_Type;

   begin
      --  Add a new element in the location table and fill it with the
      --  protocol name (major) and the protocol data (minor).

      Locations.Increment_Last;
      LID := Locations.Last;
      Locations.Table (LID).Major := Major;
      Locations.Table (LID).Minor := Minor;
      Locations.Table (LID).Next  := Null_LID;

      --  Link this new location to the end of the partition location
      --  list.

      if First = Null_LID then
         First := LID;
      else
         Locations.Table (Last).Next := LID;
      end if;
      Last := LID;
   end Add_Location;

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

      --  If this unit is already loaded then its info is set to its
      --  unit entry.

      if Get_Name_Table_Info (Name_Find) /= 0 then
         return True;
      end if;

      --  If this unit has only a spec, check again.

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
      HID  : HID_Type;

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

      for P in Partitions.First .. Partitions.Last loop
         HID := Partitions.Table (P).Host;
         if HID /= Null_HID
           and then not Hosts.Table (HID).Static
           and then Hosts.Table (HID).Import = Ada_Import then
            Add_Conf_Unit (Hosts.Table (HID).External, P);
         end if;
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
     (Channel   : in Variable_Id)
   is
      Channel_Name   : Name_Id;
      Partition_Name : Name_Id;
      Partition_Node : Variable_Id;
      Component_Node : Component_Id;
      Channel_ID     : CID_Type;

   begin
      Channel_Name := Get_Variable_Name (Channel);

      --  Create a new entry in Channels.Table.

      Create_Channel (Channel_Name, Node_Id (Channel), Channel_ID);

      --  Scan Channel_Name partition pair and Channel_Name attributes.

      First_Variable_Component (Channel, Component_Node);
      while Component_Node /= Null_Component loop

         --  This is a partition (upper or lower).

         if Get_Attribute_Kind (Component_Node) = Attribute_Unknown then
            if Is_Component_Initialized (Component_Node) then

               --  Append this partition to the pair.
               Partition_Node := Get_Component_Value (Component_Node);
               Partition_Name := Get_Variable_Name (Partition_Node);
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
      Host_Entry : out HID_Type)
   is
      Host : HID_Type;
      Name : Name_Id;
      Node : Node_Id;

   begin
      Node := Node_Id (Subprogram);
      Name := Get_Node_Name (Node);
      Host := Get_HID (Name);

      if Host = Null_HID then
         Create_Host (Name, Node, Host);
         Hosts.Table (Host).Static      := False;
         Hosts.Table (Host).Import      := Ada_Import;
         Hosts.Table (Host).External    := Name;
      end if;

      Host_Entry := Host;
   end Build_New_Host;

   -------------------------
   -- Build_New_Partition --
   -------------------------

   procedure Build_New_Partition
     (Partition : in Variable_Id)
   is
      Partition_Name : Name_Id;
      Ada_Unit_Name  : Name_Id;
      Ada_Unit_Node  : Variable_Id;
      Component_Node : Component_Id;
      Partition_ID   : PID_Type;

   begin
      Partition_Name := Get_Variable_Name (Partition);

      --  Create a new entry into Partitions.Table.

      Create_Partition (Partition_Name, Node_Id (Partition), Partition_ID);

      --  Scan Partition_Name ada unit list and Partition_Name attributes.

      First_Variable_Component (Partition, Component_Node);
      while Component_Node /= Null_Component loop

         if Get_Attribute_Kind (Component_Node) = Attribute_Unknown then

            --  This is a configured ada unit.

            --  Append this unit to the partition list.
            Ada_Unit_Node := Get_Component_Value (Component_Node);
            Ada_Unit_Name := Get_Variable_Name (Ada_Unit_Node);
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
     (Variable : in Variable_Id)
   is
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

   ----------------------
   -- Compute_Checksum --
   ----------------------

   procedure Compute_Checksum
     (P : in PID_Type;
      F : in File_Name_Type)
   is
      S : Int;

   begin
      S := Get_Name_Table_Info (F);
      if S = 0 then
         return;
      end if;
      Partitions.Table (P).Global_Checksum :=
        Partitions.Table (P).Global_Checksum
        xor Source.Table (Source_Id (S)).Checksum;
   end Compute_Checksum;

   --------------------
   -- Create_Channel --
   --------------------

   procedure Create_Channel
     (Name : in  Channel_Name_Type;
      Node : in  Node_Id;
      CID  : out CID_Type)
   is
      Channel : CID_Type;

   begin
      if Debug_Mode then
         Message ("create channel", Name);
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
      HID  : out HID_Type)
   is
      Host : HID_Type;

   begin
      if Debug_Mode then
         Message ("create host", Name);
      end if;

      Hosts.Increment_Last;
      Host := Hosts.Last;
      Hosts.Table (Host).Name            := Name;
      Hosts.Table (Host).Node            := Node;
      Hosts.Table (Host).Static          := True;
      Hosts.Table (Host).Import          := None_Import;
      Hosts.Table (Host).External        := No_Name;
      Hosts.Table (Host).Most_Recent     := No_File;
      Set_HID (Name, Host);
      HID := Host;
   end Create_Host;

   ----------------------
   -- Create_Partition --
   ----------------------

   procedure Create_Partition
     (Name : in Partition_Name_Type;
      Node : in Node_Id;
      PID  : out PID_Type)
   is
      Partition : PID_Type;

   begin
      if Debug_Mode then
         Message ("create partition", Name);
      end if;

      Partitions.Increment_Last;
      Partition := Partitions.Last;
      Set_PID (Name, Partition);

      --  Initialize through an aggregate in order to set of the
      --  fields.

      Partitions.Table (Partition) :=
        (Name            => Name,
         Node            => Node,
         Host            => Null_HID,
         Directory       => No_Directory,
         Command_Line    => No_Command_Line,
         Main_Subprogram => No_Name,
         Termination     => Unknown_Termination,
         Reconnection    => Unknown_Reconnection,
         Task_Pool       => No_Task_Pool,
         RCI_Or_RACW     => False,
         Use_Tasking     => False,
         Passive         => Bunknown,
         Filter          => No_Filter_Name,
         Executable_File => No_Name,
         Partition_Dir   => No_Name,
         First_Unit      => Null_CUID,
         Last_Unit       => Null_CUID,
         First_Channel   => Null_CID,
         Last_Channel    => Null_CID,
         F_Net_Location  => Null_LID,
         L_Net_Location  => Null_LID,
         Mem_Location    => Null_LID,
         To_Build        => True,
         Most_Recent     => No_File,
         Global_Checksum => 0);
      PID := Partition;
   end Create_Partition;

   -----------------------
   -- Get_Absolute_Exec --
   -----------------------

   function Get_Absolute_Exec (P : PID_Type) return Name_Id is
      S : Name_Id;
      N : Name_Id renames Partitions.Table (P).Name;

   begin
      S := Partitions.Table (P).Directory;
      if S = No_Directory then
         S := Partitions.Table (Default_Partition).Directory;
      end if;

      if S = No_Directory then

         --  No storage directory means current directory.
         return Dir (PWD_Id, N & Exe_Suffix);

      else
         Get_Name_String (S);
         if Name_Buffer (1) /= Directory_Separator and then
           Name_Buffer (1) /= '/' then

            --  The storage directory is relative.

            return Dir (PWD_Id, S, N & Exe_Suffix);

         end if;

         --  Write the directory as is.

         return Dir (S, N & Exe_Suffix);

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

   function Get_Command_Line (P : PID_Type) return Command_Line_Type is
      Cmd : Command_Line_Type;

   begin
      Cmd := Partitions.Table (P).Command_Line;
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

   ---------------------
   -- Get_Directory --
   ---------------------

   function Get_Directory (P : PID_Type) return Directory_Name_Type is
      S : Directory_Name_Type;

   begin
      S := Partitions.Table (P).Directory;
      if S = No_Directory then
         S := Partitions.Table (Default_Partition).Directory;
      end if;
      return S;
   end Get_Directory;

   ----------------
   -- Get_Filter --
   ----------------

   function Get_Filter (C : CID_Type) return Name_Id is
      F : Name_Id;

   begin
      F := Channels.Table (C).Filter;
      if F = No_Filter_Name then
         F := Channels.Table (Default_Channel).Filter;
      end if;
      return F;
   end Get_Filter;

   ----------------
   -- Get_Filter --
   ----------------

   function Get_Filter (P : PID_Type) return Name_Id is
      F : Name_Id;

   begin
      F := Partitions.Table (P).Filter;
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

   function Get_Host (P : PID_Type) return Name_Id is
      H : HID_Type;

   begin
      H := Partitions.Table (P).Host;
      if H = Null_HID then
         H := Partitions.Table (Default_Partition).Host;
      end if;

      if H /= Null_HID then
         if not Hosts.Table (H).Static then
            if Hosts.Table (H).Import = Shell_Import then
               Name_Len := 0;
               Add_Str_To_Name_Buffer ("""`");
               Get_Name_String_And_Append (Hosts.Table (H).External);
               Add_Char_To_Name_Buffer (' ');
               Get_Name_String_And_Append (Partitions.Table (P).Name);
               Add_Str_To_Name_Buffer ("`""");
               return Name_Find;

            elsif Hosts.Table (H).Import = Ada_Import then
               Get_Name_String (Hosts.Table (H).External);
               Add_Char_To_Name_Buffer ('(');
               Get_Name_String_And_Append (Partitions.Table (P).Name);
               Add_Char_To_Name_Buffer (')');
               return Name_Find;
            end if;
            raise Parsing_Error;

         else
            Name_Len := 0;
            Add_Char_To_Name_Buffer ('"'); -- "
            Get_Name_String_And_Append (Hosts.Table (H).Name);
            Add_Char_To_Name_Buffer ('"'); -- "
            return Name_Find;
         end if;

      else
         return No_Name;
      end if;
   end Get_Host;

   -----------------------
   -- Get_Internal_Dir --
   -----------------------

   function Get_Internal_Dir (P : PID_Type) return File_Name_Type is
   begin
      return Dir (DSA_Dir, Configuration, Partitions.Table (P).Name);
   end Get_Internal_Dir;

   ---------------------
   -- Get_RCI_Or_RACW --
   ---------------------

   function Get_RCI_Or_RACW (P : PID_Type) return Boolean is
   begin
      return Partitions.Table (P).RCI_Or_RACW;
   end Get_RCI_Or_RACW;

   -------------------------
   -- Get_Main_Subprogram --
   -------------------------

   function Get_Main_Subprogram (P : PID_Type) return Main_Subprogram_Type is
      Main : Main_Subprogram_Type;

   begin
      Main := Partitions.Table (P).Main_Subprogram;
      if Main = No_Main_Subprogram then
         Main := Partitions.Table (Default_Partition).Main_Subprogram;
      end if;
      return Main;
   end Get_Main_Subprogram;

   ----------------
   -- Get_Parent --
   ----------------

   function Get_Parent (N : Name_Id) return Name_Id is
   begin
      Get_Name_String (N);
      pragma Assert (Name_Len > 0 and then Name_Buffer (1) /= '.');
      for Index in reverse 1 .. Name_Len loop
         if Name_Buffer (Index) = '.' then
            Name_Len := Index - 1;
            return Name_Find;
         end if;
      end loop;
      return No_Name;
   end Get_Parent;

   -----------------
   -- Get_Passive --
   -----------------

   function Get_Passive (P : PID_Type) return Boolean_Type is
      Passive : Boolean_Type;

   begin
      Passive := Partitions.Table (P).Passive;
      if Passive = Bunknown then
         Passive := Partitions.Table (Default_Partition).Passive;
      end if;
      return Passive;
   end Get_Passive;

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

   ------------------
   -- Get_Protocol --
   ------------------

   function Get_Protocol (P : PID_Type) return LID_Type is
      L : LID_Type;

   begin
      L := Partitions.Table (P).F_Net_Location;
      if L = Null_LID then
         L := Partitions.Table (Default_Partition).F_Net_Location;
      end if;
      return L;
   end Get_Protocol;

   ----------------------
   -- Get_Reconnection --
   ----------------------

   function Get_Reconnection (P : PID_Type) return Reconnection_Type is
      Reconnection : Reconnection_Type;

   begin
      Reconnection := Partitions.Table (P).Reconnection;
      if Reconnection = Unknown_Reconnection then
         Reconnection := Partitions.Table (Default_Partition).Reconnection;
      end if;
      return Reconnection;
   end Get_Reconnection;

   -----------------------
   -- Get_Relative_Exec --
   -----------------------

   function Get_Relative_Exec (P : in PID_Type) return Name_Id is
      D : Name_Id := Partitions.Table (P).Directory;
      N : Name_Id renames Partitions.Table (P).Name;

   begin
      if D = No_Directory then
         D := Partitions.Table (Default_Partition).Directory;
      end if;
      if D = No_Directory then
         return N & Exe_Suffix;
      else
         return Dir (D, N & Exe_Suffix);
      end if;
   end Get_Relative_Exec;

   ---------------------
   -- Get_Rsh_Command --
   ---------------------

   function Get_Rsh_Command return Types.Name_Id is
   begin
      if Default_Rsh_Command = No_Name then
         return Str_To_Id (XE_Defs.Get_Rsh_Command);
      else
         return Default_Rsh_Command;
      end if;
   end Get_Rsh_Command;

   ---------------------
   -- Get_Rsh_Options --
   ---------------------

   function Get_Rsh_Options return Types.Name_Id is
   begin
      if Default_Rsh_Options = No_Name then
         return Str_To_Id (XE_Defs.Get_Rsh_Options);
      else
         return Default_Rsh_Options;
      end if;
   end Get_Rsh_Options;

   -----------------
   -- Get_Storage --
   -----------------

   function Get_Storage (P : PID_Type) return LID_Type is
      Mem_Location : LID_Type;

   begin
      Mem_Location := Partitions.Table (P).Mem_Location;
      if Mem_Location = Null_LID then
         Mem_Location := Partitions.Table (Default_Partition).Mem_Location;
      end if;
      return Mem_Location;
   end Get_Storage;

   -------------------
   -- Get_Task_Pool --
   -------------------

   function Get_Task_Pool (P : PID_Type) return Task_Pool_Type is
      Task_Pool : Task_Pool_Type;

   begin
      Task_Pool := Partitions.Table (P).Task_Pool;
      if Task_Pool = No_Task_Pool then
         Task_Pool := Partitions.Table (Default_Partition).Task_Pool;
      end if;
      return Task_Pool;
   end Get_Task_Pool;

   -----------------
   -- Get_Tasking --
   -----------------

   function Get_Tasking (P : PID_Type) return Boolean is
   begin
      return Partitions.Table (P).Use_Tasking;
   end Get_Tasking;

   -----------------
   -- Get_Tasking --
   -----------------

   function Get_Tasking (A : ALI_Id) return Character is
   begin
      return ALIs.Table (A).Task_Dispatching_Policy;
   end Get_Tasking;

   ---------------------
   -- Get_Termination --
   ---------------------

   function Get_Termination (P : PID_Type) return Termination_Type is
      Termination : Termination_Type;

   begin
      Termination := Partitions.Table (P).Termination;
      if Termination = Unknown_Termination then
         Termination := Partitions.Table (Default_Partition).Termination;
      end if;
      return Termination;
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

   function Get_Unit_Sfile (U : Unit_Id) return File_Name_Type is
   begin
      Get_Name_String (Units.Table (U).Sfile);
      Name_Len := Name_Len - 4;
      return Name_Find;
   end Get_Unit_Sfile;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
      P : PID_Type;
      C : CID_Type;
      N : Partition_Name_Type;
      F : LID_Type := Null_LID;
      L : LID_Type := Null_LID;

   begin
      Add_Location
        (F, L,
         Str_To_Id (Get_Def_Storage_Name),
         Str_To_Id (Get_Def_Storage_Data));
      Def_Data_Location := F;

      N := Get_Node_Name (Node_Id (Partition_Type_Node));
      Create_Partition (N, Null_Node, P);
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
      Channels.Table (C).Name := Get_Node_Name (Node_Id (Channel_Type_Node));

      Channels.Table (C).Filter := No_Filter_Name;
      Default_Channel := C;

   end Initialize;

   -----------------------
   -- Is_RCI_Or_SP_Unit --
   -----------------------

   function Is_RCI_Or_SP_Unit (U : ALI.Unit_Id) return Boolean is
   begin
      return Units.Table (U).RCI or else Units.Table (U).Shared_Passive;
   end Is_RCI_Or_SP_Unit;

   ------------
   -- Is_Set --
   ------------

   function Is_Set (Partition : PID_Type) return Boolean is
   begin
      return Partitions.Table (Partition).Last_Unit /= Null_CUID;
   end Is_Set;

   -----------------------
   -- Most_Recent_Stamp --
   -----------------------

   procedure Most_Recent_Stamp (P : in PID_Type; F : in File_Name_Type) is
      Most_Recent : File_Name_Type;
      Has_Changed : Boolean := False;
   begin
      Most_Recent := Partitions.Table (P).Most_Recent;
      if Most_Recent = No_Name then
         Partitions.Table (P).Most_Recent := F;
         Has_Changed := True;
      elsif Stamp (F) > Stamp (Most_Recent) then
         Partitions.Table (P).Most_Recent := F;
         Has_Changed := True;
      end if;
      if Debug_Mode and Has_Changed then
         Osint.Write_Program_Name;
         Write_Str  (": ");
         Write_Name (Partitions.Table (P).Name);
         Write_Str  ("'s most recent stamp is ");
         Write_Str  (Stamp (Partitions.Table (P).Most_Recent));
         Write_Eol;
         Osint.Write_Program_Name;
         Write_Str  (":    with file ");
         Write_Name (Partitions.Table (P).Most_Recent);
         Write_Eol;
      end if;
   end Most_Recent_Stamp;

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
      Channel   : in CID_Type)
   is
      Attr_Item : Variable_Id;
      Attr_Kind : Attribute_Type;
   begin

      --  Apply attribute to a channel.

      Attr_Kind := Get_Attribute_Kind (Component_Id (Attribute));
      Attr_Item := Get_Component_Value (Component_Id (Attribute));

      --  No attribute was really assigned.

      if Attr_Item = Null_Variable then
         return;
      end if;

      case Attr_Kind is
         when Attribute_CFilter =>

            --  Only string literals are allowed here.

            if Get_Variable_Type (Attr_Item) /= String_Type_Node then
               Write_SLOC (Node_Id (Attribute));
               Write_Name (Channels.Table (Channel).Name);
               Write_Str ("'s filter attribute must be ");
               Write_Str ("a string literal");
               Write_Eol;
               raise Parsing_Error;
            end if;

            --  Does it apply to all channels ? Therefore, check
            --  that this has not already been done.

            if Channel = Null_CID
              and then
              Channels.Table (Default_Channel).Filter = No_Filter_Name
            then
               Channels.Table (Default_Channel).Filter
                 := Get_Variable_Name (Attr_Item);
               To_Lower (Channels.Table (Default_Channel).Filter);

            --  Apply to one channel. Check that it has not already
            --  been done.

            elsif Channel /= Null_CID
              and then
              Channels.Table (Channel).Filter = No_Filter_Name
            then
               Channels.Table (Channel).Filter
                 := Get_Variable_Name (Attr_Item);
               To_Lower (Channels.Table (Channel).Filter);

            --  This operation has already been done !

            else
               Write_SLOC (Node_Id (Attr_Item));
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

   ---------------------
   -- Set_RCI_Or_RACW --
   ---------------------

   procedure Set_RCI_Or_RACW (P : PID_Type; B : Boolean) is
   begin
      Partitions.Table (P).RCI_Or_RACW := B;
   end Set_RCI_Or_RACW;

   -----------------------------
   -- Set_Partition_Attribute --
   -----------------------------

   procedure Set_Partition_Attribute
     (Attribute : in Attribute_Id;
      Partition : in PID_Type)
   is
      Attr_Item : Variable_Id;
      Attr_Kind : Attribute_Type;
      Attr_Type : Type_Id;
      Comp_Node : Component_Id;
      PID       : PID_Type;
      Ada_Unit  : Name_Id;

      Host      : HID_Type;
      Name      : Name_Id;
      Data      : Name_Id;

      procedure Write_Attr_Init_Error
        (Attr_Name : in String);
      procedure Write_Attr_Kind_Error
        (Attr_Name : in String;
         Attr_Kind : in String);

      procedure Write_Attr_Init_Error
        (Attr_Name : in String) is
      begin
         Write_SLOC (Node_Id (Attribute));
         Write_Name (Partitions.Table (Partition).Name);
         Write_Str ("'s ");
         Write_Str (Attr_Name);
         Write_Str (" attribute has been assigned twice");
         Write_Eol;
         raise Parsing_Error;
      end Write_Attr_Init_Error;

      procedure Write_Attr_Kind_Error
        (Attr_Name : in String;
         Attr_Kind : in String) is
      begin
         Write_SLOC (Node_Id (Attribute));
         Write_Name (Partitions.Table (Partition).Name);
         Write_Str ("'s ");
         Write_Str (Attr_Name);
         Write_Str (" attribute must be ");
         Write_Str (Attr_Kind);
         Write_Eol;
         raise Parsing_Error;
      end Write_Attr_Kind_Error;

   begin

      --  If this attribute applies to partition type itself, it may not
      --  have a value. No big deal, we use defaults.

      --  Apply attribute to a partition.

      Attr_Kind := Get_Attribute_Kind (Component_Id (Attribute));
      Attr_Item := Get_Component_Value (Component_Id (Attribute));

      --  No attribute was really assigned.

      if Attr_Item = Null_Variable then
         return;
      end if;

      PID := Partition;

      case Attr_Kind is
         when Attribute_PFilter =>

            --  Only string literals are allowed here.

            if Get_Variable_Type (Attr_Item) /= String_Type_Node then
               Write_Attr_Kind_Error ("filter", "a string literal");
            end if;

            --  Does it apply to all partitions ? Therefore, check
            --  that this has not already been done.

            if PID = Default_Partition
              and then
              Partitions.Table (PID).Filter = No_Filter_Name
            then
               Partitions.Table (PID).Filter := Get_Variable_Name (Attr_Item);
               To_Lower (Partitions.Table (PID).Filter);

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
               Write_Attr_Init_Error ("filter");
            end if;

         when Attribute_Directory =>

            --  Only strings are allowed here.

            if Get_Variable_Type (Attr_Item) /= String_Type_Node then
               Write_Attr_Kind_Error ("directory", "a string litteral");
            end if;

            --  Check that it has not already been assigned.

            if Partitions.Table (PID).Directory = No_Directory then
               Partitions.Table (PID).Directory
                 := Get_Variable_Name (Attr_Item);
            else
               Write_Attr_Init_Error ("directory");
            end if;

         when Attribute_Host =>

            Attr_Type := Get_Variable_Type (Attr_Item);
            case Get_Type_Kind (Attr_Type) is
               when Pre_Type_String =>
                  Hosts.Increment_Last;
                  Host := Hosts.Last;
                  Hosts.Table (Host).Name := Get_Variable_Name (Attr_Item);
                  Hosts.Table (Host).Static := True;

               when Pre_Type_Ada_Unit =>
                  Attr_Item := Get_Variable_Value (Attr_Item);
                  Build_New_Host (Subprogram_Id (Attr_Item), Host);

               when others =>
                  Write_Attr_Kind_Error ("host", "of string type");
            end case;

            --  Check that it has not already been assigned.

            if Partitions.Table (PID).Host = Null_HID then
               Partitions.Table (PID).Host := Host;
            else
               Write_Attr_Init_Error ("host");
            end if;

         when Attribute_Main =>

            --  Check that it has not already been assigned.

            if Partitions.Table (PID).Main_Subprogram = No_Main_Subprogram then
               Partitions.Table (PID).Main_Subprogram
                 := Get_Variable_Name (Attr_Item);

               --  We are not sure at this point that this unit
               --  has been configured on partition.

               Ada_Unit := Get_Variable_Name (Attr_Item);
               Add_Conf_Unit (Ada_Unit, PID);

            else
               Write_Attr_Init_Error ("main");
            end if;

         when Attribute_Command_Line =>

            --  Only strings are allowed.

            if Get_Variable_Type (Attr_Item) /= String_Type_Node then
               Write_Attr_Kind_Error ("command_line", "a string litteral");
            end if;

            --  Check that this has not already been assigned.

            if Partitions.Table (PID).Command_Line = No_Command_Line then
               Partitions.Table (PID).Command_Line
                 := Get_Variable_Name (Attr_Item);
            else
               Write_Attr_Init_Error ("command_line");
            end if;

         when Attribute_Termination =>

            if Get_Variable_Type (Attr_Item) /= Integer_Type_Node then
               Write_Attr_Kind_Error ("termination", "of termination type");
            end if;

            --  Check that it has not already been assigned.

            if Partitions.Table (PID).Termination = Unknown_Termination then
               Set_Termination
                 (PID, Termination_Type (Get_Scalar_Value (Attr_Item)));
            else
               Write_Attr_Init_Error ("termination");
            end if;

         when Attribute_Passive =>

            if Get_Variable_Type (Attr_Item) /= Boolean_Type_Node then
               Write_Attr_Kind_Error ("passive", "of boolean type");
            end if;

            --  Check that it has not already been assigned.

            if Partitions.Table (PID).Passive = Bunknown then
               Set_Passive
                 (PID, Boolean_Type (Get_Scalar_Value (Attr_Item)));
            else
               Write_Attr_Init_Error ("passive");
            end if;

         when Attribute_Reconnection =>

            if Get_Variable_Type (Attr_Item) /= Integer_Type_Node then
               Write_Attr_Kind_Error ("reconnection", "of reconnection type");
            end if;

            --  Check that it has not already been assigned.

            if Partitions.Table (PID).Reconnection = Unknown_Reconnection then
               Set_Reconnection (PID, Convert (Get_Scalar_Value (Attr_Item)));
            else
               Write_Attr_Init_Error ("reconnection");
            end if;

         when Attribute_Leader =>

            --  Internal attribute. Don't check anything.

            if Partition /= Null_PID then
               if Main_Partition /= Null_PID then
                  Write_SLOC (Node_Id (Attribute));
                  Write_Str  ("multiple definitions of ");
                  Write_Str  ("application main subprogram not allowed");
                  Write_Eol;
                  raise Parsing_Error;
               end if;
               Main_Partition := Partition;
            end if;

         when Attribute_Task_Pool =>

            First_Variable_Component (Attr_Item, Comp_Node);
            for B in Partitions.Table (PID).Task_Pool'Range loop
               Partitions.Table (PID).Task_Pool (B)
                 := Get_Variable_Name (Get_Component_Value (Comp_Node));
               Next_Variable_Component (Comp_Node);
            end loop;

         when Attribute_Storage =>
            if Partitions.Table (PID).Mem_Location = Null_LID then
               First_Variable_Component (Attr_Item, Comp_Node);
               Name := Get_Variable_Name (Get_Component_Value (Comp_Node));
               Next_Variable_Component (Comp_Node);
               Data := Get_Variable_Name (Get_Component_Value (Comp_Node));
               declare
                  LID : LID_Type;
               begin
                  Locations.Increment_Last;
                  LID := Locations.Last;
                  Locations.Table (LID).Major := Name;
                  Locations.Table (LID).Minor := Data;
                  Locations.Table (LID).Next := Null_LID;
                  Set_Storage (PID, LID);
               end;

            else
               Write_Attr_Init_Error ("storage");
            end if;

         when Attribute_Protocol =>
            if Partitions.Table (PID).F_Net_Location = Null_LID then
               Attr_Type := Get_Variable_Type (Attr_Item);

               case Get_Type_Kind (Attr_Type) is
                  when Pre_Type_Location =>
                     First_Variable_Component (Attr_Item, Comp_Node);
                     Name
                       := Get_Variable_Name (Get_Component_Value (Comp_Node));
                     Next_Variable_Component (Comp_Node);
                     Data
                       := Get_Variable_Name (Get_Component_Value (Comp_Node));
                     Add_Location
                       (Partitions.Table (PID).F_Net_Location,
                        Partitions.Table (PID).L_Net_Location,
                        Name, Data);

                  when Pre_Type_Locations =>
                     First_Variable_Component (Attr_Item, Comp_Node);
                     while Comp_Node /= Null_Component loop
                        declare
                           C : Component_Id;
                           V : Variable_Id;
                        begin
                           V := Get_Component_Value (Comp_Node);
                           First_Variable_Component (V, C);
                           Name
                             := Get_Variable_Name (Get_Component_Value (C));
                           Next_Variable_Component (C);
                           Data
                             := Get_Variable_Name (Get_Component_Value (C));
                           Add_Location
                             (Partitions.Table (PID).F_Net_Location,
                              Partitions.Table (PID).L_Net_Location,
                              Name, Data);
                        end;
                        Next_Variable_Component (Comp_Node);
                     end loop;

                  when others =>
                     raise Parsing_Error;
               end case;

            else
               Write_Attr_Init_Error ("location");
            end if;

         when Attribute_CFilter | Attribute_Unknown =>
            raise Fatal_Error;

      end case;
   end Set_Partition_Attribute;

   -----------------
   -- Set_Passive --
   -----------------

   procedure Set_Passive
     (P : in PID_Type;
      B : in XE.Boolean_Type) is
   begin
      Partitions.Table (P).Passive := B;
   end Set_Passive;

   -------------
   -- Set_PID --
   -------------

   procedure Set_PID
     (N : in Name_Id;
      P : in PID_Type) is
   begin
      Set_Name_Table_Info (N, Int (P));
   end Set_PID;

   --------------------------
   -- Set_Pragma_Statement --
   --------------------------

   procedure Set_Pragma_Statement
     (Subprogram  : in Subprogram_Id)
   is
      Pragma_Kind : Pragma_Type;
      Parameter   : Parameter_Id;
      Method      : Import_Method_Type;
      Value       : Variable_Id;
      Host        : HID_Type;
      Name        : Name_Id;
      Data        : Name_Id;
      Param_Type  : Type_Id;
      Param_Kind  : Predefined_Type;

   begin

      --  Apply pragma statement.

      Pragma_Kind := Get_Pragma_Kind (Subprogram);
      First_Subprogram_Parameter (Subprogram, Parameter);

      case Pragma_Kind is
         when Pragma_Import =>
            Value := Get_Parameter_Value (Parameter);
            Method := Convert (Get_Scalar_Value (Value));
            Next_Subprogram_Parameter (Parameter);
            Value := Get_Parameter_Value (Parameter);
            Value := Get_Variable_Value (Value);

            --  We are not sure that this function has been already
            --  declared as an host function.

            Build_New_Host (Subprogram_Id (Value), Host);

            --  Apply Import pragma ...

            Hosts.Table (Host).Import := Method;
            Next_Subprogram_Parameter (Parameter);
            Value := Get_Parameter_Value (Parameter);
            Hosts.Table (Host).External := Get_Variable_Name (Value);

         when Pragma_Remote_Shell =>
            Value := Get_Parameter_Value (Parameter);
            Default_Rsh_Command := Get_Variable_Name (Value);

            Next_Subprogram_Parameter (Parameter);

            Value := Get_Parameter_Value (Parameter);
            Default_Rsh_Options := Get_Variable_Name (Value);

         when Pragma_Starter =>
            Value := Get_Parameter_Value (Parameter);
            Default_Starter := Convert (Get_Scalar_Value (Value));

         when Pragma_Boot_Location =>
            if Def_Boot_Location_First /= Null_LID then
               Write_SLOC (Node_Id (Subprogram));
               Write_Str  ("multiple boot location definition not allowed");
               Write_Eol;
               raise Parsing_Error;
            end if;

            Param_Type := Get_Parameter_Type (Parameter);
            Param_Kind := Get_Type_Kind (Param_Type);
            case Param_Kind is
               when Pre_Type_String =>
                  Value := Get_Parameter_Value (Parameter);
                  Name  := Get_Variable_Name (Value);
                  Next_Subprogram_Parameter (Parameter);
                  Value := Get_Parameter_Value (Parameter);
                  Data  := Get_Variable_Name (Value);
                  Add_Location
                    (Def_Boot_Location_First,
                     Def_Boot_Location_Last,
                     Name,
                     Data);

               when Pre_Type_Location =>
                  declare
                     V : Variable_Id;
                     C : Component_Id;
                  begin
                     V := Get_Parameter_Value (Parameter);
                     First_Variable_Component (V, C);
                     Name := Get_Variable_Name (Get_Component_Value (C));
                     Next_Variable_Component (C);
                     Data := Get_Variable_Name (Get_Component_Value (C));
                     Add_Location
                       (Def_Boot_Location_First,
                        Def_Boot_Location_Last,
                        Name, Data);
                  end;

               when Pre_Type_Locations =>
                  declare
                     V1 : Variable_Id;
                     C1 : Component_Id;
                  begin
                     V1 := Get_Parameter_Value (Parameter);
                     First_Variable_Component (V1, C1);
                     while C1 /= Null_Component loop
                        declare
                           V2 : Variable_Id;
                           C2 : Component_Id;
                        begin
                           V2 := Get_Component_Value (C1);
                           First_Variable_Component (V2, C2);
                           Name
                             := Get_Variable_Name (Get_Component_Value (C2));
                           Next_Variable_Component (C2);
                           Data
                             := Get_Variable_Name (Get_Component_Value (C2));
                           Add_Location
                             (Def_Boot_Location_First,
                              Def_Boot_Location_Last,
                              Name, Data);
                        end;
                        Next_Variable_Component (C1);
                     end loop;
                  end;

               when others =>
                  raise Program_Error;
            end case;

         when Pragma_Version =>
            Value := Get_Parameter_Value (Parameter);
            Default_Version_Check := (Get_Scalar_Value (Value) = Int (Btrue));

         when Pragma_Reg_Filter =>
            Value := Get_Parameter_Value (Parameter);
            Default_Registration_Filter := Get_Variable_Name (Value);

         when Pragma_Priority =>
            raise Program_Error;

         when Pragma_Unknown =>
            raise Program_Error;

      end case;
   end Set_Pragma_Statement;

   ----------------------
   -- Set_Reconnection --
   ----------------------

   procedure Set_Reconnection
     (P : in PID_Type;
      R : in Reconnection_Type) is
   begin
      Partitions.Table (P).Reconnection := R;
   end Set_Reconnection;

   -----------------
   -- Set_Storage --
   -----------------

   procedure Set_Storage
     (P : in PID_Type;
      L : in LID_Type) is
   begin
      Partitions.Table (P).Mem_Location := L;
   end Set_Storage;

   -----------------
   -- Set_Tasking --
   -----------------

   procedure Set_Tasking
     (P : in PID_Type;
      B : in Boolean) is
   begin
      Partitions.Table (P).Use_Tasking := B;
   end Set_Tasking;

   -----------------
   -- Set_Tasking --
   -----------------

   procedure Set_Tasking (A : ALI_Id; Tasking : Character) is
   begin
      ALIs.Table (A).Task_Dispatching_Policy := Tasking;
   end Set_Tasking;

   ---------------------
   -- Set_Termination --
   ---------------------

   procedure Set_Termination
     (P : in PID_Type;
      T : in Termination_Type) is
   begin
      Partitions.Table (P).Termination := T;
   end Set_Termination;

   ------------------------
   -- Set_Type_Attribute --
   ------------------------

   procedure Set_Type_Attribute (Pre_Type : in Type_Id) is
      Component_Node : Component_Id;
      Pre_Type_Id    : Predefined_Type;

   begin
      if Is_Type_Composite (Pre_Type) then
         Pre_Type_Id := Get_Type_Kind (Pre_Type);
         First_Type_Component (Pre_Type, Component_Node);
         while Component_Node /= Null_Component loop
            if Get_Attribute_Kind (Component_Node) /= Attribute_Unknown
              and then Is_Component_Initialized (Component_Node)
            then
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

   procedure Set_Unit_Id (N : in Name_Id; U : in Unit_Id) is
   begin
      Set_Name_Table_Info (N, Int (U));
   end Set_Unit_Id;

   ------------------------
   -- Show_Configuration --
   ------------------------

   procedure Show_Configuration is
   begin
      Write_Str (" ------------------------------");
      Write_Eol;
      Write_Str (" ---- Configuration report ----");
      Write_Eol;
      Write_Str (" ------------------------------");
      Write_Eol;
      Write_Str ("Configuration :");
      Write_Eol;

      Write_Field (1, "Name");
      Write_Name  (Configuration);
      Write_Eol;

      Write_Field (1, "Main");
      Write_Name  (Main_Subprogram);
      Write_Eol;

      Write_Field (1, "Starter");
      case Default_Starter is
         when Ada_Import =>
            Write_Str ("Ada code");
         when Shell_Import =>
            Write_Str ("shell script");
         when None_Import =>
            Write_Str ("none");
      end case;
      Write_Eol;

      if Def_Boot_Location_First /= Null_LID then
         Write_Field (1, "Protocols");
         declare
            LID : LID_Type := Def_Boot_Location_First;
            One : Boolean  := (Locations.Table (LID).Next = Null_LID);
         begin
            if One then
               Write_Name (Locations.Table (LID).Major);
               Write_Str  ("://");
               Write_Name (Locations.Table (LID).Minor);

            else
               while LID /= Null_LID loop
                  Write_Eol;
                  Write_Str ("             - ");
                  Write_Name (Locations.Table (LID).Major);
                  Write_Str  ("://");
                  Write_Name (Locations.Table (LID).Minor);
                  LID := Locations.Table (LID).Next;
               end loop;
            end if;
            Write_Eol;
         end;
      end if;
      Write_Eol;

      for P in Partitions.First + 1 .. Partitions.Last loop
         Show_Partition (P);
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
               Write_Field (1, "Partition 1");
               P := Channels.Table (C).Lower.My_Partition;
               Write_Name (Partitions.Table (P).Name);
               Write_Eol;
               Write_Field (1, "Partition 2");
               P := Channels.Table (C).Upper.My_Partition;
               Write_Name (Partitions.Table (P).Name);
               Write_Eol;
               F := Get_Filter (C);
               if F /= No_Filter_Name then
                  Write_Field  (1, "Filter");
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

   --------------------
   -- Show_Partition --
   --------------------

   procedure Show_Partition
     (PID : in PID_Type)
   is
      M : Main_Subprogram_Type;
      H : HID_Type;
      S : Directory_Name_Type;
      C : Command_Line_Type;
      T : Task_Pool_Type;
      U : CUID_Type;

   begin
      Write_Str  ("Partition ");
      Write_Name (Partitions.Table (PID).Name);
      Write_Eol;

      M := Get_Main_Subprogram (PID);
      if M /= No_Main_Subprogram then
         Write_Field (1, "Main");
         Write_Name (M);
         Write_Eol;
      end if;

      H := Partitions.Table (PID).Host;
      if H = Null_HID then
         H := Partitions.Table (Default_Partition).Host;
      end if;

      if H /= Null_HID then
         Write_Field (1, "Host");
         if Hosts.Table (H).Static then
            Write_Name (Hosts.Table (H).Name);
         else
            Write_Str ("function call :: ");
            Write_Name (Hosts.Table (H).External);
            case Hosts.Table (H).Import is
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

      S := Get_Directory (PID);
      if S /= No_Directory then
         Write_Field (1, "Directory");
         Write_Name (S);
         Write_Eol;
      end if;

      C := Get_Command_Line (PID);
      if C /= No_Command_Line then
         Write_Field (1, "Command");
         Write_Name (C);
         Write_Eol;
      end if;

      T := Get_Task_Pool (PID);
      if T /= No_Task_Pool then
         Write_Field (1, "Task Pool");
         for B in T'Range loop
            Write_Name (T (B));
            Write_Str (" ");
         end loop;
         Write_Eol;
      end if;

      if Get_Termination (PID) /= Unknown_Termination then
         Write_Field (1, "Termination");
         case Get_Termination (PID) is
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

      if Get_Protocol (PID) /= Null_LID then
         Write_Field  (1, "Protocols");
         Write_Eol;
         declare
            L : LID_Type := Get_Protocol (PID);
         begin
            while L /= Null_LID loop
               Write_Str  ("             - ");
               Write_Name (Locations.Table (L).Major);
               if Locations.Table (L).Minor /= No_Name then
                  Write_Str ("://");
                  Write_Name (Locations.Table (L).Minor);
               end if;
               Write_Eol;
               L := Locations.Table (L).Next;
            end loop;
         end;
      end if;

      if Get_Storage (PID) /= Null_LID then
         Write_Field  (1, "Storages");
         Write_Eol;
         declare
            L : LID_Type := Get_Storage (PID);
         begin
            while L /= Null_LID loop
               Write_Str  ("             - ");
               Write_Name (Locations.Table (L).Major);
               if Locations.Table (L).Minor /= No_Name then
                  Write_Str ("://");
                  Write_Name (Locations.Table (L).Minor);
               end if;
               Write_Eol;
               L := Locations.Table (L).Next;
            end loop;
         end;
      end if;

      if Partitions.Table (PID).First_Unit /= Null_CUID then
         Write_Field (1, "Units");
         Write_Eol;

         U := Partitions.Table (PID).First_Unit;
         while U /= Null_CUID loop
            Write_Str ("             - ");
            Write_Name (CUnits.Table (U).CUname);
            if Units.Table (CUnits.Table (U).My_Unit).RCI then
               Write_Str (" (rci)");
            elsif Units.Table (CUnits.Table (U).My_Unit).Remote_Types then
               Write_Str (" (rt)");
            elsif Units.Table (CUnits.Table (U).My_Unit).Shared_Passive then
               Write_Str (" (sp)");
            else
               Write_Str (" (normal)");
            end if;
            Write_Eol;
            U := CUnits.Table (U).Next;
         end loop;
         Write_Eol;
      end if;
   end Show_Partition;

   --------------
   -- To_Build --
   --------------

   function To_Build (U : CUID_Type) return Boolean is
   begin
      return Partitions.Table (CUnits.Table (U).Partition).To_Build;
   end To_Build;

   -----------------
   -- Write_Field --
   -----------------

   procedure Write_Field
     (Indent : in Natural;
      Field  : in String;
      Width  : in Natural := Min_Width)
   is
      W : Natural := Width;

   begin
      for I in 1 .. Indent loop
         Write_Str ("   ");
      end loop;
      if Field'Length > W then
         W := Field'Length;
      end if;
      declare
         L : String (1 .. W) := (others => ' ');
      begin
         L (1 .. Field'Length) := Field;
         Write_Str (L);
      end;
      Write_Str (" : ");
   end Write_Field;

end XE_Back;
