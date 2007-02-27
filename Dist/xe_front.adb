------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--                             X E _ F R O N T                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 1995-2007, Free Software Foundation, Inc.          --
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

with XE_Back;     use XE_Back;
with XE_Defs;     use XE_Defs;
with XE_Flags;    use XE_Flags;
with XE_IO;       use XE_IO;
with XE_Names;    use XE_Names;
with XE_Utils;    use XE_Utils;

package body XE_Front is

   Min_Field_Width : constant := 11;

   procedure Add_Channel_Partition (P : Partition_Id; C : Channel_Id);
   --  Assign a partition to a channel. Sort the partition pair.

   procedure Build_New_Channel (V : Variable_Id);
   --  Retrieve the two partitions and attributes previously parsed in
   --  order to build the channel.

   procedure Build_New_Host
     (Subprogram : Subprogram_Id;
      Host_Entry : out Host_Id);

   procedure Build_New_Partition (V : Variable_Id);
   --  Retrieve ada units and attributes previously parsed in order to
   --  build the partition.

   procedure Build_New_Variable
     (Variable : Variable_Id);
   --  Dispatching procedure to create entities of different types.

   procedure Set_Channel_Attribute
     (Attribute : Attribute_Id;
      Channel   : Channel_Id);

   procedure Set_Partition_Attribute
     (Attribute : Attribute_Id;
      Partition : Partition_Id);

   procedure Set_Pragma_Statement
     (Subprogram : Subprogram_Id);

   procedure Set_Type_Attribute
     (Pre_Type : Type_Id);

   procedure Show_Partition (P : Partition_Id);
   --  Output the different attributes of a partition.

   procedure Write_Field
     (Indent : Natural;
      Field  : String;
      Width  : Natural := Min_Field_Width);
   --  Output field with at least Width characters and indent it.

   ---------------------------
   -- Add_Channel_Partition --
   ---------------------------

   procedure Add_Channel_Partition
     (P : Partition_Id;
      C : Channel_Id)
   is
      procedure Update_Channel_Partition
        (Channel_Partition : in out Channel_Partition_Type;
         Partition         : Partition_Id;
         Channel           : Channel_Id);
      --  Link Channel into the list of partition Channels. The head of
      --  this list is First_Channel (Partitions) and the tail is
      --  Last_Channel. Next elements are Next_Channel (Channels).

      procedure Update_Channel_Partition
        (Channel_Partition : in out Channel_Partition_Type;
         Partition         : Partition_Id;
         Channel           : Channel_Id)
      is
         CID : Channel_Id;

      begin
         Channel_Partition.My_Partition := Partition;
         Channel_Partition.Next_Channel := No_Channel_Id;

         CID := Partitions.Table (Partition).Last_Channel;
         if CID = No_Channel_Id then
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
         Message ("add partition", Partitions.Table (P).Name,
                  "to channel", Channels.Table (C).Name);
      end if;

      if Channels.Table (C).Lower = Null_Channel_Partition then
         Update_Channel_Partition (Channels.Table (C).Lower, P, C);

      elsif P > Channels.Table (C).Lower.My_Partition then
         Update_Channel_Partition (Channels.Table (C).Upper, P, C);

      else
         Channels.Table (C).Upper := Channels.Table (C).Lower;
         Update_Channel_Partition (Channels.Table (C).Lower, P, C);
      end if;
   end Add_Channel_Partition;

   -------------------
   -- Add_Conf_Unit --
   -------------------

   procedure Add_Conf_Unit (U : Unit_Name_Type; P : Partition_Id) is
   begin
      if Get_Partition_Id (U) = P then
         return;
      end if;

      if Debug_Mode then
         Message ("configuring unit", U,
                  "on partition", Partitions.Table (P).Name);
      end if;

      --  Mark this configured unit as already partitioned

      Set_Partition_Id (U, P);

      --  The same unit can be multiply declared especially if
      --  this unit is a normal package

      Conf_Units.Increment_Last;
      Conf_Units.Table (Conf_Units.Last) := Null_Conf_Unit;
      Conf_Units.Table (Conf_Units.Last).Partition := P;
      Conf_Units.Table (Conf_Units.Last).Name      := U;

      --  Update partition single linked list of configured units

      if Partitions.Table (P).First_Unit = No_Conf_Unit_Id then
         Partitions.Table (P).First_Unit := Conf_Units.Last;

      else
         Conf_Units.Table (Partitions.Table (P).Last_Unit).Next_Unit :=
           Conf_Units.Last;
      end if;
      Partitions.Table (P).Last_Unit := Conf_Units.Last;
   end Add_Conf_Unit;

   ------------------
   -- Add_Location --
   ------------------

   procedure Add_Location
     (First : in out Location_Id;
      Last  : in out Location_Id;
      Major : Name_Id;
      Minor : Name_Id)
   is
      L : Location_Id;

   begin
      --  Add a new element in the location table and fill it with the
      --  protocol name (major) and the protocol data (minor).

      Locations.Increment_Last;
      L := Locations.Last;
      Locations.Table (L).Major         := Major;
      Locations.Table (L).Minor         := Minor;
      Locations.Table (L).Next_Location := No_Location_Id;

      --  Link this new location to the end of the partition location
      --  list.

      if First = No_Location_Id then
         First := L;

      else
         Locations.Table (Last).Next_Location := L;
      end if;
      Last := L;
   end Add_Location;

   -----------------------
   -- Build_New_Channel --
   -----------------------

   procedure Build_New_Channel (V : Variable_Id) is
      Channel_Name   : Name_Id;
      Partition_Name : Name_Id;
      Partition_Node : Variable_Id;
      Component_Node : Component_Id;
      Partition      : Partition_Id;
      Channel        : Channel_Id;

   begin
      Channel_Name := Get_Variable_Name (V);

      --  Create a new entry in Channels.Table

      Create_Channel (Channel_Name, Node_Id (V), Channel);

      --  Scan Channel_Name partition pair and Channel_Name attributes

      First_Variable_Component (V, Component_Node);
      while Component_Node /= Null_Component loop

         --  This is a partition (upper or lower)

         if Get_Attribute_Kind (Component_Node) = Attribute_Unknown then
            if Is_Component_Initialized (Component_Node) then

               --  Append this partition to the pair

               Partition_Node := Get_Component_Value (Component_Node);
               Partition_Name := Get_Variable_Name (Partition_Node);
               Partition      := Get_Partition_Id (Partition_Name);

               if Partition = No_Partition_Id then
                  Write_SLOC (Node_Id (Partition_Node));
                  Write_Str  ("no such partition ");
                  Write_Name (Partition_Name);
                  Write_Eol;
                  raise Parsing_Error;
               end if;

               Add_Channel_Partition (Partition, Channel);
            end if;

         else
            Set_Channel_Attribute (Attribute_Id (Component_Node), Channel);
         end if;
         Next_Variable_Component (Component_Node);
      end loop;
   end Build_New_Channel;

   --------------------
   -- Build_New_Host --
   --------------------

   procedure Build_New_Host
     (Subprogram : Subprogram_Id;
      Host_Entry : out Host_Id)
   is
      Host : Host_Id;
      Name : Name_Id;
      Node : Node_Id;

   begin
      Node := Node_Id (Subprogram);
      Name := Get_Node_Name (Node);
      Host := Get_Host_Id (Name);

      if Host = No_Host_Id then
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

   procedure Build_New_Partition (V : Variable_Id) is
      Partition_Name : Name_Id;
      Ada_Unit_Name  : Name_Id;
      Ada_Unit_Node  : Variable_Id;
      Component_Node : Component_Id;
      Partition   : Partition_Id;

   begin
      Partition_Name := Get_Variable_Name (V);

      --  Create a new entry into Partitions.Table

      Create_Partition (Partition_Name, Node_Id (V), Partition);

      --  Scan Partition_Name ada unit list and Partition_Name attributes

      First_Variable_Component (V, Component_Node);
      while Component_Node /= Null_Component loop
         if Get_Attribute_Kind (Component_Node) = Attribute_Unknown then

            --  Append this unit to the partition list

            Ada_Unit_Node := Get_Component_Value (Component_Node);
            Ada_Unit_Name := Get_Variable_Name (Ada_Unit_Node);
            Add_Conf_Unit (Ada_Unit_Name, Partition);

         else

            --  This information is a partition attribute

            Set_Partition_Attribute (Attribute_Id (Component_Node), Partition);

         end if;
         Next_Variable_Component (Component_Node);
      end loop;
   end Build_New_Partition;

   ------------------------
   -- Build_New_Variable --
   ------------------------

   procedure Build_New_Variable
     (Variable : Variable_Id)
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

   --------------------
   -- Create_Channel --
   --------------------

   procedure Create_Channel
     (Name : Channel_Name_Type;
      Node : Node_Id;
      CID  : out Channel_Id)
   is
      Channel : Channel_Id;

   begin
      if Debug_Mode then
         Message ("create channel", Name);
      end if;

      Channels.Increment_Last;
      Channel := Channels.Last;
      Set_Channel_Id (Name, Channel);
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
     (Name : Host_Name_Type;
      Node : Node_Id;
      HID  : out Host_Id)
   is
      Host : Host_Id;

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
      Hosts.Table (Host).Most_Recent     := No_File_Name;
      Set_Host_Id (Name, Host);
      HID := Host;
   end Create_Host;

   ----------------------
   -- Create_Partition --
   ----------------------

   procedure Create_Partition
     (Name : Partition_Name_Type;
      Node : Node_Id;
      PID  : out Partition_Id)
   is
      Partition : Partition_Id;

   begin
      if Debug_Mode then
         Message ("create partition", Name);
      end if;

      Partitions.Increment_Last;
      Partition := Partitions.Last;
      Set_Partition_Id (Name, Partition);
      Partitions.Table (Partition) := Null_Partition;
      Partitions.Table (Partition).Name := Name;
      Partitions.Table (Partition).Node := Node;
      PID := Partition;
   end Create_Partition;

   --------------
   -- Frontend --
   --------------

   procedure Frontend is
      Node : Node_Id;
      HID  : Host_Id;

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
         if HID /= No_Host_Id
           and then not Hosts.Table (HID).Static
           and then Hosts.Table (HID).Import = Ada_Import then
            Add_Conf_Unit (Hosts.Table (HID).External, P);
         end if;
      end loop;

      if Main_Partition = No_Partition_Id then
         Write_SLOC (Node_Id (Configuration_Node));
         Write_Str  ("non-dist. app. main subprogram has not been declared");
         Write_Eol;
         raise Parsing_Error;
      end if;
   end Frontend;

   ----------------
   -- Get_ALI_Id --
   ----------------

   function Get_ALI_Id (N : Name_Id) return ALI_Id is
      Info : Int;

   begin
      Info := Get_Name_Table_Info (Name (N));
      case Info is
         when Int (ALI_Id'First) .. Int (ALI_Id'Last) =>
            null;

         when others =>
            raise Program_Error;
      end case;
      return ALI_Id (Info);
   end Get_ALI_Id;

   --------------------
   -- Get_Channel_Id --
   --------------------

   function Get_Channel_Id (N : Name_Id) return Channel_Id is
      Info : Int;

   begin
      Info := Get_Name_Table_Info (Name (N));
      case Info is
         when Int (Channel_Id'First) .. Int (Channel_Id'Last) =>
            null;

         when 0 =>
            Info := Int (No_Channel_Id);

         when others =>
            raise Program_Error;
      end case;
      return Channel_Id (Info);
   end Get_Channel_Id;

   ----------------------
   -- Get_Conf_Unit_Id --
   ----------------------

   function Get_Conf_Unit_Id (N : Name_Id) return Conf_Unit_Id is
      Info : Int;

   begin
      Info := Get_Name_Table_Info (Name (N));
      if Info in Int (Conf_Unit_Id'First) .. Int (Conf_Unit_Id'Last) then
         return Conf_Unit_Id (Info);
      end if;
      return No_Conf_Unit_Id;
   end Get_Conf_Unit_Id;

   -----------------
   -- Get_Host_Id --
   -----------------

   function Get_Host_Id (N : Name_Id) return Host_Id is
      Info : Int;

   begin
      Info := Get_Name_Table_Info (N);
      if Info in Int (Host_Id'First) .. Int (Host_Id'Last) then
         return Host_Id (Info);
      end if;
      return No_Host_Id;
   end Get_Host_Id;

   ----------------------
   -- Get_Partition_Id --
   ----------------------

   function Get_Partition_Id (N : Name_Id) return Partition_Id is
      PID  : Int;
      Info : Byte;

   begin
      Info := Get_Name_Table_Byte (Name (N));
      PID  := Int (Info) + Int (Partition_Id'First);
      if PID in Int (Partition_Id'First) .. Int (Partition_Id'Last) then
         return Partition_Id (PID);
      end if;
      return No_Partition_Id;
   end Get_Partition_Id;

   ---------------------
   -- Get_Rsh_Command --
   ---------------------

   function Get_Rsh_Command return XE_Types.Name_Id is
   begin
      if No (Default_Rsh_Command) then
         return Id (XE_Defs.Get_Rsh_Command);
      end if;
      return Default_Rsh_Command;
   end Get_Rsh_Command;

   ---------------------
   -- Get_Rsh_Options --
   ---------------------

   function Get_Rsh_Options return XE_Types.Name_Id is
   begin
      if No (Default_Rsh_Options) then
         return Id (XE_Defs.Get_Rsh_Options);
      end if;
      return Default_Rsh_Options;
   end Get_Rsh_Options;

   -----------------
   -- Get_Tasking --
   -----------------

   function Get_Tasking (A : ALI_Id) return Character is
   begin
      return ALIs.Table (A).Tasking;
   end Get_Tasking;

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

         when 0 =>
            Info := Int (No_Unit_Id);

         when others =>
            raise Program_Error;
      end case;
      return Unit_Id (Info);
   end Get_Unit_Id;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
      P : Partition_Id;
      C : Channel_Id;
      N : Partition_Name_Type;
      F : Location_Id := No_Location_Id;
      L : Location_Id := No_Location_Id;

   begin
      Add_Location
        (F, L,
         Id (Get_Def_Storage_Name),
         Id (Get_Def_Storage_Data));
      Default_Data_Location := F;

      N := Get_Node_Name (Node_Id (Partition_Type_Node));
      Create_Partition (N, Null_Node, P);
      Default_Partition_Id := P;

      Channels.Increment_Last;
      C := Channels.Last;
      Channels.Table (C).Name := Get_Node_Name (Node_Id (Channel_Type_Node));

      Channels.Table (C).Filter := No_Filter_Name;
      Default_Channel_Id := C;

   end Initialize;

   ----------------
   -- Set_ALI_Id --
   ----------------

   procedure Set_ALI_Id (N : Name_Id; A : ALI_Id) is
   begin
      Set_Name_Table_Info (Name (N), Int (A));
   end Set_ALI_Id;

   ---------------------------
   -- Set_Channel_Attribute --
   ---------------------------

   procedure Set_Channel_Attribute
     (Attribute : Attribute_Id;
      Channel   : Channel_Id)
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

            if Channel = No_Channel_Id
              and then
              Channels.Table (Default_Channel_Id).Filter = No_Filter_Name
            then
               Channels.Table (Default_Channel_Id).Filter
                 := Get_Variable_Name (Attr_Item);
               To_Lower (Channels.Table (Default_Channel_Id).Filter);

            --  Apply to one channel. Check that it has not already
            --  been done.

            elsif Channel /= No_Channel_Id
              and then
              Channels.Table (Channel).Filter = No_Filter_Name
            then
               Channels.Table (Channel).Filter
                 := Get_Variable_Name (Attr_Item);
               To_Lower (Channels.Table (Channel).Filter);

            --  This operation has already been done !

            else
               Write_SLOC (Node_Id (Attr_Item));
               if Channel = No_Channel_Id then
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

   --------------------
   -- Set_Channel_Id --
   --------------------

   procedure Set_Channel_Id (N : Name_Id; C : Channel_Id) is
   begin
      Set_Name_Table_Info (N, Int (C));
   end Set_Channel_Id;

   ----------------------
   -- Set_Conf_Unit_Id --
   ----------------------

   procedure Set_Conf_Unit_Id (N : Name_Id; U : Conf_Unit_Id) is
   begin
      Set_Name_Table_Info (Name (N), Int (U));
   end Set_Conf_Unit_Id;

   -----------------
   -- Set_Host_Id --
   -----------------

   procedure Set_Host_Id (N : Name_Id; H : Host_Id) is
   begin
      Set_Name_Table_Info (N, Int (H));
   end Set_Host_Id;

   -----------------------------
   -- Set_Partition_Attribute --
   -----------------------------

   procedure Set_Partition_Attribute
     (Attribute : Attribute_Id;
      Partition : Partition_Id)
   is
      Attr_Item : Variable_Id;
      Attr_Kind : Attribute_Type;
      Attr_Type : Type_Id;
      Comp_Node : Component_Id;
      Ada_Unit  : Name_Id;
      Current   : Partition_Type renames Partitions.Table (Partition);

      Host      : Host_Id;
      Name      : Name_Id;
      Data      : Name_Id;

      procedure Write_Attr_Init_Error
        (Attr_Name : String);

      procedure Write_Attr_Kind_Error
        (Attr_Name : String;
         Attr_Kind : String);

      ---------------------------
      -- Write_Attr_Init_Error --
      ---------------------------

      procedure Write_Attr_Init_Error
        (Attr_Name : String) is
      begin
         Write_SLOC (Node_Id (Attribute));
         Write_Name (Partitions.Table (Partition).Name);
         Write_Str ("'s ");
         Write_Str (Attr_Name);
         Write_Str (" attribute has been assigned twice");
         Write_Eol;
         raise Parsing_Error;
      end Write_Attr_Init_Error;

      ---------------------------
      -- Write_Attr_Kind_Error --
      ---------------------------

      procedure Write_Attr_Kind_Error
        (Attr_Name : String;
         Attr_Kind : String) is
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

      case Attr_Kind is
         when Attribute_PFilter =>

            --  Only string literals are allowed here.

            if Get_Variable_Type (Attr_Item) /= String_Type_Node then
               Write_Attr_Kind_Error ("filter", "a string literal");
            end if;

            --  Does it apply to all partitions ? Therefore, check
            --  that this has not already been done.

            if Partition = Default_Partition_Id
              and then Current.Filter = No_Filter_Name
            then
               Current.Filter := Get_Variable_Name (Attr_Item);
               To_Lower (Current.Filter);

            --  Apply to one partition. Check that it has not already
            --  been done.

            elsif Partition /= Default_Partition_Id then
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

            if Current.Executable_Dir = No_Directory_Name then
               Current.Executable_Dir
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

            if Current.Host = No_Host_Id then
               Current.Host := Host;
            else
               Write_Attr_Init_Error ("host");
            end if;

         when Attribute_Main =>

            --  Check that it has not already been assigned.

            if Current.Main_Subprogram = No_Main_Subprogram then
               Current.Main_Subprogram
                 := Get_Variable_Name (Attr_Item);

               --  We are not sure at this point that this unit
               --  has been configured on partition.

               Ada_Unit := Get_Variable_Name (Attr_Item);
               Add_Conf_Unit (Ada_Unit, Partition);

            else
               Write_Attr_Init_Error ("main");
            end if;

         when Attribute_Command_Line =>

            --  Only strings are allowed.

            if Get_Variable_Type (Attr_Item) /= String_Type_Node then
               Write_Attr_Kind_Error ("command_line", "a string litteral");
            end if;

            --  Check that this has not already been assigned.

            if Current.Command_Line = No_Command_Line then
               Current.Command_Line
                 := Get_Variable_Name (Attr_Item);
            else
               Write_Attr_Init_Error ("command_line");
            end if;

         when Attribute_Termination =>

            if Get_Variable_Type (Attr_Item) /= Integer_Type_Node then
               Write_Attr_Kind_Error ("termination", "of termination type");
            end if;

            --  Check that it has not already been assigned.

            if Current.Termination = No_Termination then
               Current.Termination :=
                 Termination_Type (Get_Scalar_Value (Attr_Item));
            else
               Write_Attr_Init_Error ("termination");
            end if;

         when Attribute_Passive =>

            if Get_Variable_Type (Attr_Item) /= Boolean_Type_Node then
               Write_Attr_Kind_Error ("passive", "of boolean type");
            end if;

            --  Check that it has not already been assigned.

            if Current.Passive = BMaybe then
               Current.Passive :=
                 Boolean_Type (Get_Scalar_Value (Attr_Item));
            else
               Write_Attr_Init_Error ("passive");
            end if;

         when Attribute_Allow_Light_PCS =>

            if Get_Variable_Type (Attr_Item) /= Boolean_Type_Node then
               Write_Attr_Kind_Error ("allow_light_pcs", "of boolean type");
            end if;

            --  Check that it has not already been assigned.

            if Current.Light_PCS = BMaybe then
               Current.Light_PCS :=
                 Boolean_Type (Get_Scalar_Value (Attr_Item));
            else
               Write_Attr_Init_Error ("allow_light_pcs");
            end if;

         when Attribute_Reconnection =>

            if Get_Variable_Type (Attr_Item) /= Integer_Type_Node then
               Write_Attr_Kind_Error ("reconnection", "of reconnection type");
            end if;

            --  Check that it has not already been assigned.

            if Current.Reconnection = No_Reconnection then
               Current.Reconnection :=
                 Convert (Get_Scalar_Value (Attr_Item));
            else
               Write_Attr_Init_Error ("reconnection");
            end if;

         when Attribute_Leader =>

            --  Internal attribute. Don't check anything.

            if Partition /= No_Partition_Id then
               if Main_Partition /= No_Partition_Id then
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
            for B in Current.Task_Pool'Range loop
               begin
                  Get_Name_String
                    (Get_Variable_Name (Get_Component_Value (Comp_Node)));
                  Current.Task_Pool (B) :=
                    Int'Value (Name_Buffer (1 .. Name_Len));
               exception when others =>
                  Write_SLOC (Node_Id (Attribute));
                  Write_Str  ("incorrect integer value");
                  Write_Eol;
                  raise Parsing_Error;
               end;
               Next_Variable_Component (Comp_Node);
            end loop;

         when Attribute_Storage =>
            if Current.Storage_Loc = No_Location_Id then
               First_Variable_Component (Attr_Item, Comp_Node);
               Name := Get_Variable_Name (Get_Component_Value (Comp_Node));
               Next_Variable_Component (Comp_Node);
               Data := Get_Variable_Name (Get_Component_Value (Comp_Node));
               declare
                  LID : Location_Id;
               begin
                  Locations.Increment_Last;
                  LID := Locations.Last;
                  Locations.Table (LID).Major         := Name;
                  Locations.Table (LID).Minor         := Data;
                  Locations.Table (LID).Next_Location := No_Location_Id;
                  Current.Storage_Loc := LID;
               end;

            else
               Write_Attr_Init_Error ("storage");
            end if;

         when Attribute_Protocol =>
            if Current.First_Network_Loc = No_Location_Id then
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
                       (Current.First_Network_Loc,
                        Current.Last_Network_Loc,
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
                             (Current.First_Network_Loc,
                              Current.Last_Network_Loc,
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

         when Attribute_Priority =>
            if Get_Variable_Type (Attr_Item) /= Integer_Type_Node then
               Write_Attr_Kind_Error ("priority", "of priority type");
            end if;

            --  Check that it has not already been assigned.

            if Current.Priority = No_Priority then
               begin
                  Get_Name_String (Get_Variable_Name (Attr_Item));
                  Current.Priority :=
                    Priority_Type'Value (Name_Buffer (1 .. Name_Len));
               exception when others =>
                  Write_SLOC (Node_Id (Attribute));
                  Write_Str  ("incorrect integer value");
                  Write_Eol;
                  raise Parsing_Error;
               end;

            else
               Write_Attr_Init_Error ("priority");
            end if;

         when Attribute_CFilter | Attribute_Unknown =>
            raise Fatal_Error;
      end case;
   end Set_Partition_Attribute;

   ----------------------
   -- Set_Partition_Id --
   ----------------------

   procedure Set_Partition_Id (N : Name_Id; P : Partition_Id) is
      Info : constant Int := Int (P) - Int (Partition_Id'First);

   begin
      Set_Name_Table_Byte (N, Byte (Info));
   end Set_Partition_Id;

   --------------------------
   -- Set_Pragma_Statement --
   --------------------------

   procedure Set_Pragma_Statement
     (Subprogram  : Subprogram_Id)
   is
      Pragma_Kind : Pragma_Type;
      Parameter   : Parameter_Id;
      Method      : Import_Method_Type;
      Value       : Variable_Id;
      Host        : Host_Id;
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
            if Default_First_Boot_Location /= No_Location_Id then
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
                    (Default_First_Boot_Location,
                     Default_Last_Boot_Location,
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
                       (Default_First_Boot_Location,
                        Default_Last_Boot_Location,
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
                             (Default_First_Boot_Location,
                              Default_Last_Boot_Location,
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
            Default_Version_Check := (Get_Scalar_Value (Value) = Int (BTrue));

         when Pragma_Reg_Filter =>
            Value := Get_Parameter_Value (Parameter);
            Default_Registration_Filter := Get_Variable_Name (Value);

         when Pragma_Priority =>
            Value := Get_Parameter_Value (Parameter);
            Default_Priority_Policy := Convert (Get_Scalar_Value (Value));

         when Pragma_Unknown =>
            raise Program_Error;
      end case;
   end Set_Pragma_Statement;

   -----------------
   -- Set_Tasking --
   -----------------

   procedure Set_Tasking (A : ALI_Id; T : Character) is
   begin
      ALIs.Table (A).Tasking := T;
   end Set_Tasking;

   ------------------------
   -- Set_Type_Attribute --
   ------------------------

   procedure Set_Type_Attribute (Pre_Type : Type_Id) is
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
                       (Attribute_Id (Component_Node), Default_Partition_Id);

                  when Pre_Type_Channel   =>
                     Set_Channel_Attribute
                       (Attribute_Id (Component_Node), Default_Channel_Id);

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

      if Default_First_Boot_Location /= No_Location_Id then
         Write_Field (1, "Protocols");
         declare
            LID : Location_Id := Default_First_Boot_Location;
            One : constant Boolean :=
              (Locations.Table (LID).Next_Location = No_Location_Id);
         begin
            if One then
               Write_Name (Locations.Table (LID).Major);
               Write_Str  ("://");
               Write_Name (Locations.Table (LID).Minor);

            else
               while LID /= No_Location_Id loop
                  Write_Eol;
                  Write_Str ("             - ");
                  Write_Name (Locations.Table (LID).Major);
                  Write_Str  ("://");
                  Write_Name (Locations.Table (LID).Minor);
                  LID := Locations.Table (LID).Next_Location;
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
            P : Partition_Id;
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
               F := Channels.Table (C).Filter;
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

   procedure Show_Partition (P : Partition_Id) is
      H : Host_Id;
      U : Conf_Unit_Id;
      I : Unit_Id;
      Current : Partition_Type renames Partitions.Table (P);

   begin
      Write_Str  ("Partition ");
      Write_Name (Current.Name);
      Write_Eol;

      if Present (Current.Main_Subprogram) then
         Write_Field (1, "Main");
         Write_Name  (Current.Main_Subprogram);
         Write_Eol;
      end if;

      H := Partitions.Table (P).Host;
      if H = No_Host_Id then
         H := Partitions.Table (Default_Partition_Id).Host;
      end if;

      if H /= No_Host_Id then
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

      if Present (Current.Executable_Dir) then
         Write_Field (1, "Directory");
         Write_Name  (Current.Executable_Dir);
         Write_Eol;
      end if;

      if Present (Current.Command_Line) then
         Write_Field (1, "Command");
         Write_Name (Current.Command_Line);
         Write_Eol;
      end if;

      if Current.Task_Pool /= No_Task_Pool then
         Write_Field (1, "Task Pool");
         for J in Current.Task_Pool'Range loop
            Write_Int (Current.Task_Pool (J));
            Write_Str (" ");
         end loop;
         Write_Eol;
      end if;

      if Current.Priority /= No_Priority then
         Write_Field (1, "Priority");
         Write_Int (Int (Current.Priority));
         Write_Eol;
      end if;

      if Current.Termination /= No_Termination then
         Write_Field (1, "Termination");
         case Current.Termination is
            when Local_Termination =>
               Write_Str ("local");

            when Global_Termination =>
               Write_Str ("global");

            when Deferred_Termination =>
               Write_Str ("deferred");

            when No_Termination =>
               null;
         end case;
         Write_Eol;
      end if;

      if Current.First_Network_Loc /= No_Location_Id then
         Write_Field  (1, "Protocols");
         Write_Eol;
         declare
            L : Location_Id := Current.First_Network_Loc;
         begin
            while L /= No_Location_Id loop
               Write_Str  ("             - ");
               Write_Name (Locations.Table (L).Major);
               if Present (Locations.Table (L).Minor) then
                  Write_Str ("://");
                  Write_Name (Locations.Table (L).Minor);
               end if;
               Write_Eol;
               L := Locations.Table (L).Next_Location;
            end loop;
         end;
      end if;

      if Current.Storage_Loc /= No_Location_Id then
         Write_Field  (1, "Storages");
         Write_Eol;
         declare
            L : Location_Id := Current.Storage_Loc;
         begin
            while L /= No_Location_Id loop
               Write_Str  ("             - ");
               Write_Name (Locations.Table (L).Major);
               if Present (Locations.Table (L).Minor) then
                  Write_Str ("://");
                  Write_Name (Locations.Table (L).Minor);
               end if;
               Write_Eol;
               L := Locations.Table (L).Next_Location;
            end loop;
         end;
      end if;

      if Current.Light_PCS = BFalse then
         Write_Field (1, "Light PCS");
         Write_Str ("false");
         Write_Eol;
      end if;

      if Partitions.Table (P).First_Unit /= No_Conf_Unit_Id then
         Write_Field (1, "Units");
         Write_Eol;
         U := Partitions.Table (P).First_Unit;
         while U /= No_Conf_Unit_Id loop
            I := Conf_Units.Table (U).My_Unit;
            Write_Str ("             - ");
            Write_Name (Conf_Units.Table (U).Name);
            Write_Str (" (");

            --  Indicate unit categorization

            if Units.Table (I).RCI then
               Write_Str ("rci");

            elsif Units.Table (I).Remote_Types then
               Write_Str ("rt");

            elsif Units.Table (I).Shared_Passive then
               Write_Str ("sp");

            else
               Write_Str ("normal");
            end if;

            --  Indicate if unit is configured automatically by the PCS

            if Conf_Units.Table (U).Name = PCS_Conf_Unit then
               Write_Str (", from PCS");
            end if;

            Write_Line (")");
            U := Conf_Units.Table (U).Next_Unit;
         end loop;
         Write_Eol;
      end if;
   end Show_Partition;

   --------------
   -- To_Build --
   --------------

   function To_Build (U : Conf_Unit_Id) return Boolean is
   begin
      return Partitions.Table (Conf_Units.Table (U).Partition).To_Build;
   end To_Build;

   ------------------------------
   -- Update_Most_Recent_Stamp --
   ------------------------------

   procedure Update_Most_Recent_Stamp (P : Partition_Id; F : File_Name_Type) is
      Most_Recent : File_Name_Type;
   begin
      Most_Recent := Partitions.Table (P).Most_Recent;
      if No (Most_Recent) then
         Partitions.Table (P).Most_Recent := F;
         if Debug_Mode then
            Write_Program_Name;
            Write_Str  (": ");
            Write_Name (Partitions.Table (P).Name);
            Write_Str  ("'s most recent stamp comes from ");
            Write_Name (F);
            Write_Eol;
         end if;

      elsif File_Time_Stamp (F) > File_Time_Stamp (Most_Recent) then
         Partitions.Table (P).Most_Recent := F;
         if Debug_Mode then
            Write_Program_Name;
            Write_Str  (": ");
            Write_Name (Partitions.Table (P).Name);
            Write_Str  (" most recent file updated to ");
            Write_Name (F);
            Write_Eol;
            Write_Stamp_Comparison (F, Most_Recent);
            Write_Eol;
         end if;
      end if;
   end Update_Most_Recent_Stamp;

   -----------------
   -- Write_Field --
   -----------------

   procedure Write_Field
     (Indent : Natural;
      Field  : String;
      Width  : Natural := Min_Field_Width)
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

end XE_Front;
