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

with Osint;            use Osint;
with Output;           use Output;
with Namet;            use Namet;
with Types;
with XE_Scan;          use XE_Scan;
with XE_Utils;         use XE_Utils;
with XE;               use XE;
with Table;

package body XE_Back is

   use type Types.Name_Id;
   use type Types.Unit_Name_Type;
   use type Types.Int;

   subtype Name_Id        is Types.Name_Id;
   subtype Unit_Name_Type is Types.Unit_Name_Type;
   subtype Int            is Types.Int;

   procedure Build_New_Host
     (Subprogram : in Subprogram_Id;
      Host_Entry : out Host_Id);

   procedure Build_New_Variable
     (Variable : in Variable_Id);

   procedure Build_New_Partition
     (Partition : in Variable_Id);
   --  Retrieve ada units and attributes previously parsed in order to
   --  build the partition.

   procedure Build_New_Channel
     (Channel   : in Variable_Id);
   --  Retrieve the two partitions and attributes previously parsed in
   --  order to build the channel.

   function Get_Host
     (Node : Node_Id)
     return Host_Id;
   --  Retrieve Node Mark component.

   procedure Set_Host
     (Node : in Node_Id;
      Host : in Host_Id);
   --  Set Node Mark component to Host.

   procedure Set_Channel_Attribute
     (Attribute : in Attribute_Id;
      Channel   : in CID_Type);

   procedure Set_Partition_Attribute
     (Attribute : in Attribute_Id;
      Partition : in PID_Type);

   procedure Set_Type_Attribute
     (Pre_Type : in Type_Id);

   procedure Set_Pragma_Statement
     (Subprogram : in Subprogram_Id);

   ----------
   -- Back --
   ----------

   procedure Back is
      Node : Node_Id;
      Host : Host_Id;
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

         elsif Is_Subprogram (Node) then
            Build_New_Host (Subprogram_Id (Node), Host);
         end if;
         Next_Configuration_Declaration (Node);
      end loop;

      if Main_Subprogram = No_Main_Subprogram then
         Write_SLOC (Node_Id (Configuration_Node));
         Write_Str ("non-dist. app. main subprogram has not been declared");
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

      Create_Channel (Channel_Name, Channel_ID);

      --  Scan Channel_Name partition pair and Channel_Name attributes.

      First_Variable_Component (Variable_Id (Channel), Component_Node);
      while Component_Node /= Null_Component loop

         --  This is a partition (upper or lower).

         if not Is_Component_An_Attribute (Component_Node) then

            --  Append this partition to the pair.
            Partition_Node := Get_Component_Value (Component_Node);
            Partition_Name := Get_Node_Name (Partition_Node);
            Add_Channel_Partition (Partition_Name, Channel_ID);

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
      Host_Entry : out Host_Id) is
      Host : Host_Id := Null_Host;
      Name : Name_Id;
   begin

      Host := Get_Host (Node_Id (Subprogram));

      if Host = Wrong_Host then
         Hosts.Increment_Last;
         Host := Hosts.Last;
         Name := Get_Node_Name (Node_Id (Subprogram));
         Hosts.Table (Host).Name     := Name;
         Hosts.Table (Host).Static   := False;
         Hosts.Table (Host).Import   := None_Import;
         Hosts.Table (Host).External := Name;
         Set_Host (Node_Id (Subprogram), Host);
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
      Parser_Naming  : Name_Id;
   begin

      --  Create a new entry into Partitions.Table.

      Create_Partition (Partition_Name, Partition_ID);

      --  Scan Partition_Name ada unit list and Partition_Name attributes.

      First_Variable_Component (Variable_Id (Partition), Component_Node);
      while Component_Node /= Null_Component loop

         --  This is a configured ada unit.

         if not Is_Component_An_Attribute (Component_Node) then

            --  Append this unit to the partition list.
            Ada_Unit_Node := Get_Component_Value (Component_Node);
            Ada_Unit_Name := Get_Node_Name (Ada_Unit_Node);
            Add_Conf_Unit (Ada_Unit_Name, Partition_ID);

            --  Is this ada unit a main procedure or THE main program ?
            Parser_Naming := Get_Node_Name (Node_Id (Component_Node));
            if Parser_Naming /= Component_Unit then

               --  Is it at least a main procedure ?
               Partitions.Table (Partition_ID).Main_Subprogram
                 := Ada_Unit_Name;

               --  Is it also a main program ?
               if Parser_Naming = Procedure_Unit then

                  --  Has the main program already been declared ?
                  if Main_Partition /= Null_PID then
                     Write_SLOC (Node_Id (Ada_Unit_Name));
                     Write_Name (Main_Subprogram);
                     Write_Str  (" and ");
                     Write_Name (Ada_Unit_Name);
                     Write_Str  (" are both non-dist. app. main subprograms");
                     Write_Eol;
                     raise Parsing_Error;
                  end if;

                  Main_Partition := Partition_ID;
                  Main_Subprogram := Ada_Unit_Name;

               end if;

            end if;

         --  This information is a partition attribute.

         else
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
      Pre_Type := Convert (Get_Type_Mark (Var_Type));
      case Pre_Type is

         when Pre_Type_Partition =>
            Build_New_Partition (Variable);

         when Pre_Type_Channel   =>
            Build_New_Channel (Variable);

         when others =>
            null;

      end case;
   end Build_New_Variable;

   --------------
   -- Get_Host --
   --------------

   function Get_Host (Node : Node_Id) return Host_Id is
      Info : Int;
   begin
      if Is_Subprogram (Node) then
         Info := Get_Subprogram_Mark (Subprogram_Id (Node));
      elsif Is_Variable (Node) then
         Info := Get_Variable_Mark (Variable_Id (Node));
      else
         raise Parsing_Error;
      end if;
      case Info is
         when Host_First .. Host_Last =>
            return Host_Id (Info);
         when others =>
            return Wrong_Host;
      end case;
   end Get_Host;

   ---------------------------
   -- Set_Channel_Attribute --
   ---------------------------

   procedure Set_Channel_Attribute
     (Attribute : in Attribute_Id;
      Channel   : in CID_Type) is

      --  Could be a variable or a subprogram.
      Attribute_Item : Node_Id;

      Attribute_Kind : Attribute_Type;

   begin

      --  Apply attribute to a channel.

      Attribute_Kind :=
        Convert (Get_Component_Mark (Component_Id (Attribute)));
      Attribute_Item :=
        Get_Component_Value (Component_Id (Attribute));

      --  No attribute was really assigned.

      if Attribute_Item = Null_Node then
         return;
      end if;

      case Attribute_Kind is
         when Attribute_Filter =>

            --  Only strings are allowed here.

            if not Is_Variable (Attribute_Item) or else
              Get_Variable_Type (Variable_Id (Attribute_Item)) /=
              String_Type_Node then
               Write_SLOC (Node_Id (Attribute));
               Write_Name (Channels.Table (Channel).Name);
               Write_Str ("'s filter attribute must be ");
               Write_Str ("a string litteral");
               Write_Eol;
               raise Parsing_Error;
            end if;

            --  Does it apply to all channels ? Therefore, check
            --  that this has not already been done.

            if Channel = Null_CID and then
               Default_Filter = No_Filter_Name then
               Default_Filter := Get_Node_Name (Attribute_Item);

            --  Apply to one channel. Check that it has not already
            --  been done.

            elsif Channel /= Null_CID and then
              Channels.Table (Channel).Filter = No_Filter_Name then
               Channels.Table (Channel).Filter
                 := Get_Node_Name (Attribute_Item);

            --  This operation has already been done !

            else
               Write_SLOC (Attribute_Item);
               if Channel = Null_CID then
                  Write_Str ("type channel");
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

   --------------
   -- Set_Host --
   --------------

   procedure Set_Host
     (Node : in Node_Id;
      Host : in Host_Id) is
   begin
      if Is_Subprogram (Node) then
         Set_Subprogram_Mark (Subprogram_Id (Node), Int (Host));
      elsif Is_Variable (Node) then
         Set_Variable_Mark (Variable_Id (Node), Int (Host));
      else
         raise Parsing_Error;
      end if;
   end Set_Host;

   -----------------------------
   -- Set_Partition_Attribute --
   -----------------------------

   procedure Set_Partition_Attribute
     (Attribute : in Attribute_Id;
      Partition : in PID_Type) is

      --  Could be a variable or a subprogram.
      Attribute_Item : Node_Id;

      Attribute_Kind : Attribute_Type;
      Ada_Unit_Name  : Name_Id;

   begin

      --  If this attribute applies to partition type itself, it may not
      --  have a value. No big deal, we use defaults.

      if Partition = Null_PID and then
        not Has_Component_A_Value (Component_Id (Attribute)) then
         return;
      end if;

      --  Apply attribute to a partition.

      Attribute_Kind :=
        Convert (Get_Component_Mark (Component_Id (Attribute)));
      Attribute_Item :=
        Get_Component_Value (Component_Id (Attribute));

      --  No attribute was really assigned.

      if Attribute_Item = Null_Node then
         return;
      end if;

      case Attribute_Kind is
         when Attribute_Storage_Dir =>

            --  Only strings are allowed here.

            if not Is_Variable (Attribute_Item) or else
              Get_Variable_Type (Variable_Id (Attribute_Item)) /=
              String_Type_Node then
               Write_SLOC (Node_Id (Attribute));
               Write_Name (Partitions.Table (Partition).Name);
               Write_Str ("'s storage_dir attribute must be ");
               Write_Str ("a string litteral");
               Write_Eol;
               raise Parsing_Error;
            end if;

            --  Does it apply to all partitions ? Therefore, check
            --  that this has not already been done.

            if Partition = Null_PID and then
               Default_Storage_Dir = No_Storage_Dir then
               Default_Storage_Dir := Get_Node_Name (Attribute_Item);

            --  Apply to one partition. Check that it has not already
            --  been done.

            elsif Partition /= Null_PID and then
              Partitions.Table (Partition).Storage_Dir = No_Storage_Dir then
               Partitions.Table (Partition).Storage_Dir
                 := Get_Node_Name (Attribute_Item);

            --  This operation has already been done !

            else
               Write_SLOC (Attribute_Item);
               if Partition = Null_PID then
                  Write_Str ("type partition");
               else
                  Write_Name (Partitions.Table (Partition).Name);
               end if;
               Write_Str ("'s storage_dir attribute has been assigned twice");
               Write_Eol;
               raise Parsing_Error;
            end if;

         when Attribute_Host =>

            declare
               Host : Host_Id;
            begin

               if Is_Subprogram (Attribute_Item) then
                  Build_New_Host (Subprogram_Id (Attribute_Item), Host);

               --  Create an entry for this host string.

               elsif Get_Variable_Type (Variable_Id (Attribute_Item)) =
                     String_Type_Node then
                  Hosts.Increment_Last;
                  Host := Hosts.Last;
                  Hosts.Table (Host).Name
                    := Get_Node_Name (Node_Id (Attribute_Item));
                  Hosts.Table (Host).Static := True;

               else
                  Write_SLOC (Node_Id (Attribute));
                  Write_Name (Partitions.Table (Partition).Name);
                  Write_Str  ("'s host attribute must of string type");
                  Write_Eol;
                  raise Parsing_Error;
               end if;

               --  Does it apply to all partitions ? Therefore, check
               --  that this has not already been done.

               if Partition = Null_PID and then Default_Host = Null_Host then
                  Default_Host := Host;

               --  Apply to one partition. Check that it has not already
               --  been done.

               elsif Partition /= Null_PID and then
                 Partitions.Table (Partition).Host = Null_Host then
                  Partitions.Table (Partition).Host := Host;

               else
                  Write_SLOC (Node_Id (Attribute));
                  if Partition = Null_PID then
                     Write_Str ("type partition");
                  else
                     Write_Name (Partitions.Table (Partition).Name);
                  end if;
                  Write_Str ("'s host attribute has been assigned twice");
                  Write_Eol;
                  raise Parsing_Error;
               end if;

            end;

         when Attribute_Main =>

            --  Does it apply to all partitions ? Therefore, check
            --  that this has not already been done.

            if Partition = Null_PID and then
              Default_Main = No_Main_Subprogram then
               Default_Main := Get_Node_Name (Node_Id (Attribute_Item));

            --  Apply to one partition. Check that it has not already
            --  been done.

            elsif Partition /= Null_PID and then
              Partitions.Table (Partition).Main_Subprogram =
              No_Main_Subprogram then
               Partitions.Table (Partition).Main_Subprogram
                 := Get_Node_Name (Node_Id (Attribute_Item));

               --  We are not sure at this point that this unit
               --  has been configured on partition.

               Ada_Unit_Name := Get_Node_Name (Node_Id (Attribute_Item));
               Add_Conf_Unit (Ada_Unit_Name, Partition);

            else
               Write_SLOC (Node_Id (Attribute));
               if Partition = Null_PID then
                  Write_Str ("type partition");
               else
                  Write_Name (Partitions.Table (Partition).Name);
               end if;
               Write_Str ("'s main attribute has been assigned twice");
               Write_Eol;
               raise Parsing_Error;
            end if;

         when Attribute_Command_Line =>

            --  Only strings are allowed.

            if not Is_Variable (Attribute_Item) or else
              Get_Variable_Type (Variable_Id (Attribute_Item)) /=
              String_Type_Node then
               Write_SLOC (Node_Id (Attribute));
               Write_Name (Partitions.Table (Partition).Name);
               Write_Str ("'s command line attribute must be string litteral");
               Write_Eol;
               raise Parsing_Error;
            end if;

            --  Does it apply to all partitions ? Therefore, check
            --  that this has not already been done.

            if Partition = Null_PID and then
              Default_Command_Line = No_Command_Line then
               Default_Command_Line
                 := Get_Node_Name (Node_Id (Attribute_Item));

            --  Apply to one partition. Check that it has not already
            --  been done.

            elsif Partition /= Null_PID and then
              Partitions.Table (Partition).Command_Line = No_Command_Line then
               Partitions.Table (Partition).Command_Line
                 := Get_Node_Name (Node_Id (Attribute_Item));

            else
               Write_SLOC (Node_Id (Attribute));
               if Partition = Null_PID then
                  Write_Str ("type partition");
               else
                  Write_Name (Partitions.Table (Partition).Name);
               end if;
               Write_Str ("'s command_line attribute has been assigned twice");
               Write_Eol;
               raise Parsing_Error;
            end if;

         when Attribute_Termination =>

            --  Only booleans are allowed.

            if not Is_Variable (Attribute_Item) or else
              Get_Variable_Type (Variable_Id (Attribute_Item)) /=
              Integer_Type_Node then
               Write_SLOC (Node_Id (Attribute));
               Write_Name (Partitions.Table (Partition).Name);
               Write_Str ("'s termination attribute must be ");
               Write_Str ("of termination type");
               Write_Eol;
               raise Parsing_Error;
            end if;

            --  Does it apply to all partitions ? Therefore, check
            --  that this has not already been done.

            if Partition = Null_PID and then
              Default_Termination = Unknown_Termination then
               Default_Termination :=
                 Termination_Type
                 (Get_Variable_Mark (Variable_Id (Attribute_Item)));

            --  Apply to one partition. Check that it has not already
            --  been done.

            elsif Partition /= Null_PID and then
              Partitions.Table (Partition).Termination = Unknown_Termination
            then
               Partitions.Table (Partition).Termination :=
                 Termination_Type
                 (Get_Variable_Mark (Variable_Id (Attribute_Item)));

            else
               Write_SLOC (Node_Id (Attribute));
               if Partition = Null_PID then
                  Write_Str ("type partition");
               else
                  Write_Name (Partitions.Table (Partition).Name);
               end if;
               Write_Str ("'s termination attribute has been assigned twice");
               Write_Eol;
               raise Parsing_Error;
            end if;

         when others =>
            raise Fatal_Error;

      end case;

   end Set_Partition_Attribute;

   --------------------------
   -- Set_Pragma_Statement --
   --------------------------

   procedure Set_Pragma_Statement
     (Subprogram  : in Subprogram_Id) is

      Pragma_Kind : Pragma_Type;
      Parameter   : Parameter_Id;
      Method      : Import_Method_Type;
      Value       : Variable_Id;
      Host        : Host_Id;

   begin

      --  Apply pragma statement.

      Pragma_Kind := Convert (Get_Subprogram_Mark (Subprogram));
      First_Subprogram_Parameter (Subprogram, Parameter);

      case Pragma_Kind is

         when Pragma_Import =>
            Value := Get_Variable_Value (Variable_Id (Parameter));
            Method := Convert (Get_Variable_Mark (Value));
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
            Starter_Method := Convert (Get_Variable_Mark (Value));

         when Pragma_Boot_Server =>
            Value := Get_Variable_Value (Variable_Id (Parameter));
            Protocol_Name := Get_Node_Name (Node_Id (Value));
            Next_Subprogram_Parameter (Parameter);
            Value := Get_Variable_Value (Variable_Id (Parameter));
            Protocol_Data := Get_Node_Name (Node_Id (Value));

         when Pragma_Version =>
            Value := Get_Variable_Value (Variable_Id (Parameter));
            Version_Checks := Convert (Get_Variable_Mark (Value));

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
   begin

      --  Some attribute apply to type ... in our case, Partition is
      --  the only type concerned.

      if Pre_Type /= Partition_Type_Node then
         return;
      end if;

      First_Type_Component (Pre_Type, Component_Node);
      while Component_Node /= Null_Component loop
         if Is_Component_An_Attribute (Component_Node) then
            Set_Partition_Attribute (Attribute_Id (Component_Node), Null_PID);
         end if;
         Next_Type_Component (Component_Node);
      end loop;

   end Set_Type_Attribute;

end XE_Back;
