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
--                 GLADE is maintained by ACT Europe.                       --
--            (email:distribution@act-europe.gnat.com).                     --
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

   procedure Set_Host
     (Node : in Node_Id;
      Host : in Host_Id);
   --  Set Node Mark component to Host.

   function Get_Host
     (Node : Node_Id)
     return Host_Id;
   --  Retrieve Node Mark component.

   procedure Build_New_Partition
     (Partition : Node_Id);
   --  Retrieve ada units and attributes previously parsed in order to
   --  build the partition.

   procedure Build_New_Host
     (Subprogram : in Subprogram_Id;
      Host_Entry : out Host_Id);

   procedure Set_Partition_Attribute
     (Attribute : Attribute_Id;
      Partition : PID_Type);
   procedure Set_Type_Attribute
     (Pre_Type : Type_Id);
   procedure Set_Pragma_Statement
     (Subprogram : Subprogram_Id);

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

   -------------------------
   -- Build_New_Partition --
   -------------------------

   procedure Build_New_Partition (Partition : Node_Id) is
      Partition_Name : Name_Id := Get_Node_Name (Partition);
      Partition_Type : Type_Id := Get_Variable_Type (Variable_Id (Partition));
      Ada_Unit_Name  : Name_Id;
      Ada_Unit_Node  : Node_Id;
      Component_Node : Component_Id;
      Partition_PID  : PID_Type;
      Parser_Naming  : Name_Id;
   begin

      --  Create a new entry into Partitions.Table.

      Create_Partition (Partition_Name, Partition_PID);

      --  Scan Partition_Name ada unit list and Partition_Name attributes.

      First_Variable_Component (Variable_Id (Partition), Component_Node);
      while Component_Node /= Null_Component loop

         --  This is a configured ada unit.

         if not Is_Component_An_Attribute (Component_Node) then

            --  Append this unit to the partition list.
            Ada_Unit_Node := Get_Component_Value (Component_Node);
            Ada_Unit_Name := Get_Node_Name (Ada_Unit_Node);
            Add_Conf_Unit (Ada_Unit_Name, Partition_PID);

            --  Is this ada unit a main procedure or THE main program ?
            Parser_Naming := Get_Node_Name (Node_Id (Component_Node));
            if Parser_Naming /= Conf_Ada_Unit then

               --  Is it at least a main procedure ?
               Partitions.Table (Partition_PID).Main_Subprogram
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

                  Main_Partition := Partition_PID;
                  Main_Subprogram := Ada_Unit_Name;

               end if;

            end if;

         --  This information is a partition attribute.

         else
            Set_Partition_Attribute
              (Attribute_Id (Component_Node), Partition_PID);
         end if;

         Next_Variable_Component (Component_Node);

      end loop;

   end Build_New_Partition;

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

   ------------------------
   -- Set_Type_Attribute --
   ------------------------

   procedure Set_Type_Attribute
     (Pre_Type : Type_Id) is
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

   -----------------------------
   -- Set_Partition_Attribute --
   -----------------------------

   procedure Set_Partition_Attribute
     (Attribute : Attribute_Id;
      Partition : PID_Type) is
      Attribute_Kind : Attribute_Type;
      Attribute_Item : Variable_Id;
      Ada_Unit_Name  : Name_Id;
   begin

      --  Apply attribute to a partition.

      Attribute_Kind :=
        Convert (Get_Component_Mark (Component_Id (Attribute)));
      Attribute_Item :=
        Variable_Id (Get_Component_Value (Component_Id (Attribute)));

      --  No attribute was really assigned.

      if Attribute_Item = Null_Variable then
         return;
      end if;

      case Attribute_Kind is
         when Attribute_Storage_Dir =>

            --  Only strings are allowed here.

            if Get_Variable_Type (Attribute_Item) /= String_Type_Node then
               Write_SLOC (Node_Id (Attribute_Item));
               Write_Name (Partitions.Table (Partition).Name);
               Write_Str  ("'s attribute must be of type string");
               Write_Eol;
               raise Parsing_Error;
            end if;

            --  Does it apply to all partitions ? Therefore, check
            --  that this has not already been done.

            if Partition = Null_PID and then
               Default_Storage_Dir = No_Storage_Dir then
               Default_Storage_Dir := Get_Node_Name (Node_Id (Attribute_Item));

            --  Apply to one partition. Check that it has not already
            --  been done.

            elsif Partition /= Null_PID and then
              Partitions.Table (Partition).Storage_Dir = No_Storage_Dir then
               Partitions.Table (Partition).Storage_Dir
                 := Get_Node_Name (Node_Id (Attribute_Item));

            --  This operation has already been done !

            else
               Write_SLOC (Node_Id (Attribute_Item));
               if Partition = Null_PID then
                  Write_Str ("type partition");
               else
                  Write_Name (Partitions.Table (Partition).Name);
               end if;
               Write_Str ("'s command_line attribute has been assigned twice");
               Write_Eol;
               raise Parsing_Error;
            end if;

         when Attribute_Host =>

            declare
               Host : Host_Id;
            begin

               if Is_Subprogram (Node_Id (Attribute_Item)) then
                  Build_New_Host (Subprogram_Id (Attribute_Item), Host);

               --  Create an entry for this host string.

               elsif Get_Variable_Type (Attribute_Item) = String_Type_Node then
                  Hosts.Increment_Last;
                  Host := Hosts.Last;
                  Hosts.Table (Host).Name
                    := Get_Node_Name (Node_Id (Attribute_Item));
                  Hosts.Table (Host).Static := True;

               else
                  Write_SLOC (Node_Id (Attribute_Item));
                  Write_Name (Partitions.Table (Partition).Name);
                  Write_Str  ("'s host attribute is not a string value");
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
                  Write_SLOC (Node_Id (Attribute_Item));
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
               Write_SLOC (Node_Id (Attribute_Item));
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

            if Get_Variable_Type (Attribute_Item) /= String_Type_Node then
               Write_SLOC (Node_Id (Attribute_Item));
               Write_Name (Partitions.Table (Partition).Name);
               Write_Str  ("'s command line attribute must be of string type");
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
               Write_SLOC (Node_Id (Attribute_Item));
               if Partition = Null_PID then
                  Write_Str ("type partition");
               else
                  Write_Name (Partitions.Table (Partition).Name);
               end if;
               Write_Str ("'s command_line attribute has been assigned twice");
               Write_Eol;
               raise Parsing_Error;
            end if;

         when Attribute_Permanent =>

            --  Only booleans are allowed.

            if Get_Variable_Type (Attribute_Item) /= Boolean_Type_Node then
               Write_SLOC (Node_Id (Attribute_Item));
               Write_Name (Partitions.Table (Partition).Name);
               Write_Str  ("'s permanent attribute must be of boolean type");
               Write_Eol;
               raise Parsing_Error;
            end if;

            --  Does it apply to all partitions ? Therefore, check
            --  that this has not already been done.

            if Partition = Null_PID and then
              Default_Permanent = Unknown then
               if Convert (Get_Variable_Mark (Attribute_Item)) = True then
                  Default_Permanent := Yes;
               else
                  Default_Permanent := No;
               end if;

            --  Apply to one partition. Check that it has not already
            --  been done.

            elsif Partition /= Null_PID and then
              Partitions.Table (Partition).Permanent = Unknown then
               if Convert (Get_Variable_Mark (Attribute_Item)) = True then
                  Partitions.Table (Partition).Permanent := Yes;
               else
                  Partitions.Table (Partition).Permanent := No;
               end if;

            else
               Write_SLOC (Node_Id (Attribute_Item));
               if Partition = Null_PID then
                  Write_Str ("type partition");
               else
                  Write_Name (Partitions.Table (Partition).Name);
               end if;
               Write_Str ("'s command_line attribute has been assigned twice");
               Write_Eol;
               raise Parsing_Error;
            end if;

         when Attribute_Unknown =>
            null;

      end case;

   end Set_Partition_Attribute;

   --------------------------
   -- Set_Pragma_Statement --
   --------------------------

   procedure Set_Pragma_Statement
     (Subprogram  : Subprogram_Id) is
      Pragma_Kind : Pragma_Type;
      Parameter : Parameter_Id;
      Method    : Import_Method_Type;
      Value     : Variable_Id;
      Host      : Host_Id;
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

   ----------
   -- Back --
   ----------

   procedure Back is
      Node : Node_Id;
      Host : Host_Id;
   begin

      First_Configuration_Declaration (Configuration_Node, Node);
      while Node /= Null_Node loop
         if Is_Variable (Node) and then
            Get_Variable_Type (Variable_Id (Node)) = Partition_Type_Node then
            Build_New_Partition (Node);

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

end XE_Back;






