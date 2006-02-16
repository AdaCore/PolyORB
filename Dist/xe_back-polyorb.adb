------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--                      X E _ B A C K . P O L Y O R B                       --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 1995-2005 Free Software Foundation, Inc.           --
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
--                   GLADE  is maintained by AdaCore                        --
--                      (email: sales@adacore.com)                          --
--                                                                          --
------------------------------------------------------------------------------

with GNAT.Expect; use GNAT.Expect;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with XE;          use XE;
with XE_Flags;    use XE_Flags;
with XE_Front;    use XE_Front;
with XE_IO;       use XE_IO;
with XE_Names;    use XE_Names;
with XE_Utils;    use XE_Utils;

package body XE_Back.PolyORB is

   type PolyORB_Backend is new Backend with null record;

   procedure Set_PCS_Dist_Flags (Self : access PolyORB_Backend);
   procedure Initialize (Self : access PolyORB_Backend);
   procedure Run_Backend (Self : access PolyORB_Backend);

   Elaboration_File : File_Name_Type;

   type RU_Id is
     (RU_PolyORB,
      RU_PolyORB_Binding_Data,
      RU_PolyORB_Binding_Data_IIOP,
      RU_PolyORB_Initialization,
      RU_PolyORB_ORB_Controller,
      RU_PolyORB_ORB_Controller_Workers,
      RU_PolyORB_ORB_No_Tasking,
      RU_PolyORB_ORB,
      RU_PolyORB_ORB_Thread_Pool,
      RU_PolyORB_Parameters,
      RU_PolyORB_Parameters_Partition_Conf,
      RU_PolyORB_POA_Config,
      RU_PolyORB_POA_Config_RACWs,
      RU_PolyORB_Setup,
      RU_PolyORB_Setup_Access_Points,
      RU_PolyORB_Setup_Access_Points_IIOP,
      RU_PolyORB_Setup_Base,
      RU_PolyORB_Setup_IIOP,
      RU_PolyORB_Setup_OA,
      RU_PolyORB_Setup_OA_Basic_POA,
      RU_PolyORB_Setup_Tasking,
      RU_PolyORB_Setup_Tasking_Full_Tasking,
      RU_PolyORB_Setup_Tasking_No_Tasking,
      RU_System_Partition_Interface,
      RU_System_RPC,
      RU_System_RPC_Server,
      RU_Tasking);

   RU : array (RU_Id) of Unit_Name_Type;

   type RE_Id is
     (RE_Initialize_World,
      RE_Run,
      RE_The_ORB);

   RE : array (RE_Id) of Unit_Name_Type;

   RE_Unit_Table : constant array (RE_Id) of RU_Id :=
     (RE_Initialize_World  => RU_PolyORB_Initialization,
      RE_Run               => RU_PolyORB_ORB,
      RE_The_ORB           => RU_PolyORB_Setup);

   ---------------------
   -- Parameter types --
   ---------------------

   type PS_Id is
     (PS_Tasking);

   PS : array (PS_Id) of Unit_Name_Type;

   type PE_Id is
     (PE_Min_Spare_Threads,
      PE_Max_Spare_Threads,
      PE_Max_Threads);

   PE : array (PE_Id) of Unit_Name_Type;

   PE_Section_Table : constant array (PE_Id) of PS_Id :=
     (PE_Min_Spare_Threads => PS_Tasking,
      PE_Max_Spare_Threads => PS_Tasking,
      PE_Max_Threads       => PS_Tasking);

   -----------------------------
   -- Parameter table entries --
   -----------------------------

   type Parameter_Entry is record
      Var : Name_Id;
      Val : Name_Id;
   end record;

   Table : array (0 .. 31) of Parameter_Entry;
   Last  : Integer := -1;

   ---------------------------
   -- Generation Procedures --
   ---------------------------

   procedure Generate_Elaboration_File (P : Partition_Id);
   --  Create the elaboration unit for the given partition. This unit
   --  overloads the default PCS settings.

   procedure Generate_Executable_File (P : Partition_Id);
   --  Compile main partition file and elaboration file.
   --  Bind and link partition to create executable.

   procedure Generate_Partition_Main_File (P : Partition_Id);
   --  Create a procedure which "withes" all the RCI or SP receivers
   --  of the partition and insert the main procedure if needed.

   procedure Generate_Stub (A : ALI_Id);
   --  Create stub and skel for a RCI or SP unit.

   function Strip (S : String) return Unit_Name_Type;
   --  Return the prefix and a possible suffix from S

   procedure Set_Conf (Var : PE_Id; Val : Name_Id);

   -------------------------------
   -- Generate_Elaboration_File --
   -------------------------------

   procedure Generate_Elaboration_File (P : Partition_Id) is

      Filename     : File_Name_Type;
      File         : File_Descriptor;
      Current      : Partition_Type renames Partitions.Table (P);

   begin
      Filename := Elaboration_File & ADB_Suffix_Id;
      Filename := Dir (Current.Partition_Dir, Filename);
      Create_File (File, Filename);
      Set_Output  (File);

      if Current.Task_Pool /= No_Task_Pool then
         Set_Nat_To_Name_Buffer (Current.Task_Pool (1));
         Set_Conf (PE_Min_Spare_Threads, Name_Find);
         Set_Nat_To_Name_Buffer (Current.Task_Pool (2));
         Set_Conf (PE_Max_Spare_Threads, Name_Find);
         Set_Nat_To_Name_Buffer (Current.Task_Pool (3));
         Set_Conf (PE_Max_Threads, Name_Find);
      end if;

      Write_Line ("pragma Warnings (Off);");
      Write_Str  ("package body ");
      Write_Name (RU (RU_PolyORB_Parameters_Partition_Conf));
      Write_Line (" is");

      Increment_Indentation;

      Write_Indentation;
      Write_Line ("function Lookup (S : String) return Integer is");
      Write_Indentation;
      Write_Line ("begin");

      Increment_Indentation;
      Write_Indentation;
      Write_Line ("for I in 0 .. Last loop");

      Increment_Indentation;
      Write_Indentation;
      Write_Line ("if Table (I).Var.all = S then");

      Increment_Indentation;
      Write_Indentation;
      Write_Line ("return I;");

      Decrement_Indentation;
      Write_Indentation;
      Write_Line ("end if;");

      Decrement_Indentation;
      Write_Indentation;
      Write_Line ("end loop;");
      Write_Indentation;
      Write_Line ("return -1;");

      Decrement_Indentation;
      Write_Indentation;
      Write_Str ("end Lookup;");
      Write_Eol;

      Write_Indentation;
      Write_Line ("procedure Initialize is");
      Write_Indentation;
      Write_Line ("begin");
      Increment_Indentation;
      Write_Indentation;
      Write_Line ("Last := -1;");

      for P in Table'First .. Last loop
         Write_Indentation;
         Write_Line ("Last := Last + 1;");
         Write_Indentation;
         Write_Str  ("Table (Last).Var := new String'(""");
         Write_Name (Table (P).Var);
         Write_Line (""");");
         Write_Indentation;
         Write_Str  ("Table (Last).Val := new String'(""");
         Write_Name (Table (P).Val);
         Write_Line (""");");
      end loop;

      Decrement_Indentation;
      Write_Indentation;
      Write_Line ("end Initialize;");

      Decrement_Indentation;
      Write_Str  ("end ");
      Write_Name (RU (RU_PolyORB_Parameters_Partition_Conf));
      Write_Line (";");

      Close (File);
      Set_Standard_Output;
   end Generate_Elaboration_File;

   ------------------------------
   -- Generate_Executable_File --
   ------------------------------

   procedure Generate_Executable_File (P : Partition_Id) is
      Current    : Partition_Type renames Partitions.Table (P);
      Executable : File_Name_Type;
      Directory  : Directory_Name_Type;
      I_Part_Dir : String_Access;
      Comp_Args  : String_List (1 .. 9);
      Make_Args  : String_List (1 .. 8);
      Sfile      : File_Name_Type;
      Prj_Fname  : File_Name_Type;
      Length     : Natural;

   begin
      Executable := Current.Executable_File;
      Directory  := Current.Partition_Dir;

      Name_Len := 2;
      Name_Buffer (1) := '-';
      Name_Buffer (2) := 'I';
      Get_Name_String_And_Append (Directory);
      I_Part_Dir := new String'(Name_Buffer (1 .. Name_Len));

      --  Give the priority to partition and stub directory against
      --  current directory.

      Comp_Args (1) := E_Current_Dir;
      Comp_Args (2) := I_Part_Dir;
      Comp_Args (3) := I_Stub_Dir;
      Comp_Args (4) := I_Current_Dir;

      --  If there is no project file, then save ali and object files
      --  in partition directory.

      if Project_File_Name = null then
         Comp_Args (5) := Object_Dir_Flag;
         Comp_Args (6) := new String'(Get_Name_String (Directory));

      else
         Comp_Args (5) := Project_File_Flag;
         Prj_Fname     := Dir (Directory, Part_Prj_File_Name);
         Comp_Args (6) := new String'(Get_Name_String (Prj_Fname));
      end if;

      Length := 6;

      --  We already checked the consistency of all the partition
      --  units. In case of an inconsistency of exception mode, we may
      --  have to rebuild some parts of garlic (units configured just
      --  for this partition). Note that some parts of Garlic may have
      --  been already recompiled when the monolithic application was
      --  initially build. Some bodies may be missing as they are
      --  assigned to partitions we do not want to build. So compile
      --  silently and do not exit on errors (keep going).

      if Project_File_Name = null then
         Comp_Args (7) := Compile_Only_Flag;
         Comp_Args (8) := Keep_Going_Flag;
         Comp_Args (9) := Readonly_Flag;
         Length := 9;
      end if;

      Sfile := Elaboration_File & ADB_Suffix_Id;
      if Project_File_Name = null then
         Sfile := Dir (Directory, Sfile);
      end if;
      Compile (Sfile, Comp_Args (1 .. Length));

      Sfile := Partition_Main_File & ADB_Suffix_Id;
      if Project_File_Name = null then
         Sfile := Dir (Directory, Sfile);
      end if;
      Compile (Sfile, Comp_Args (1 .. Length));

      Free (Comp_Args (6));

      --  Now we just want to bind and link as the ALI files are now
      --  consistent.

      Make_Args (1) := E_Current_Dir;
      Make_Args (2) := I_Part_Dir;
      Make_Args (3) := I_Stub_Dir;
      Make_Args (4) := I_Current_Dir;
      Make_Args (5) := Bind_Only_Flag;
      Make_Args (6) := Link_Only_Flag;

      if Project_File_Name = null then
         Make_Args (7) := Output_Flag;
         Make_Args (8) := new String'(Get_Name_String (Executable));

      else
         Make_Args (7) := Project_File_Flag;
         Prj_Fname := Dir (Directory, Part_Prj_File_Name);
         Make_Args (8) := new String'(Get_Name_String (Prj_Fname));
      end if;

      Build (Sfile, Make_Args, Fatal => True, Silent => False);

      Free (Make_Args (2));
      Free (Make_Args (8));
   end Generate_Executable_File;

   ----------------------------------
   -- Generate_Partition_Main_File --
   ----------------------------------

   procedure Generate_Partition_Main_File (P : Partition_Id) is
      Filename  : File_Name_Type;
      File      : File_Descriptor;
      Current   : Partition_Type renames Partitions.Table (P);
      Conf_Unit : Conf_Unit_Id;

   begin
      Filename := Partition_Main_File & ADB_Suffix_Id;
      Filename := Dir (Current.Partition_Dir, Filename);
      Create_File (File, Filename);
      Set_Output  (File);
      Write_Line  ("pragma Warnings (Off);");

      Write_With_Clause (RU (RU_PolyORB_ORB), False, True);
      Write_With_Clause (RU (RU_PolyORB_ORB_Controller_Workers), False, True);
      Write_With_Clause (RU (RU_PolyORB_Initialization), False, True);
      Write_With_Clause (RU (RU_PolyORB_Setup_Base), False, True);
      Write_With_Clause (RU (RU_PolyORB_Setup_IIOP), False, True);
      Write_With_Clause (RU (RU_PolyORB_Setup_OA_Basic_POA), False, True);

      if Current.Tasking = 'N' then
         Write_With_Clause (RU (RU_PolyORB_Setup_Tasking_No_Tasking));
         Write_With_Clause (RU (RU_PolyORB_ORB_No_Tasking));
         Write_With_Clause (RU (RU_PolyORB_Binding_Data_IIOP));

      else
         Write_With_Clause (RU (RU_PolyORB_Setup_Tasking_Full_Tasking));

         if Current.Tasking = 'T' then
            Write_With_Clause (RU (RU_PolyORB_ORB_No_Tasking));
            Write_With_Clause (RU (RU_PolyORB_Binding_Data_IIOP));

         else
            Write_With_Clause (RU (RU_PolyORB_ORB_Thread_Pool));
            Write_With_Clause (RU (RU_PolyORB_Setup_Access_Points_IIOP));
            Write_With_Clause (RU (RU_PolyORB_POA_Config_RACWs));
         end if;
      end if;

      --  Assign RCI or SP skels on the partition

      Conf_Unit := Current.First_Unit;
      while Conf_Unit /= No_Conf_Unit_Id loop
         Write_With_Clause (Conf_Units.Table (Conf_Unit).Name);
         Conf_Unit := Conf_Units.Table (Conf_Unit).Next_Unit;
      end loop;

      --  Assign the RCI or SP stubs to compare version with skels

      for J in Current.First_Stub .. Current.Last_Stub loop
         Write_With_Clause (Stubs.Table (J));
      end loop;

      --  Add termination package and locking mechanisms if needed

      Write_Str  ("procedure ");
      Write_Name (Partition_Main_Name);
      Write_Line (" is");
      Write_Line ("begin");

      Increment_Indentation;
      Write_Call (RU (RE_Unit_Table (RE_Initialize_World))
                    and RE (RE_Initialize_World));

      --  Invoke main subprogram when there is one

      if Present (Current.Main_Subprogram) then
         Write_Call (Current.Main_Subprogram);

      --  XXX We launch ORB.Run although this is only required for a
      --  non-tasking server. Note that this can be considered as
      --  incorrect since the env task becomes indirectly part of the
      --  task pool.

      else
         Write_Call
           (RU (RE_Unit_Table (RE_Run)) and RE (RE_Run),
            RU (RE_Unit_Table (RE_The_ORB)) and RE (RE_The_ORB),
            "May_Poll => True");
      end if;

      Decrement_Indentation;
      Write_Str  ("end ");
      Write_Name (Partition_Main_Name);
      Write_Line (";");

      Close (File);
      Set_Standard_Output;
   end Generate_Partition_Main_File;

   -------------------
   -- Generate_Stub --
   -------------------

   procedure Generate_Stub (A : ALI_Id) is
      Obsolete       : Boolean;
      Full_Unit_File : File_Name_Type;
      Full_ALI_File  : File_Name_Type;
      Stub_Object    : File_Name_Type;
      Stub_ALI       : File_Name_Type;
      Unit           : Unit_Id := ALIs.Table (A).Last_Unit;
      Arguments      : Argument_List (1 .. 3);
      Part_Prj_Fname : File_Name_Type := No_File_Name;

   begin
      if Units.Table (Unit).Shared_Passive then
         Unit := ALIs.Table (A).First_Unit;
      end if;

      Full_Unit_File := Units.Table (Unit).Sfile;
      Full_ALI_File  := ALIs.Table (A).Afile;
      Stub_ALI       := To_Afile (Strip_Directory (Full_Unit_File));
      Stub_ALI       := Dir (Stub_Dir_Name, Stub_ALI);
      Stub_Object    := To_Ofile (Stub_ALI);

      --  Do we need to regenerate the caller stub and its ali

      Obsolete := False;
      if not Is_Regular_File (Stub_Object) then
         if Verbose_Mode then
            Write_Missing_File (Stub_Object);
         end if;
         Obsolete := True;
      end if;

      if not Obsolete
        and then not Is_Regular_File (Stub_ALI)
      then
         if Verbose_Mode then
            Write_Missing_File (Stub_ALI);
         end if;
         Obsolete := True;
      end if;

      if not Obsolete
        and then File_Time_Stamp (Full_ALI_File) > File_Time_Stamp (Stub_ALI)
      then
         if Verbose_Mode then
            Write_Stamp_Comparison (Full_ALI_File, Stub_ALI);
         end if;
         Obsolete := True;
      end if;

      if Obsolete then
         if not Quiet_Mode then
            Message
              ("building", ALIs.Table (A).Uname,
               "caller stubs from", Normalize_CWD (Full_Unit_File));
         end if;

         Arguments (1) := Stub_Flag;

         if Project_File_Name = null then
            Arguments (2) := Object_Dir_Flag;
            Arguments (3) := Stub_Dir;

         else
            Arguments (2)  := Project_File_Flag;
            Part_Prj_Fname := Dir (Stub_Dir, Part_Prj_File_Name);
            Get_Name_String (Part_Prj_Fname);
            Arguments (3)  := new String'(Name_Buffer (1 .. Name_Len));
         end if;

         Compile (Full_Unit_File, Arguments, Fatal => False, Silent => True);

         if Present (Part_Prj_Fname) then
            Free (Arguments (3));
         end if;

      elsif not Quiet_Mode then
         Message ("  ", ALIs.Table (A).Uname, "caller stubs is up to date");
      end if;
   end Generate_Stub;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Self : access PolyORB_Backend) is
      pragma Unreferenced (Self);

      Pos : Integer;
      Len : Natural;

   begin
      XE_Back.Initialize;

      --  This RCI unit has to be automatically configured on the main
      --  partition.

      PCS_Conf_Unit    := Id ("polyorb.dsa_p.partitions");
      Elaboration_File := Id ("polyorb-parameters-partition_conf");

      Register_Casing_Rule ("ORB");

      for U in RU_Id'First .. RU_Id'Last loop
         RU (U) := Strip (RU_Id'Image (U));

         --  Allow to get the litteral value back from the name id. As
         --  the default value of info is zero, and as the first pos
         --  of an enumeration type is also zero, increment the pos.

         Set_Name_Table_Info (RU (U), RU_Id'Pos (U) + 1);

         --  Look for parent units

         Pos := 0;
         Get_Name_String (RU (U));
         Len := Name_Len;
         while Name_Len > 0 loop
            if Name_Buffer (Name_Len) = '_' then
               Name_Len := Name_Len - 1;
               Pos := Integer (Get_Name_Table_Info (Name_Find));
               exit when Pos > 0;

            else
               Name_Len := Name_Len - 1;
            end if;
         end loop;

         --  When there is a parent, remove parent unit name from
         --  unit name to get real identifier.

         if Pos > 0 then
            Set_Str_To_Name_Buffer (Name_Buffer (Name_Len + 2 .. Len));
            RU (U) := RU (RU_Id'Val (Pos - 1)) and Name_Find;

         else
            Set_Str_To_Name_Buffer (Name_Buffer (1 .. Len));
            RU (U) := Name_Find;
         end if;

         if Debug_Mode then
            Message (U'Img & " = " & Get_Name_String (RU (U)));
         end if;
      end loop;

      for E in RE_Id loop
         RE (E) := Strip (RE_Id'Image (E));
      end loop;

      for S in PS_Id loop
         PS (S) := Strip (PS_Id'Image (S));
      end loop;

      for E in PE_Id loop
         PE (E) := Strip (PE_Id'Image (E));
      end loop;
   end Initialize;

   -----------------
   -- Run_Backend --
   -----------------

   procedure Run_Backend (Self : access PolyORB_Backend) is
      pragma Unreferenced (Self);
      Afile   : File_Name_Type;
      PID     : Partition_Id;
      Unit    : Unit_Id;
      Uname   : Unit_Name_Type;
      Current : Partition_Type;

   begin
      if Project_File_Name /= null then
         Generate_Partition_Project_File (Stub_Dir_Name);
      end if;

      for J in Partitions.First + 1 .. Partitions.Last loop
         Current := Partitions.Table (J);
         if Current.To_Build and then Current.Passive /= BTrue then

            --  Create directories in which resp skels, main partition
            --  unit, elaboration unit and executables are stored

            Create_Dir (Current.Partition_Dir);
            Create_Dir (Current.Executable_Dir);

            if Present (Current.Executable_Dir) then
               Get_Name_String (Current.Executable_Dir);
               Set_Str_To_Name_Buffer
                 (Normalize_Pathname (Name_Buffer (1 .. Name_Len)));
               Partitions.Table (J).Executable_Dir := Name_Find;
            end if;

            Get_Name_String (Current.Executable_File);
            Set_Str_To_Name_Buffer
              (Normalize_Pathname (Name_Buffer (1 .. Name_Len)));
            Current.Executable_File := Name_Find;

            if Project_File_Name /= null then
               Generate_Partition_Project_File (Current.Partition_Dir, J);
            end if;

            for K in ALIs.First .. ALIs.Last loop
               Afile := ALIs.Table (K).Afile;
               Unit  := ALIs.Table (K).Last_Unit;
               Uname := Units.Table (Unit).Uname;

               --  Remove possible copies of unit object

               if (not Units.Table (Unit).RCI
                   and then not Units.Table (Unit).Shared_Passive)
                 or else Get_Partition_Id (Uname) /= J
               then
                  Afile := Strip_Directory (Afile);
                  Afile := Dir (Current.Partition_Dir, Afile);
                  Delete_File (Afile);
                  Delete_File (To_Ofile (Afile));
               end if;
            end loop;
         end if;
      end loop;

      for J in ALIs.First .. ALIs.Last loop
         Unit := ALIs.Table (J).Last_Unit;

         --  Create stub files. Create skel files as well when we have
         --  to build the partition on which this unit is mapped.

         if not Units.Table (Unit).Is_Generic
           and then (Units.Table (Unit).RCI
                     or else Units.Table (Unit).Shared_Passive)
         then
            Uname := Name (Units.Table (Unit).Uname);
            PID   := Get_Partition_Id (Uname);

            if PID /= No_Partition_Id
              and then Partitions.Table (PID).To_Build
              and then Partitions.Table (PID).Passive /= BTrue
            then
               if Debug_Mode then
                  Message ("create caller and receiver stubs for", Uname);
               end if;

               Generate_Stub (J);
               Generate_Skel (J, PID);

            else
               if Debug_Mode then
                  Message ("create caller stubs for", Uname);
               end if;

               Generate_Stub (J);
            end if;

         end if;
      end loop;

      --  Create partition files and fill partition directories

      for J in Partitions.First + 1 .. Partitions.Last loop
         if Partitions.Table (J).To_Build then
            Current := Partitions.Table (J);

            if Current.To_Build and then Current.Passive /= BTrue then
               if Rebuild_Partition (J) then
                  if not Quiet_Mode then
                     Message ("building partition", Current.Name);
                  end if;

                  Generate_Partition_Main_File (J);
                  Generate_Elaboration_File (J);
                  Generate_Executable_File (J);
                  Generate_Stamp_File (J);
               end if;

            elsif Verbose_Mode then
               Message ("no need to expand", Current.Name);
            end if;
         end if;
      end loop;

      Generate_Starter_File;
   end Run_Backend;

   --------------
   -- Set_Conf --
   --------------

   procedure Set_Conf (Var : PE_Id; Val : Name_Id) is
   begin
      Last := Last + 1;
      Name_Len := 0;
      Add_Str_To_Name_Buffer ("[");
      Get_Name_String_And_Append (PS (PE_Section_Table (Var)));
      Add_Str_To_Name_Buffer ("]");
      Get_Name_String_And_Append (PE (Var));
      To_Lower (Name_Buffer (1 .. Name_Len));
      Table (Last).Var := Name_Find;
      Table (Last).Val := Val;
   end Set_Conf;

   ------------------------
   -- Set_PCS_Dist_Flags --
   ------------------------

   procedure Set_PCS_Dist_Flags (Self : access PolyORB_Backend) is
      pragma Unreferenced (Self);
      Status         : aliased Integer;
   begin
      declare
         PolyORB_Config : constant String :=
           Get_Command_Output ("polyorb-config", (1 .. 0 => null), "",
                               Status'Access);
      begin
         Scan_Dist_Args (PolyORB_Config);
      end;
   exception
      when others =>
         Message ("PolyORB installation not found");
         raise Fatal_Error;
   end Set_PCS_Dist_Flags;

   -----------
   -- Strip --
   -----------

   function Strip (S : String) return Unit_Name_Type is
   begin
      Set_Str_To_Name_Buffer (S);
      Set_Str_To_Name_Buffer (Name_Buffer (4 .. Name_Len));
      Apply_Casing_Rules (Name_Buffer (1 .. Name_Len));

      while Name_Buffer (Name_Len) in '0' .. '9'
        or else Name_Buffer (Name_Len) = '_'
      loop
         Name_Len := Name_Len - 1;
      end loop;
      return Name_Find;
   end Strip;

begin
   Register_Backend ("polyorb", new PolyORB_Backend);
end XE_Back.PolyORB;
