------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--                             X E _ C H E C K                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision$                             --
--                                                                          --
--         Copyright (C) 1996-1998 Free Software Foundation, Inc.           --
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
with Fname;            use Fname;
with GNAT.OS_Lib;      use GNAT.OS_Lib;
with Make;             use Make;
with Namet;            use Namet;
with Opt;
with Osint;            use Osint;
with Types;            use Types;
with XE;               use XE;
with XE_Back;          use XE_Back;
with XE_Utils;         use XE_Utils;

package body XE_Check is

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      Initialize_ALI;
   end Initialize;

   -----------
   -- Check --
   -----------

   procedure Check is

      --  Once this procedure called, we have the following properties:
      --
      --  * Info of CUnit.Table (U).CUname corresponds to its ALI_Id ie ali
      --  index corresponding to ada unit CUnit.Table (U).CUname.
      --
      --  * Info of Unit.Table (U).Uname corresponds to its CUID_Id ie
      --  mapped unit index corresponding to ada unit Unit.Table (U).Uname
      --  if this unit has been mapped.
      --
      --  * Info of Partitions.Table (P).Name corresponds to its PID.

      Inconsistent : Boolean := False;
      PID  : PID_Type;
      Ali  : ALI_Id;

      Compiled    : Name_Id;
      Args        : Argument_List (Gcc_Switches.First .. Gcc_Switches.Last);
      Main        : Boolean;

      procedure Load_ALIs (Afile : in File_Name_Type);
      --  Load ali in ali table and recursively load dependencies. Skip
      --  loading for an internal library or an already loaded library.

      procedure Load_Units (Uname : in Name_Id);
      --  Load ali of this unit in ali table and recursively load
      --  dependencies. Don't load an already loaded library.

      procedure Recompile (Unit : in Name_Id);

      ---------------
      -- Load_ALIs --
      ---------------

      procedure Load_ALIs (Afile : in File_Name_Type) is
         A    : ALI_Id;
         Text : Text_Buffer_Ptr;

      begin
         if Debug_Mode then
            Message ("load alis of ", Afile);
         end if;

         if Is_Internal_File_Name (Afile) then
            if Debug_Mode then
               Message ("... skip ", Afile, " internal library");
            end if;
            return;
         end if;

         A := Get_ALI_Id (Afile);
         if A /= No_ALI_Id then
            if Debug_Mode then
               Message ("... skip ", Afile, " already loaded");
            end if;
            return;
         end if;

         Text := Read_Library_Info (Afile);
         A    := Scan_ALI (Afile, Text);
         ALIs.Table (A).Ofile_Full_Name := Full_Lib_File_Name (Afile);

         for U in ALIs.Table (A).First_Unit .. ALIs.Table (A).Last_Unit loop
            for W in Unit.Table (U).First_With .. Unit.Table (U).Last_With loop
               if Withs.Table (W).Afile /= No_File then
                  Load_ALIs (Withs.Table (W).Afile);
               end if;
            end loop;
         end loop;
      end Load_ALIs;

      ----------------
      -- Load_Units --
      ----------------

      procedure Load_Units (Uname : in Name_Id) is
         A    : ALI_Id;
         Lib  : Name_Id;
         Text : Text_Buffer_Ptr;

      begin
         if Debug_Mode then
            Message ("load alis of ", Uname);
         end if;

         Lib := Lib_File_Name (Find_Source (Uname));

         A := Get_ALI_Id (Lib);
         if A /= No_ALI_Id then
            if Debug_Mode then
               Message ("... skip ", Lib, " already loaded");
            end if;
            return;
         end if;

         Text := Read_Library_Info (Lib);
         A    := Scan_ALI (Lib, Text);
         ALIs.Table (A).Ofile_Full_Name := Full_Lib_File_Name (Lib);

         if Is_Internal_File_Name (Lib) then
            if Debug_Mode then
               Message ("... skip ", Lib, " internal library");
            end if;
            return;
         end if;

         for U in ALIs.Table (A).First_Unit .. ALIs.Table (A).Last_Unit loop
            for W in Unit.Table (U).First_With .. Unit.Table (U).Last_With loop
               if Withs.Table (W).Afile /= No_File then
                  Load_ALIs (Withs.Table (W).Afile);
               end if;
            end loop;
         end loop;
      end Load_Units;

      ---------------
      -- Recompile --
      ---------------

      procedure Recompile (Unit : in Name_Id) is
         File_Name     : Name_Id;
         Missing_Alis  : Boolean;
         Object        : Name_Id;
         Stamp         : Time_Stamp_Type;
         Lib_File      : Name_Id;

      begin
         File_Name := Find_Source (Unit);
         Lib_File  := Lib_File_Name (File_Name);

         if Get_ALI_Id (Lib_File) /= No_ALI_Id then
            return;
         end if;

         if Verbose_Mode then
            Message ("recompile ", Unit);
         end if;

         Compile_Sources
           (Main_Source           => File_Name,
            Args                  => Args,
            First_Compiled_File   => Compiled,
            Most_Recent_Obj_File  => Object,
            Most_Recent_Obj_Stamp => Stamp,
            Main_Unit             => Main,
            Missing_Alis          => Missing_Alis,
            Check_Readonly_Files  => Opt.Check_Readonly_Files,
            Dont_Execute          => No_Recompilation,
            Force_Compilations    => Opt.Force_Compilations,
            In_Place_Mode         => Opt.In_Place_Mode,
            Initialize_Ali_Data   => False,
            Max_Process           => Opt.Maximum_Processes);

         if Building_Script then
            Write_Compile_Command (File_Name);
         end if;
      end Recompile;

   begin

      --  Configure units configured on partition type on every partition.
      declare
         U : CUID_Type := Partitions.Table (Default_Partition).First_Unit;
      begin
         while U /= Null_CUID loop
            for P in Partitions.First + 1 .. Partitions.Last loop
               Add_Conf_Unit (CUnit.Table (U).CUname, P);
            end loop;
            U := CUnit.Table (U).Next;
         end loop;
      end;

      if Debug_Mode then
         Message ("unmark configured units");
      end if;

      for U in CUnit.First .. CUnit.Last loop
         Set_Name_Table_Info (CUnit.Table (U).CUname, 0);
      end loop;

      --  Set future Ada names to null. Compile (or load) all Ada units and
      --  check later on that these are not already used.
      Set_Name_Table_Info (Configuration, 0);

      --  Recompile the non-distributed application.

      if not No_Recompilation then

         Display_Commands (Verbose_Mode or Building_Script);
         for Switch in Gcc_Switches.First .. Gcc_Switches.Last loop
            Args (Switch) := Gcc_Switches.Table (Switch);
         end loop;

      end if;

      if Debug_Mode then
         Message ("load dist. app. units");
      end if;

      --  Recompile all the configured units to check that
      --  they are read Ada units. It is also a way to load the ali
      --  files in the ALIs table and to get the most recent file to
      --  which depends a unit. Load the main subprogram first because
      --  this unit has to match GNAT source file convention. When
      --  this unit is loaded, gnat.adc has been taken into account
      --  and the next units will be allowed to have special naming.

      Main_Subprogram := Get_Main_Subprogram (Main_Partition);
      Recompile (Main_Subprogram);
      for U in CUnit.First .. CUnit.Last loop
         Recompile (CUnit.Table (U).CUname);
      end loop;

      --  These units have to be explicitly loaded. If the library is a
      --  readonly or internal library, it won't be loaded by gnatmake but
      --  we need to load this library in the ALI table. Moreover, when
      --  gnatmake is invoked several times, it won't realize that some
      --  units are already loaded. For this reason, we can't use Recompile
      --  to load the ALI files. Moreover, when an ALI file is incorrect,
      --  the wrong ALI file is kept in the ALI table. So, we choose to
      --  recompile everything, to free the tables and then to reload
      --  everything.

      for A in ALIs.First .. ALIs.Last loop
         Set_Name_Table_Info (ALIs.Table (A).Afile, 0);
         if Debug_Mode then
            Message ("reset ", ALIs.Table (A).Afile);
         end if;
      end loop;

      for U in Unit.First .. Unit.Last loop
         Set_Name_Table_Info (Unit.Table (U).Uname, 0);
         if Debug_Mode then
            Message ("reset ", Unit.Table (U).Uname);
         end if;
      end loop;

      ALIs.Init;
      Unit.Init;
      Withs.Init;
      Sdep.Init;
      Source.Init;

      Load_Units (Main_Subprogram);
      for U in CUnit.First .. CUnit.Last loop
         Load_Units (CUnit.Table (U).CUname);
      end loop;

      --  Set configured unit name key to No_Ali_Id.       (1)

      if Debug_Mode then
         Message ("set configured unit name key to No_Ali_Id");
      end if;

      for U in CUnit.First .. CUnit.Last loop
         Set_ALI_Id (CUnit.Table (U).CUname, No_ALI_Id);
      end loop;

      --  Set ada unit name key to null.                   (2)
      --  Set configured unit name key to the ali file id. (3)

      if Debug_Mode then
         Message ("set ada unit name key to null");
         Message ("set configured unit name key to the ali file id");
      end if;

      for U in Unit.First .. Unit.Last loop
         Set_CUID (Unit.Table (U).Uname, Null_CUID);
         Get_Name_String (Unit.Table (U).Uname);
         Name_Len := Name_Len - 2;
         Set_ALI_Id (Name_Find, Unit.Table (U).My_ALI);
      end loop;

      --  Set partition name key to Null_PID.              (4)

      if Debug_Mode then
         Message ("set partition name key to Null_PID");
      end if;

      for P in Partitions.First + 1 .. Partitions.Last loop
         Set_PID (Partitions.Table (P).Name, Null_PID);
      end loop;

      if not Quiet_Output then
         Message ("checking configuration consistency");
      end if;

      --  Check conf. unit name key to detect non-Ada unit.
      --  Check conf. unit are not multiply configured.

      if Debug_Mode then
         Message ("check conf. unit name key to detect non-Ada unit");
         Message ("check conf. unit are not multiply configured");
      end if;

      for U in CUnit.First .. CUnit.Last loop
         Ali := Get_ALI_Id (CUnit.Table (U).CUname);

         --  Use (3) and (1). If null, then there is no ali
         --  file associated to this configured unit name.
         --  The configured unit is not an Ada unit.

         if Ali = No_ALI_Id then

            --  This unit is not an ada unit
            --  as no ali file has been found.

            Message ("configured unit """, CUnit.Table (U).CUname,
                     """ is not an Ada unit");
            Inconsistent := True;

         else
            for I in ALIs.Table (Ali).First_Unit ..
                     ALIs.Table (Ali).Last_Unit loop

               if Unit.Table (I).Is_Generic then
                  Message ("generic unit """, U_To_N (Unit.Table (I).Uname),
                           """ cannot be assigned to a partition");
                  Inconsistent := True;

               elsif Unit.Table (I).RCI then

                  --  If not null, we have already set this
                  --  configured rci unit name to a partition.

                  if Get_CUID (Unit.Table (I).Uname) /= Null_CUID  then
                     Message ("RCI Ada unit """, CUnit.Table (U).CUname,
                              """ has been assigned twice");
                     Inconsistent := True;
                  end if;

                  --  This RCI has been assigned                  (5)
                  --  and it won't be assigned again.

                  Set_CUID (Unit.Table (I).Uname, U);

               end if;

               --  If there is no body, reference the spec.

               if Unit.Table (I).Utype /= Is_Body then
                  CUnit.Table (U).My_Unit := I;
               end if;

            end loop;

            CUnit.Table (U).My_ALI := Ali;

            --  Set partition name to its index value.             (7)
            --  This way we confirm that the partition is not
            --  empty as it contains at least one unit.

            PID := CUnit.Table (U).Partition;
            Set_PID (Partitions.Table (PID).Name, PID);

         end if;
      end loop;

      --  Use (5) and (2). To check all RCI units (except generics)
      --  are configured.

      if Debug_Mode then
         Message ("check all RCI units are configured");
      end if;

      for U in Unit.First .. Unit.Last loop
         if Unit.Table (U).RCI
           and then not Unit.Table (U).Is_Generic
           and then Get_CUID (Unit.Table (U).Uname) = Null_CUID then
            Message ("RCI Ada unit """, U_To_N (Unit.Table (U).Uname),
                     """ has not been assigned to a partition");
            Inconsistent := True;
         end if;
      end loop;

      if Debug_Mode then
         Message ("check child and parent are configured on same partition");
      end if;

      declare
         Parent : Name_Id;
         Child  : Name_Id;
         PPID   : PID_Type;
         CPID   : PID_Type;
         CUID   : CUID_Type;

      begin
         for U in CUnit.First .. CUnit.Last loop

            --  Update most recent stamp of the partition on which this
            --  unit is configured.
            Most_Recent_Stamp
              (CUnit.Table (U).Partition,
               ALIs.Table (CUnit.Table (U).My_ALI).Ofile_Full_Name);

            --  This check applies to a RCI package.
            if Unit.Table (CUnit.Table (U).My_Unit).RCI then
               Child := CUnit.Table (U).CUname;
               CPID  := CUnit.Table (U).Partition;

               loop
                  Parent := Get_Parent (Child);
                  exit when Parent = No_Name;

                  CUID := Get_CUID (Parent & Spec_Suffix);
                  if CUID /= Null_CUID then

                     --  The child has to be on its parent partition.
                     PPID := CUnit.Table (CUID).Partition;
                     if PPID /= CPID then
                        Message ("""", Parent, """ and """, Child,
                                 """ are not on the same partition");
                        Inconsistent := True;
                     end if;

                  end if;

                  Child := Parent;
               end loop;

            end if;
         end loop;
      end;

      --  Use (7). Check that no partition is empty.

      if Debug_Mode then
         Message ("check that no partition is empty");
      end if;

      for P in Partitions.First + 1 .. Partitions.Last loop
         PID := Get_PID (Partitions.Table (P).Name);
         if PID = Null_PID and then
           Partitions.Table (P).Main_Subprogram = No_Name then
            Message ("partition """, Partitions.Table (P).Name,
                     """ is empty");
            Inconsistent := True;
         end if;
      end loop;

      for U in Unit.First .. Unit.Last loop
         Set_PID (Unit.Table (U).Uname, Null_PID);
      end loop;

      --  Is it still used ?

      for U in CUnit.First .. CUnit.Last loop
         Set_ALI_Id (CUnit.Table (U).CUname, CUnit.Table (U).My_ALI);
      end loop;

      --  Check that the main program is really a main program.

      if ALIs.Table (Get_ALI_Id (Main_Subprogram)).Main_Program = None then
         Message ("""", Main_Subprogram, """ is not a main program");
         Inconsistent := True;
      end if;

      --  Check channel configuration (duplication, inconsistency, ...)

      declare
         Upper, Lower : Name_Id;
      begin
         for C in Channels.First + 1 .. Channels.Last loop
            if Channels.Table (C).Upper.My_Partition =
              Channels.Table (C).Lower.My_Partition then
               Message ("channel """, Channels.Table (C).Name,
                        """ is an illegal pair of partitions");
               Inconsistent := True;
            end if;
            Lower :=
              Partitions.Table (Channels.Table (C).Lower.My_Partition).Name;
            Upper :=
              Partitions.Table (Channels.Table (C).Upper.My_Partition).Name;
            Set_CID (Dir (Lower, Upper), Null_CID);
         end loop;
         for C in Channels.First + 1 .. Channels.Last loop
            Lower :=
              Partitions.Table (Channels.Table (C).Lower.My_Partition).Name;
            Upper :=
              Partitions.Table (Channels.Table (C).Upper.My_Partition).Name;
            if Get_CID (Dir (Lower, Upper)) /= Null_CID then
               Message ("two channels define """, Lower,
                        """ and """, Upper, """ pair");
               Inconsistent := True;
            end if;
            Set_CID (Dir (Lower, Upper), C);
         end loop;
      end;

      if Inconsistent then
         raise Partitioning_Error;
      end if;

   end Check;

end XE_Check;
