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

with Fname;            use Fname;
with GNAT.Os_Lib;      use GNAT.Os_Lib;
with Make;             use Make;
with Namet;            use Namet;
with Opt;
with Osint;            use Osint;
with Output;           use Output;
with XE;               use XE;
with XE_Back;          use XE_Back;
with XE_Stubs;         use XE_Stubs;
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
      --  * Key of CUnit.Table (U).CUname corresponds to its ALI_Id ie ali
      --  index corresponding to ada unit CUnit.Table (U).CUname.
      --
      --  * Key of Unit.Table (U).Uname corresponds to its CUID_Id ie
      --  mapped unit index corresponding to ada unit Unit.Table (U).Uname
      --  if this unit has been mapped.
      --
      --  * Key of Partitions.Table (P).Name corresponds to its PID.

      Inconsistent : Boolean := False;
      PID  : PID_Type;
      Ali  : ALI_Id;

      Compiled    : Name_Id;
      Args        : Argument_List (Gcc_Switches.First .. Gcc_Switches.Last);
      Main        : Boolean;

      procedure Recompile (Unit : Name_Id);

      procedure Recompile (Unit : Name_Id) is
         File_Name    : Name_Id;
         Missing_Alis : Boolean;
         Object       : Name_Id;
         Stamp        : Time_Stamp_Type;

      begin

         if not Already_Loaded (Unit) then

            File_Name := File_Name_Of_Body (Unit);
            if Full_Source_Name (File_Name) = No_File then
               File_Name := File_Name_Of_Spec (Unit);
               if Full_Source_Name (File_Name) = No_File then
                  Message ("""", Unit, """ cannot be found");
                  Exit_Program (E_Fatal);
               end if;
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
               Max_Process           => 1);

            if Building_Script then
               Write_Compile_Command (File_Name);
            end if;

         end if;
      end Recompile;

   begin

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

      for U in CUnit.First .. CUnit.Last loop

         --  Recompile all the configured units to check that
         --  they are present. It is also a way to load the ali files
         --  in the ALIs table.
         Recompile (CUnit.Table (U).CUname);

      end loop;

      if Debug_Mode then
         Message ("load external configured units");
      end if;

      for H in Hosts.First .. Hosts.Last loop

         if not Hosts.Table (H).Static and then
            Hosts.Table (H).Import = Ada_Import then
            Recompile (Hosts.Table (H).External);
         end if;
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

               if Unit.Table (I).RCI then

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
           and then Get_CUID (Unit.Table (U).Uname) = Null_CUID
         then
            Write_Program_Name;
            Write_Str (": RCI Ada unit """);
            Write_Unit_Name (Unit.Table (U).Uname);
            Write_Str (""" has not been assigned to a partition");
            Write_Eol;
            Inconsistent := True;
         end if;
      end loop;

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

      Main_Subprogram := Get_Main_Subprogram (Main_Partition);
      if ALIs.Table (Get_ALI_Id (Main_Subprogram)).Main_Program = None then
         Message ("""", Main_Subprogram, """ is not a main program");
         Inconsistent := True;
      end if;

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
            Set_CID (Lower & Parent_Dir & Upper, Null_CID);
         end loop;
         for C in Channels.First + 1 .. Channels.Last loop
            Lower :=
              Partitions.Table (Channels.Table (C).Lower.My_Partition).Name;
            Upper :=
              Partitions.Table (Channels.Table (C).Upper.My_Partition).Name;
            if Get_CID (Lower & Parent_Dir & Upper) /= Null_CID then
               Message ("two channels define """, Lower,
                        """ and """, Upper, """ pair");
               Inconsistent := True;
            end if;
            Set_CID (Lower & Parent_Dir & Upper, C);
         end loop;
      end;

      if Inconsistent then
         raise Partitioning_Error;
      end if;

   end Check;

end XE_Check;
