------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--                             X E _ C H E C K                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision$
--                                                                          --
--         Copyright (C) 1996-1999 Free Software Foundation, Inc.           --
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
with Fname;            use Fname;
with GNAT.OS_Lib;      use GNAT.OS_Lib;
with Make;             use Make;
with Namet;            use Namet;
with Opt;
with Osint;
with Table;
with Types;            use Types;
with XE;               use XE;
with XE_Back;          use XE_Back;
with XE_Utils;         use XE_Utils;

package body XE_Check is

   type Compilation_Job is
      record
         Uname : Unit_Name_Type;
         Fatal : Boolean;
      end record;

   package Compilation_Jobs  is new Table.Table
     (Table_Component_Type => Compilation_Job,
      Table_Index_Type     => Natural,
      Table_Low_Bound      => 1,
      Table_Initial        => 20,
      Table_Increment      => 100,
      Table_Name           => "Compilation_Jobs");

   Arguments    : Argument_List_Access;
   Old_Obj_Chk  : Boolean;

   Compilation_Jobs_First : Natural := 1;

   function Build_Full_Configuration return Boolean;
   --  True iff we build all the partitions

   procedure Compile_RCI_Spec_Only
     (Uname : in Unit_Name_Type;
      Ufile : in File_Name_Type);
   --  GNAT failed to compile unit Uname. Either it does not find
   --  Ufile or Ufile does not compile. Try to compile the spec file
   --  instead. To do so, check that Ufile is not already a spec file.
   --  If any error occurs in this procedure, then we shall
   --  decide later on if it is a fatal error. If it compiles, then
   --  load the ali file to check it is a RCI unit. If it is not a RCI
   --  unit, remove its ali file and returns. If it is a RCI unit,
   --  push the withed units of the spec unit in the compilation queue.

   function Empty return Boolean;
   --  True iff compilation queue is empty

   procedure Load_Unit
     (Uname : in Unit_Name_Type;
      Afile : in File_Name_Type := No_File);
   --  Load ali file of unit Uname and call Load_Withed_Unit on the withed
   --  units. If Afile is different from No_File, then use it. Otherwise,
   --  try to find it. If the ali file does not exist, then raise a
   --  compilation error.

   procedure Load_Withed_Unit (W : in With_Id);
   --  Load ali file of unit W and call Load_Withed_Unit on the withed
   --  units. If its ali file does not exist, then raise a compilation
   --  error. If W is a RCI or SP unit and W is not marked, then call
   --  Load_Withed_Unit on the withed units of the spec unit only.

   function Marked (U : Unit_Name_Type) return Boolean;
   --  True iff U is already in the compilation queue

   procedure Mask_Object_Consistency_Check;
   --  Preserve Check_Object_Consistency and set it to False

   procedure Pull
     (Uname : out Unit_Name_Type;
      Fatal : out Boolean);
   --  Extract first compilation job from queue

   procedure Push
     (Uname : in  Unit_Name_Type;
      Fatal : in  Boolean);
   --  Append a compilation job in queue and mark Uname to avoid
   --  multiple instances of this job.

   procedure Recompile
     (Uname : in Unit_Name_Type;
      Fatal : in Boolean);
   --  Recompile unit of name Unameand as many as its dependencies it is
   --  possible. When Fatal is true, a compilation failure on file of
   --  unit name Uname raises a fatal error. Otherwise, on a compilation
   --  failure, try to compile the spec file instead.

   procedure Unmask_Object_Consistency_Check;
   --  Restore Check_Object_Consistency

   ------------------------------
   -- Build_Full_Configuration --
   ------------------------------

   function Build_Full_Configuration return Boolean is
   begin
      return Partitions.Table (Default_Partition).To_Build;
   end Build_Full_Configuration;

   -----------
   -- Check --
   -----------

   procedure Check is
      A      : ALI_Id;
      CU     : CUID_Type;
      Uname  : Unit_Name_Type;
      CUname : Unit_Name_Type;
      Fatal  : Boolean;
      Error  : Boolean := False;

   begin
      Compilation_Jobs.Init;

      --  Configure units on every partition when they are
      --  configured on partition type. This is the case when
      --  we have for Partition'Main use Main; Main has to be
      --  configured on every partition.

      CU := Partitions.Table (Default_Partition).First_Unit;
      while CU /= Null_CUID loop
         for P in Partitions.First + 1 .. Partitions.Last loop
            Add_Conf_Unit (CUnits.Table (CU).CUname, P);
         end loop;
         CU := CUnits.Table (CU).Next;
      end loop;

      --  Unmark any configured unit
      for CU in CUnits.First .. CUnits.Last loop
         Set_Name_Table_Info (CUnits.Table (CU).CUname, 0);
      end loop;

      --  Initialize parameters for Compile_Sources
      if not No_Recompilation then
         Arguments
           := new Argument_List (Osint.Gcc_Switches.First ..
                                 Osint.Gcc_Switches.Last);
         Display_Commands (Verbose_Mode or Building_Script);
         for Switch in Osint.Gcc_Switches.First ..
                       Osint.Gcc_Switches.Last loop
            Arguments (Switch) := Osint.Gcc_Switches.Table (Switch);
         end loop;
      end if;

      --  Check consistency of units when we need them to build the
      --  partitions we want to generate. We try to load main subprogram
      --  first, because the main subprogram file name has to follow
      --  GNAT file naming convention. Then, it will take gnat.adc into
      --  account and the next units will be allowed to have special naming.

      if Debug_Mode then
         Message ("check non-dist. app. units");
      end if;

      Main_Subprogram := Get_Main_Subprogram (Main_Partition);
      if Partitions.Table (Main_Partition).To_Build then
         Push (Main_Subprogram, True);
      end if;
      for U in CUnits.First .. CUnits.Last loop
         if To_Build (U) then
            Push (CUnits.Table (U).CUname, True);
         end if;
      end loop;

      while not Empty loop
         Pull (Uname, Fatal);
         Recompile (Uname, Fatal);
      end loop;

      --  These units have to be explicitly loaded. If the library is a
      --  readonly or internal library, it won't be loaded by gnatmake but
      --  we *must* load this library in the ALI table. Moreover, when
      --  Compile_Sources is invoked several times, it won't always realize
      --  that some units are already loaded. We may have to load incomplete
      --  ali file (compiled with spec only). We choose to free the tables
      --  and then to reload everything. We also check that each partition
      --  closure is correct.

      if Debug_Mode then
         Message ("load dist. app. units");
      end if;

      for A in ALIs.First .. ALIs.Last loop
         Set_Name_Table_Info (ALIs.Table (A).Afile, 0);
      end loop;

      for U in Units.First .. Units.Last loop
         Set_Name_Table_Info (Units.Table (U).Uname, 0);
      end loop;

      for S in Source.First .. Source.Last loop
         Set_Name_Table_Info (Source.Table (S).Sfile, 0);
      end loop;

      ALIs.Init;
      Units.Init;
      Withs.Init;
      Sdep.Init;
      Source.Init;

      --  Suppress object consistency because of Ada libraries and RCI
      --  or SP units compiled with spec only. We know that the application
      --  is consistent. Somehow, it is an optimization.

      --  Note that all the configured units have been marked because
      --  they were pushed in the compilation queue. This assertion
      --  is very important and use in Load_Unit and Load_Withed_Units.

      Mask_Object_Consistency_Check;

      if Partitions.Table (Main_Partition).To_Build then
         Load_Unit (Main_Subprogram);
      end if;
      for U in CUnits.First .. CUnits.Last loop
         if To_Build (U) then
            Load_Unit (CUnits.Table (U).CUname);
         end if;
      end loop;

      Unmask_Object_Consistency_Check;

      --  Set configured unit name key to No_Ali_Id       (1)

      Set_ALI_Id (Configuration, No_ALI_Id);
      for U in CUnits.First .. CUnits.Last loop
         Set_ALI_Id (CUnits.Table (U).CUname, No_ALI_Id);
      end loop;

      --  Set ada unit name key to null                   (2)
      --  Set configured unit name key to the ali file id (3)

      for U in Units.First .. Units.Last loop
         Set_CUID (Units.Table (U).Uname, Null_CUID);
         Set_ALI_Id (U_To_N (Units.Table (U).Uname), Units.Table (U).My_ALI);
      end loop;

      --  Set partition name key to Null_PID              (4)

      for P in Partitions.First + 1 .. Partitions.Last loop
         Set_PID (Partitions.Table (P).Name, Null_PID);
      end loop;

      if not Quiet_Output then
         Message ("checking configuration consistency");
      end if;

      --  Check conf. unit name key to detect non-Ada unit.
      --  Check conf. unit are not multiply configured.

      if Debug_Mode then
         Message ("detect non-Ada unit");
         Message ("detect multiply conf. unit");
      end if;

      for U in CUnits.First .. CUnits.Last loop

         if To_Build (U) then

            CUname := CUnits.Table (U).CUname;

            A := Get_ALI_Id (CUname);

            --  Use (3) and (1). If null, then there is no ali
            --  file associated to this configured unit name.
            --  The configured unit is not an Ada unit.

            if A = No_ALI_Id then
               Message ("configured unit", To_String (CUname),
                        "is not an Ada unit");
               Error := True;

            else
               for I in ALIs.Table (A).First_Unit ..
                 ALIs.Table (A).Last_Unit loop

                  Uname := Units.Table (I).Uname;

                  if Units.Table (I).Is_Generic then
                     Message ("generic unit", To_String (CUname),
                              "cannot be assigned to a partition");
                     Error := True;

                  elsif Is_RCI_Or_SP_Unit (I) then

                     --  If not null, we have already set this
                     --  configured rci or sp unit name to a partition.

                     if Get_CUID (Uname) /= Null_CUID  then
                        if Units.Table (I).RCI then
                           Message ("RCI Ada unit",
                                    To_String (CUname),
                                    "has been assigned twice");
                        else
                           Message ("Shared passive Ada unit",
                                    To_String (CUname),
                                    "has been assigned twice");
                        end if;
                        Error := True;
                     end if;

                     --  This RCI or SP has been assigned              (5)
                     --  and it won't be assigned again.

                     Set_CUID (Uname, U);

                  end if;

                  --  Save spec unit id or body unit id if not possible

                  if Units.Table (I).Utype /= Is_Body then
                     CUnits.Table (U).My_Unit := I;
                  end if;

               end loop;

               CUnits.Table (U).My_ALI := A;

               --  Set partition name to its index value.             (7)
               --  This way we confirm that the partition is not
               --  empty as it contains at least one unit.

               declare
                  PID : PID_Type := CUnits.Table (U).Partition;
               begin
                  Set_PID (Partitions.Table (PID).Name, PID);
               end;

            end if;

         end if;
      end loop;

      if Build_Full_Configuration then

         --  Use (5) and (2). To check all RCI or SP units (except generics)
         --  are configured.

         if Debug_Mode then
            Message ("check all RCI or SP units are configured");
         end if;

         for U in Units.First .. Units.Last loop
            Uname := Units.Table (U).Uname;
            if Is_RCI_Or_SP_Unit (U)
              and then not Units.Table (U).Is_Generic
              and then Get_CUID (Uname) = Null_CUID then
               if Units.Table (U).RCI then
                  Message ("RCI Ada unit",
                           To_String (U_To_N (Uname)),
                           "has not been assigned to a partition");
               else
                  Message ("Shared passive Ada unit",
                           To_String (U_To_N (Uname)),
                           "has not been assigned to a partition");
               end if;
               Error := True;
            end if;
         end loop;

      end if;

      if Debug_Mode then
         Message ("check child and parent are on same partition");
      end if;

      declare
         Parent : Name_Id;
         Child  : Name_Id;
         PPID   : PID_Type;
         CPID   : PID_Type;
         CUID   : CUID_Type;

      begin
         for U in CUnits.First .. CUnits.Last loop

            if To_Build (U) then

               --  This check applies to a RCI or SP package
               if Is_RCI_Or_SP_Unit (CUnits.Table (U).My_Unit) then
                  Child := CUnits.Table (U).CUname;
                  CPID  := CUnits.Table (U).Partition;

                  loop
                     Parent := Get_Parent (Child);
                     exit when Parent = No_Name;

                     CUID := Get_CUID (Parent & Spec_Suffix);
                     if CUID /= Null_CUID then

                        --  The child has to be on its parent partition
                        PPID := CUnits.Table (CUID).Partition;
                        if PPID /= CPID then
                           Message ("", To_String (Parent),
                                    "and", To_String (Child),
                                    "are not on the same partition");
                           Error := True;
                        end if;

                     end if;

                     Child := Parent;
                  end loop;

               end if;
            end if;
         end loop;
      end;

      --  Use (7). Check that no partition is empty

      if Debug_Mode then
         Message ("check that no partition is empty");
      end if;

      for P in Partitions.First + 1 .. Partitions.Last loop
         if Partitions.Table (P).To_Build
           and then Get_PID (Partitions.Table (P).Name) = Null_PID
         then
            Message ("partition", To_String (Partitions.Table (P).Name),
                     "is empty");
            Error := True;
         end if;
      end loop;

      --  Check that the main program is really a main program

      if Partitions.Table (Main_Partition).To_Build
        and then
        ALIs.Table (Get_ALI_Id (Main_Subprogram)).Main_Program = None
      then
         Message ("", To_String (Main_Subprogram), "is not a main program");
         Error := True;
      end if;

      --  Check channel configuration (duplication, inconsistency, ...)

      declare
         Upper, Lower : Name_Id;
      begin
         for C in Channels.First + 1 .. Channels.Last loop
            if Channels.Table (C).Upper.My_Partition =
              Channels.Table (C).Lower.My_Partition then
               Message ("channel", To_String (Channels.Table (C).Name),
                        "is an illegal pair of partitions");
               Error := True;
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
               Message ("two channels define", To_String (Lower),
                        "and", To_String (Upper), "pair");
               Error := True;
            end if;
            Set_CID (Dir (Lower, Upper), C);
         end loop;
      end;

      if Error then
         raise Partitioning_Error;
      end if;

      --  Once this procedure called, we have the following properties:
      --
      --  * Info of CUnits.Table (U).CUname corresponds to its ALI_Id ie ali
      --  index corresponding to ada unit CUnits.Table (U).CUname.
      --
      --  * Info of Units.Table (U).Uname corresponds to its CUID_Id ie
      --  mapped unit index corresponding to ada unit Units.Table (U).Uname
      --  if this unit has been mapped.
      --
      --  * Info of Partitions.Table (P).Name corresponds to its PID.

   end Check;

   ---------------------------
   -- Compile_RCI_Spec_Only --
   ---------------------------

   procedure Compile_RCI_Spec_Only
     (Uname : in Unit_Name_Type;
      Ufile : in File_Name_Type) is
      Source : File_Name_Type;
      Afile  : File_Name_Type;
      Text   : Text_Buffer_Ptr;

      A : ALI_Id;
      R : Unit_Id;

   begin
      if Debug_Mode then
         Message ("try to rescue", Uname, "or", Ufile);
      end if;

      --  This applies only to specs
      Source := Find_Source (To_Spec (Uname));

      --  Either the source is not available or GNAT failed to compile
      --  the spec file. Return immediatly and decide if it is fatal
      --  error later on.

      if Source = No_File
        or else  Source = Ufile
      then
         return;
      end if;

      if Verbose_Mode then
         Message ("compile", To_String (Source),
                  "instead of", To_String (Ufile));
      end if;

      Execute_Gcc (Source, No_File, Arguments.all, False);

      --  In this mode, there is no object file. Switch off
      --  object consistency check to restore it later.

      Mask_Object_Consistency_Check;

      Afile := Osint.Lib_File_Name (Source);
      Text  := Osint.Read_Library_Info (Afile);

      --  Decide later on if the compilation failure is a problem
      if Text = null then
         return;
      end if;

      Unmask_Object_Consistency_Check;

      A := Scan_ALI (Afile, Text, False, True);
      Free (Text);
      ALIs.Table (A).Ofile_Full_Name := Osint.Full_Lib_File_Name (Afile);
      Set_Source_Table (A);

      --  If we succeed to compile the spec, then check whether it
      --  is a RCI unit. R corresponds to RCI spec unit id.

      R := No_Unit_Id;
      for U in ALIs.Table (A).First_Unit .. ALIs.Table (A).Last_Unit loop
         if Is_RCI_Or_SP_Unit (U) then
            R := U;
            exit;
         end if;
      end loop;

      --  We tried to compile a non-RCI spec. We should have not done that
      --  and we remove the ali file. Note that we decide if it is a fatal
      --  error later on.

      if R = No_Unit_Id then
         Delete (Afile);
         return;
      end if;

      --  Any withed unit has to be checked
      for W in Units.Table (R).First_With .. Units.Table (R).Last_With loop
         Source := Get_File_Name (Withs.Table (W).Uname);
         Push (Withs.Table (W).Uname, False);
      end loop;

   end Compile_RCI_Spec_Only;

   -----------
   -- Empty --
   -----------

   function Empty return Boolean is
   begin
      return Compilation_Jobs.Last < Compilation_Jobs_First;
   end Empty;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      Initialize_ALI;
   end Initialize;

   ---------------
   -- Load_Unit --
   ---------------

   procedure Load_Unit
     (Uname : in Unit_Name_Type;
      Afile : in File_Name_Type := No_File) is
      A : ALI_Id;
      L : File_Name_Type;
      S : File_Name_Type;
      T : Text_Buffer_Ptr;

   begin
      S := Find_Source (Uname);
      if Afile = No_File then
         L := Osint.Lib_File_Name (S);
      else
         L := Afile;
      end if;

      if Debug_Mode then
         Message ("load unit", U_To_N (Uname));
      end if;

      A := Get_ALI_Id (L);
      if A /= No_ALI_Id then
         if Debug_Mode then
            Message ("... skip", L, "already loaded");
         end if;
         return;
      end if;

      if Is_Internal_File_Name (L)
        and then not Opt.Check_Readonly_Files then
         if Debug_Mode then
            Message ("... skip", L, "internal library");
         end if;
         return;
      end if;

      T := Osint.Read_Library_Info (L);
      if T = null then
         if S = No_File then
            Source_File_Error (Uname);
         end if;
         Compilation_Error (S);
      end if;

      A := Scan_ALI (L, T, False, True);
      Free (T);
      ALIs.Table (A).Ofile_Full_Name := Osint.Full_Lib_File_Name (L);
      Set_Source_Table (A);

      for I in ALIs.Table (A).First_Unit .. ALIs.Table (A).Last_Unit loop
         if Is_RCI_Or_SP_Unit (I) then

            --  Load_Unit applies to explicitly configured units. For a
            --  RCI, it is a fatal error if we cannot load the whole unit,
            --  because we won't be able to generate caller *and* receiver
            --  stubs (this is the case for explictly configured unit).

            S := Find_Source (Uname, True);

            exit;
         end if;
      end loop;

      for U in ALIs.Table (A).First_Unit .. ALIs.Table (A).Last_Unit loop
         for W in Units.Table (U).First_With .. Units.Table (U).Last_With loop

            --  If the withed unit is configured, then load it later.

            if Withs.Table (W).Afile /= No_File then
               if Marked (Withs.Table (W).Uname) then
                  Load_Unit (Withs.Table (W).Uname, Withs.Table (W).Afile);
               else
                  Load_Withed_Unit (W);
               end if;
            end if;
         end loop;
      end loop;
   end Load_Unit;

   ----------------------
   -- Load_Withed_Unit --
   ----------------------

   procedure Load_Withed_Unit
     (W : in With_Id) is
      A : ALI_Id;
      T : Text_Buffer_Ptr;
      L : File_Name_Type := Withs.Table (W).Afile;
      N : Unit_Name_Type := U_To_N (Withs.Table (W).Uname);

      FU : Unit_Id;
      LU : Unit_Id;

   begin
      if Debug_Mode then
         Message ("load withed unit", N);
      end if;

      if Get_ALI_Id (L) /= No_ALI_Id then
         if Debug_Mode then
            Message ("... skip", L, "already loaded");
         end if;
         return;
      end if;

      if Is_Internal_File_Name (L) then
         if Debug_Mode then
            Message ("... skip", L, "internal library");
         end if;
         return;
      end if;

      T := Osint.Read_Library_Info (L);
      if T = null then
         Compilation_Error (Withs.Table (W).Sfile);
      end if;

      A := Scan_ALI (L, T, False, True);
      Free (T);
      ALIs.Table (A).Ofile_Full_Name := Osint.Full_Lib_File_Name (L);
      Set_Source_Table (A);

      --  Special case: we can afford to load only withed units of a RCI
      --  spec unit because the unit is configured on a partition which is
      --  not going be generated. Is this the case?

      FU := ALIs.Table (A).First_Unit;
      LU := ALIs.Table (A).Last_Unit;
      for I in ALIs.Table (A).First_Unit .. ALIs.Table (A).Last_Unit loop
         if Is_RCI_Or_SP_Unit (I) then
            FU := I;
            LU := I;
            exit;
         end if;
      end loop;

      for U in FU .. LU loop
         for W in Units.Table (U).First_With .. Units.Table (U).Last_With loop

            --  If the withed unit is configured, load it later.

            if Withs.Table (W).Afile /= No_File
              and then not Marked (Withs.Table (W).Uname)
            then
               Load_Withed_Unit (W);
            end if;
         end loop;
      end loop;

   end Load_Withed_Unit;

   ------------
   -- Marked --
   ------------

   function Marked (U : Unit_Name_Type) return Boolean is
   begin
      return Get_Name_Table_Info (U_To_N (U)) /= 0;
   end Marked;

   -----------------------------------
   -- Mask_Object_Consistency_Check --
   -----------------------------------

   procedure Mask_Object_Consistency_Check is
   begin
      Old_Obj_Chk := Opt.Check_Object_Consistency;
      Opt.Check_Object_Consistency := False;
   end Mask_Object_Consistency_Check;

   ---------
   -- Pull --
   ---------

   procedure Pull
     (Uname : out Unit_Name_Type;
      Fatal : out Boolean) is
   begin
      Uname := Compilation_Jobs.Table (Compilation_Jobs_First).Uname;
      Fatal := Compilation_Jobs.Table (Compilation_Jobs_First).Fatal;
      Compilation_Jobs_First := Compilation_Jobs_First + 1;
   end Pull;

   ----------
   -- Push --
   ----------

   procedure Push
     (Uname : in Unit_Name_Type;
      Fatal : in Boolean) is
      N : Name_Id := U_To_N (Uname);

   begin
      if Get_Name_Table_Info (N) /= 0 then
         return;
      end if;
      Compilation_Jobs.Increment_Last;
      Compilation_Jobs.Table (Compilation_Jobs.Last).Uname := Uname;
      Compilation_Jobs.Table (Compilation_Jobs.Last).Fatal := Fatal;
      Set_Name_Table_Info (N, Int (Compilation_Jobs.Last));
   end Push;

   ---------------
   -- Recompile --
   ---------------

   procedure Recompile
     (Uname : in Unit_Name_Type;
      Fatal : in Boolean) is

      File  : File_Name_Type;
      Unit  : Unit_Name_Type;

      Source  : File_Name_Type;
      Object  : File_Name_Type;
      Afile   : File_Name_Type;
      Dummy   : File_Name_Type;
      Stamp   : Time_Stamp_Type;

      Main     : Boolean;
      Count    : Natural;
      Found    : Boolean;

   begin
      Source := Find_Source (Uname, Fatal);
      if Source = No_File then

         --  If Uname is an encoded unit name, it is not an explicitly
         --  conf. unit. It is possible that we do not need the whole
         --  unit but only its spec (if it is a RCI). Modify unit name
         --  and try again. But now, it is a fatal error if it fails.

         Unit := Uname;
         if Is_Body_Name (Unit) then
            Unit := U_To_N (Unit);
         end if;
         Source := Find_Source (Unit, True);
         Compile_RCI_Spec_Only (Unit, Source);
         return;
      end if;

      Afile  := Osint.Lib_File_Name (Source);
      if Get_ALI_Id (Afile) /= No_ALI_Id then
         return;
      end if;

      Compile_Sources
        (Main_Source           => Source,
         Args                  => Arguments.all,
         First_Compiled_File   => Dummy,
         Most_Recent_Obj_File  => Object,
         Most_Recent_Obj_Stamp => Stamp,
         Main_Unit             => Main,
         Compilation_Failures  => Count,
         Check_Readonly_Files  => Opt.Check_Readonly_Files,
         Do_Not_Execute        => No_Recompilation,
         Force_Compilations    => Opt.Force_Compilations,
         Keep_Going            => True,
         In_Place_Mode         => Opt.In_Place_Mode,
         Initialize_Ali_Data   => False,
         Max_Process           => Opt.Maximum_Processes);

      for I in 1 .. Count loop
         Extract_Failure (File, Unit, Found);

         if Debug_Mode then
            Message ("cannot compile unit", U_To_N (Unit));
         end if;

         --  When this failure comes from the original unit (i.e
         --  explicitly configured on a partition), then it is a
         --  fatal error.

         if Fatal and then Source = File then
            Compilation_Error (Source);
         end if;

         --  It is possible that the failure comes from a unit that
         --  is not needed to build the current set of partitions.
         --  We will decide later whether we need it or not. But in the
         --  case of a RCI unit, it could be useful to compile only the
         --  spec because we just need the caller stubs. Check whether
         --  Unit is already in the compilation queue. For that, Unit has
         --  to be different from No_Name. This is true with
         --  Compile_Sources when Source was successfully compiled.

         if Source /= File and then not Marked (Unit) then
            Compile_RCI_Spec_Only (Unit, File);
         end if;
      end loop;

      if Building_Script then
         --  ???
         Write_Compile_Command (Source);
      end if;
   end Recompile;

   -------------------------------------
   -- Unmask_Object_Consistency_Check --
   -------------------------------------

   procedure Unmask_Object_Consistency_Check is
   begin
      Opt.Check_Object_Consistency := Old_Obj_Chk;
   end Unmask_Object_Consistency_Check;

end XE_Check;
