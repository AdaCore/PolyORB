------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--                             X E _ S T U B S                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision$
--                                                                          --
--         Copyright (C) 1995-2004 Free Software Foundation, Inc.           --
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

with GNAT.OS_Lib;      use GNAT.OS_Lib;
with GNAT.Table;

with ALI;              use ALI;
with Hostparm;         use Hostparm;
with Krunch;
with Namet;            use Namet;
with Osint;            use Osint;
with Types;            use Types;
with XE;               use XE;
with XE_Back;          use XE_Back;
with XE_Defs;          use XE_Defs;
with XE_Utils;         use XE_Utils;

package body XE_Stubs is

   BS : Boolean renames XE.Building_Script;

   type Pkg is (None,
                PolyORB,        --  PolyORB
                Access_Points,  --  PolyORB.Access_Points
                Binding_Data,   --  PolyORB.Binding_Data
                Initialization, --  PolyORB.Initialization
                ORB,            --  PolyORB.ORB
                POA_Config,     --  PolyORB.POA_Config
                Setup,          --  PolyORB.Setup
                Tasking         --  PolyORB.Tasking
                );
   Pkg_Name : array (Pkg) of Name_Id;

   Build_Stamp_File      : Types.File_Name_Type;
   Partition_Main_File   : Types.File_Name_Type;
   Partition_Main_Name   : Types.File_Name_Type;

   package Callers  is new GNAT.Table
     (Table_Component_Type => Unit_Id,
      Table_Index_Type     => Natural,
      Table_Low_Bound      => 1,
      Table_Initial        => 20,
      Table_Increment      => 100);

   function C (X : Pkg; S : String) return Name_Id;
   function C (X : Pkg; N : Name_Id := No_Name) return Name_Id;

   procedure Close_File
     (File : in File_Descriptor);
   --  Close file and in case of BS output __EOF__ header

   procedure Copy_Stub
     (Source_Dir, Target_Dir : in File_Name_Type; A : in ALI_Id);
   --  Copy stub files (ali, object) from a source directory to a
   --  target directory. Preserve time stamps.

   procedure Create_File
     (File : out File_Descriptor;
      Name : in Name_Id);
   --  Create file and in case of BS output cat command with __EOF__ header

   procedure Create_Executable_File (PID : in PID_Type);
   --  Compile main partition file and elaboration file.
   --  Bind and link partition to create executable.

   procedure Create_Partition_Main_File (PID : in PID_Type);
   --  Create a procedure which "withes" all the RCI or SP receivers
   --  of the partition and insert the main procedure if needed.

   procedure Create_Stamp_File (PID : in PID_Type);
   --  Create a stamp file in which the executable file stamp and the
   --  configuration file stamp are stored.

   procedure Create_Stub (A : in ALI_Id; Both : in Boolean);
   --  Create the caller stub and the receiver stub for a RCI or SP unit.

   procedure Delete_Stub (Directory, ALI_File : in File_Name_Type);
   --  Delete stub files (ali, object) from a given directory.

   procedure Dwrite_With_Elab_Clause
     (File : in GNAT.OS_Lib.File_Descriptor;
      Unit : in Name_Id);

   function Name (U : Unit_Id) return Name_Id;
   --  Take a unit id and return its name removing unit suffix.

   function Rebuild_Partition (PID : in PID_Type) return Boolean;
   --  Check various file stamps to decide whether the partition
   --  executable should be regenerated.

   Strip_Flag : constant String_Access := new String'("-s");
   --  Option to strip an executable at link time

   -----------
   -- Build --
   -----------

   procedure Build is
      ALI       : ALI_Id;
      PID       : PID_Type;
      CUID      : CUID_Type;
      Both      : Boolean;
      Directory : File_Name_Type;

   begin
      if not Is_Directory (Caller_Dir) then
         Create_Dir (Caller_Dir);
      end if;

      if not Is_Directory (Receiver_Dir) then
         Create_Dir (Receiver_Dir);
      end if;

      for U in CUnits.First .. CUnits.Last loop
         Set_PID (CUnits.Table (U).CUname, CUnits.Table (U).Partition);
      end loop;

      --  Create caller *and* receiver stubs only if we have
      --  to build the partition on which this unit is mapped.
      --  Note that a RCI or SP unit is not always configured when
      --  we don't build the full configuration.

      for U in Units.First .. Units.Last loop

         --  The following test should be :
         --  if Is_RCI_Or_SP_Unit (U)
         --    and then not Units.Table (U).Is_Generic
         --  then
         --  But PolyORB does not handle shared passive units in a general
         --  remote context.

         if Is_RCI_Unit (U)
           and then not Units.Table (U).Is_Generic
         then
            PID := Get_PID (Name (U));
            Both := PID /= Null_PID and then Partitions.Table (PID).To_Build;
            Create_Stub (Units.Table (U).My_ALI, Both);
         end if;
      end loop;

      Remove_GNAT_Flag ("g");
      Remove_GNAT_Flag ("y");

      --  Create and fill partition directories.
      for P in Partitions.First + 1 .. Partitions.Last loop

         if Partitions.Table (P).To_Build
           and then Partitions.Table (P).Passive /= Btrue
         then
            Callers.Init;

            Partitions.Table (P).Executable_File :=
              Partitions.Table (P).Name & Exe_Suffix;

            --  Create storage dir and update executable filename.
            Directory  := Get_Directory (P);
            if Directory /= No_Directory then
               if not Is_Directory (Directory) then
                  Create_Dir (Directory);
               end if;
               Partitions.Table (P).Executable_File :=
                 Dir (Directory, Partitions.Table (P).Executable_File);
            end if;

            --  Create directory in which receiver stubs, main partition
            --  unit and elaboration unit are stored.
            Directory := Get_Internal_Dir (P);
            Partitions.Table (P).Partition_Dir := Directory;
            if not Is_Directory (Directory) then
               Create_Dir (Directory);
            end if;

            --  Mark all units present on this partition and update
            --  the most recent of this partition.

            CUID := Partitions.Table (P).First_Unit;
            while CUID /= Null_CUID loop

               --  Update most recent stamp of this partition
               Most_Recent_Stamp
                 (P, ALIs.Table (CUnits.Table (CUID).My_ALI).Ofile_Full_Name);

               --  Now mark all the dependencies
               Mark_Units_On_Partition (P, CUnits.Table (CUID).My_ALI);
               CUID := CUnits.Table (CUID).Next;
            end loop;

            for U in Units.First .. Units.Last loop

               if Get_PID (Units.Table (U).Uname) = P then
                  ALI := Units.Table (U).My_ALI;

                  --  Update stubs
                  if not Is_RCI_Or_SP_Unit (U) then
                     if Get_Tasking (ALI) = 'Y' then
                        if Debug_Mode then
                           Message ("unit", Name (U),
                                    "on", Partitions.Table (P).Name,
                                    "needs tasking");
                        end if;
                        Set_Tasking (P, True);
                     end if;


                  elsif Get_PID (Name (U)) = P then

                     --  Copy RCI or SP receiver stubs when this unit has been
                     --  assigned on P partition. RCI or SP caller stubs are
                     --  not needed because GNATDIST add the caller directory
                     --  in its include path.

                     if Is_RCI_Unit (U) then
                        Copy_Stub (Receiver_Dir, Directory, ALI);
                     end if;

                     --  We have RCI or SP units in this partition. So, we
                     --  need all the PCS features in this partition.

                     if Debug_Mode then
                        Message ("unit", Name (U),
                                 "on", Partitions.Table (P).Name,
                                 "has remote entities");
                     end if;
                     Set_RCI_Or_RACW (P, True);

                  else

                     --  Remove previous copies of stubs stored.
                     Delete_Stub (Directory, ALIs.Table (ALI).Afile);

                  end if;

                  --  We have RACW types in this partition.So, we
                  --  need all the PCS features in this partition.

                  if Units.Table (U).Has_RACW then
                     if Debug_Mode then
                        Message ("unit", Name (U),
                                 "on", Partitions.Table (P).Name,
                                 "has remote entities");
                     end if;
                     Set_RCI_Or_RACW (P, True);
                  end if;

                  --  Update more recent stamp to decide later on whether
                  --  we should update the partition executable.
                  Most_Recent_Stamp (P, ALIs.Table (ALI).Afile);

                  --  Compute the new checksum. This checksum will be
                  --  useful to identify this partition.
                  Compute_Checksum  (P, Units.Table (U).Sfile);

               end if;

            end loop;

            --  Check whether the partition should have a local termination
            if Get_Termination (P) = Unknown_Termination
              and then not Get_RCI_Or_RACW (P)
              and then Get_Allow_Light_PCS (P) /= Bfalse
              and then Main_Partition /= P
            then
               if Verbose_Mode then
                  Message ("local termination forced for",
                           Partitions.Table (P).Name);
               end if;
               Set_Termination (P, Local_Termination);
            end if;

            if Rebuild_Partition (P) then
               if not Quiet_Mode then
                  Message ("building partition", Partitions.Table (P).Name);
               end if;

               Create_Partition_Main_File (P);
               Create_Executable_File (P);
               Create_Stamp_File (P);
            end if;

         elsif Verbose_Mode then
            Message ("no need to build", Partitions.Table (P).Name);
         end if;
      end loop;

   end Build;

   -------
   -- C --
   -------

   function C (X : Pkg; S : String) return Name_Id is
   begin
      return C (X, Str_To_Id (S));
   end C;

   function C (X : Pkg; N : Name_Id := No_Name) return Name_Id is
     P : Name_Id := Pkg_Name (X);
   begin
      if X = None then
         return N;
      end if;
      Get_Name_String (P);
      if N /= No_Name then
         Add_Char_To_Name_Buffer ('.');
         Get_Name_String_And_Append (N);
      end if;
      case X is
         when None =>
            return No_Name;
         when PolyORB =>
            return Name_Find;
         when Access_Points |
              Tasking       =>
            return C (Setup, Name_Find);
         when others =>
            return C (PolyORB, Name_Find);
      end case;
   end C;

   ----------------
   -- Close_File --
   ----------------

   procedure Close_File
     (File : in File_Descriptor) is
   begin
      if BS then
         Write_Str (Standout, "__EOF__");
         Write_Eol (Standout);
      end if;

      Close (File);
   end Close_File;

   ---------------
   -- Copy_Stub --
   ---------------

   procedure Copy_Stub
     (Source_Dir, Target_Dir : in File_Name_Type; A : in ALI_Id) is
      ALI_Src, ALI_Tgt, Obj_Src, Obj_Tgt : File_Name_Type;

   begin
      ALI_Src := Dir (Source_Dir, ALIs.Table (A).Afile);
      ALI_Tgt := Dir (Target_Dir, ALIs.Table (A).Afile);

      Obj_Src := Strip_Suffix (ALI_Src) & Obj_Suffix;
      Obj_Tgt := Strip_Suffix (ALI_Tgt) & Obj_Suffix;

      if not Is_Regular_File (ALI_Src) then
         Write_Missing_File (ALI_Src);
         raise Fatal_Error;

      else
         Copy_With_File_Stamp (ALI_Src, ALI_Tgt);
      end if;

      if not Is_Regular_File (Obj_Src) then
         Write_Missing_File (Obj_Src);
         raise Fatal_Error;

      else
         Copy_With_File_Stamp (Obj_Src, Obj_Tgt);
      end if;
   end Copy_Stub;

   ----------------------------
   -- Create_Executable_File --
   ----------------------------

   procedure Create_Executable_File (PID : in PID_Type) is
      Directory : File_Name_Type;
      Include   : String_Access;
      Library   : String_Access;

   begin

      Directory := Partitions.Table (PID).Partition_Dir;

      Name_Len := 2;
      Name_Buffer (1) := '-';
      Name_Buffer (2) := 'I';
      Get_Name_String_And_Append (Directory);
      Include := new String'(Name_Buffer (1 .. Name_Len));

      Name_Buffer (2) := 'L';
      Library := new String'(Name_Buffer (1 .. Name_Len));

      Execute_Gcc
        (Dir (Directory, Partition_Main_File & ADB_Suffix),
         Dir (Directory, Partition_Main_File & Obj_Suffix),
         (Include, I_Caller_Dir)
         );

      Execute_Bind
        (Dir (Directory, Partition_Main_File & ALI_Suffix),
         (Include, I_Caller_Dir)
         );

      if Optimization_Mode then

         Execute_Link
           (Dir (Directory, Partition_Main_File & ALI_Suffix),
            Partitions.Table (PID).Executable_File,
            (Library, L_Caller_Dir, Strip_Flag)
            );

      else

         Execute_Link
           (Dir (Directory, Partition_Main_File & ALI_Suffix),
            Partitions.Table (PID).Executable_File,
            (Library, L_Caller_Dir)
            );

      end if;

      XE_Utils.Free (Include);
      XE_Utils.Free (Library);
   end Create_Executable_File;

   -----------------
   -- Create_File --
   -----------------

   procedure Create_File
     (File : out File_Descriptor;
      Name : in Name_Id) is
   begin
      if BS then
         Write_Str  (Standout, "cat >");
         Write_Name (Standout, Name);
         Write_Str  (Standout, " <<__EOF__");
         Write_Eol  (Standout);
      end if;

      Create (File, Name);
   end Create_File;

   --------------------------------
   -- Create_Partition_Main_File --
   --------------------------------

   procedure Create_Partition_Main_File (PID : in PID_Type) is

      Light_PCS   : Boolean;
      Pure_Client : Boolean;
      Filename    : File_Name_Type;
      CU          : CUID_Type;
      Main        : Name_Id;
      File        : File_Descriptor;
      Directory   : File_Name_Type
        renames Partitions.Table (PID).Partition_Dir;

   begin
      Filename := Dir (Directory, Partition_Main_File & ADB_Suffix);
      Create_File (File, Filename);

      Pure_Client := not Get_RCI_Or_RACW (PID)
        and then Main_Partition /= PID
        and then Get_Allow_Light_PCS (PID) /= Bfalse;

      Light_PCS := Pure_Client
        and then not Get_Tasking (PID);

      Dwrite_With_Elab_Clause (File, C (ORB));
      Dwrite_With_Elab_Clause (File, C (Setup));
      Dwrite_With_Elab_Clause (File, C (Setup, "IIOP"));
      Dwrite_With_Elab_Clause (File, C (PolyORB, "Initialization"));

      if Light_PCS then
         Dwrite_With_Elab_Clause (File, C (Tasking,      "No_Tasking"));
         Dwrite_With_Elab_Clause (File, C (ORB,          "No_Tasking"));
         Dwrite_With_Elab_Clause (File, C (Binding_Data, "IIOP"));

      else
         Dwrite_With_Elab_Clause (File, C (Tasking, "Full_Tasking"));

         if Pure_Client then
            Dwrite_With_Elab_Clause (File, C (ORB, "No_Tasking"));
            Dwrite_With_Elab_Clause (File, C (Binding_Data, "IIOP"));

         else
            Dwrite_With_Elab_Clause (File, C (ORB, "Thread_Pool"));
            Dwrite_With_Elab_Clause (File, C (Access_Points, "IIOP"));
            Dwrite_With_Elab_Clause (File, C (POA_Config, "RACWs"));
         end if;
      end if;

      --  First pass to map RCI or SP receivers on the partition.
      CU := Partitions.Table (PID).First_Unit;
      while CU /= Null_CUID loop
         Dwrite_With_Clause (File, False, CUnits.Table (CU).CUname);
         CU := CUnits.Table (CU).Next;
      end loop;

      --  Need the RCI or SP callers to compare their version with the
      --  receiver version.

      for C in Callers.First .. Callers.Last loop
         Dwrite_With_Clause (File, False, Name (Callers.Table (C)));
      end loop;

      Dwrite_Line (File, 0, "procedure ", Partition_Main_Name, " is");
      Dwrite_Line (File, 0, "begin");
      Dwrite_Call (File, 1, No_Str, C (Initialization, "Initialize_World"));
      if not Pure_Client then
         Dwrite_Call (File, 1,
                      Get_Name_String (C (ORB, "Run")),
                      C (Setup, "The_ORB"),
                      "May_Poll => True");
      end if;
      Main := Partitions.Table (PID).Main_Subprogram;
      if Main /= No_Main_Subprogram then
         Dwrite_Call (File, 1, No_Str, Main);
      end if;
      Dwrite_Line (File, 0, "end ", Partition_Main_Name, ";");

      Close_File (File);
   end Create_Partition_Main_File;

   -----------------------
   -- Create_Stamp_File --
   -----------------------

   procedure Create_Stamp_File (PID : in PID_Type) is
      File      : File_Descriptor;
      Directory : File_Name_Type renames Partitions.Table (PID).Partition_Dir;

   begin
      Create (File, Dir (Directory, Build_Stamp_File));
      Write_Str (File, Stamp (Configuration_File));
      Write_Eol (File);
      Write_Str (File, Stamp (Partitions.Table (PID).Executable_File));
      Write_Eol (File);
      Write_Str (File, Stamp (Partitions.Table (PID).Most_Recent));
      Write_Eol (File);
      Close     (File);
      if Debug_Mode then
         Message ("save C =>", No_Name,
                  Stamp (Configuration_File));
         Message ("save E =>", No_Name,
                  Stamp (Partitions.Table (PID).Executable_File));
         Message ("save O =>", No_Name,
                  Stamp (Partitions.Table (PID).Most_Recent));
      end if;
   end Create_Stamp_File;

   -----------------
   -- Create_Stub --
   -----------------

   procedure Create_Stub (A : in ALI_Id; Both : Boolean) is

      function Skip_CWD (File : File_Name_Type) return File_Name_Type;

      function Skip_CWD (File : File_Name_Type) return File_Name_Type
      is
         Len : constant Natural := Normalized_CWD'Length;
      begin
         Get_Name_String (File);
         if Name_Len > Len
           and then Name_Buffer (1 .. Len) = Normalized_CWD
         then
            return Str_To_Id (Name_Buffer (Len + 1 .. Name_Len));
         else
            return File;
         end if;
      end Skip_CWD;

      Obsolete         : Boolean;
      Full_Unit_Spec   : File_Name_Type;
      Full_Unit_Body   : File_Name_Type;
      Full_Unit_File   : File_Name_Type;
      Full_ALI_File    : File_Name_Type;
      Unit_Spec        : File_Name_Type := No_File;
      Unit_Body        : File_Name_Type := No_File;
      Unit_File        : File_Name_Type := No_File;
      ALI_File         : File_Name_Type;
      Caller_Object    : File_Name_Type;
      Caller_ALI       : File_Name_Type;
      Receiver_Object  : File_Name_Type;
      Receiver_ALI     : File_Name_Type;
      Unit_Name        : Unit_Name_Type;
      Is_RCI_Unit      : Boolean := False;

   begin

      --  Because of gnat.adc, use source filename to guess object
      --  filename.

      Unit_Name := Name (ALIs.Table (A).First_Unit);

      if Debug_Mode then
         if Both then
            Message ("create caller and receiver stubs for", Unit_Name);
         else
            Message ("create caller stubs for", Unit_Name);
         end if;
      end if;

      for U in ALIs.Table (A).First_Unit .. ALIs.Table (A).Last_Unit loop
         if Units.Table (U).RCI then
            Is_RCI_Unit := True;
         end if;
         case Units.Table (U).Utype is
            when Is_Spec =>
               Unit_Spec      := Units.Table (U).Sfile;
               Full_Unit_Spec := Full_Source_Name (Unit_Spec);
            when Is_Spec_Only =>
               Unit_Spec      := Units.Table (U).Sfile;
               Full_Unit_Spec := Full_Source_Name (Unit_Spec);
               Unit_Body      := Unit_Spec;
               Full_Unit_Body := Full_Unit_Spec;
            when Is_Body =>
               Unit_Body      := Units.Table (U).Sfile;
               Full_Unit_Body := Full_Source_Name (Unit_Body);
            when Is_Body_Only =>
               raise Program_Error;
         end case;
      end loop;

      Full_ALI_File   := Full_Lib_File_Name (ALIs.Table (A).Afile);
      ALI_File        := ALIs.Table (A).Afile;

      Receiver_ALI    := Dir (Receiver_Dir, ALI_File);
      Receiver_Object := Strip_Suffix (Receiver_ALI) & Obj_Suffix;

      Caller_ALI      := Dir (Caller_Dir, ALI_File);
      Caller_Object   := Strip_Suffix (Caller_ALI) & Obj_Suffix;

      --  Do we need to regenerate the caller stub and its ali.
      Obsolete := False;
      if not Obsolete and then not Is_Regular_File (Caller_Object) then
         if Verbose_Mode then
            Write_Missing_File (Caller_Object);
         end if;
         Obsolete := True;
      end if;

      if not Obsolete and then not Is_Regular_File (Caller_ALI) then
         if Verbose_Mode then
            Write_Missing_File (Caller_ALI);
         end if;
         Obsolete := True;
      end if;

      if not Obsolete
        and then Stamp (Full_ALI_File) > Stamp (Caller_ALI) then
         if Verbose_Mode then
            Write_Stamp_Comparison (Full_ALI_File, Caller_ALI);
         end if;
         Obsolete := True;
      end if;

      if Obsolete then
         if Is_RCI_Unit then
            Full_Unit_File := Full_Unit_Spec;
            Unit_File      := Unit_Spec;
         else
            Full_Unit_File := Full_Unit_Body;
            Unit_File      := Unit_Body;
         end if;

         if not Quiet_Mode then
            Message
              ("building", Unit_Name,
               "caller stubs from", Skip_CWD (Full_Unit_File));
         end if;

         declare
            Unit_Obj, Unit_ALI : File_Name_Type;
         begin

            --  Caller_ALI name may not match Unit_File name, because its
            --  name is based on the body source filename. With gnat.adc,
            --  spec and body may have different base names. The caller
            --  stub generation uses the spec source file and GNAT
            --  generates an object file which name is based on the source
            --  file name. In this case, the expected object file and the
            --  generated object file mismatch.

            Unit_ALI := Dir (Caller_Dir,
                             Strip_Suffix (Unit_File) & ALI_Suffix);
            Unit_Obj := Dir (Caller_Dir,
                             Strip_Suffix (Unit_File) & Obj_Suffix);
            Compile_RCI_Caller (Full_Unit_File, Unit_Obj);

            --  Rename the files when mismatch (see above).
            if Unit_ALI /= Caller_ALI then
               Copy_With_File_Stamp (Unit_ALI, Caller_ALI);
               Copy_With_File_Stamp (Unit_Obj, Caller_Object);
            end if;
         end;
      elsif not Quiet_Mode then
         Message ("  ", Unit_Name, "caller stubs is up to date");
      end if;

      if not Both then
         return;
      end if;

      --  Do we need to generate the receiver stub and its ali.
      Obsolete := False;
      if not Obsolete and then not Is_Regular_File (Receiver_Object) then
         if Verbose_Mode then
            Write_Missing_File (Receiver_Object);
         end if;
         Obsolete := True;
      end if;

      if not Obsolete and then not Is_Regular_File (Receiver_ALI) then
         if Verbose_Mode then
            Write_Missing_File (Receiver_ALI);
         end if;
         Obsolete := True;
      end if;

      if not Obsolete
        and then Stamp (Full_ALI_File) > Stamp (Receiver_ALI) then
         if Verbose_Mode then
            Write_Stamp_Comparison (Full_ALI_File, Receiver_ALI);
         end if;
         Obsolete := True;
      end if;

      if Obsolete then
         if not Quiet_Mode then
            Message
              ("building", Unit_Name,
               "receiver stubs from", Skip_CWD (Full_Unit_Body));
         end if;

         Compile_RCI_Receiver (Full_Unit_Body, Receiver_Object);

      elsif not Quiet_Mode then
         Message ("  ", Unit_Name, "receiver stubs is up to date");
      end if;
   end Create_Stub;

   -----------------
   -- Delete_Stub --
   -----------------

   procedure Delete_Stub (Directory, ALI_File  : in File_Name_Type) is
      Stub_ALI : File_Name_Type := Dir (Directory, ALI_File);
      Stub_Obj : File_Name_Type := Strip_Suffix (Stub_ALI) & Obj_Suffix;
   begin
      if Is_Regular_File (Stub_ALI) then
         Delete (Stub_ALI);
      end if;
      if Is_Regular_File (Stub_Obj) then
         Delete (Stub_Obj);
      end if;
   end Delete_Stub;

   -----------------------------
   -- Dwrite_With_Elab_Clause --
   -----------------------------

   procedure Dwrite_With_Elab_Clause
     (File : in GNAT.OS_Lib.File_Descriptor;
      Unit : in Name_Id) is
   begin
      Dwrite_With_Clause (File, False, Unit);
      Dwrite_Call (File, 0, "pragma Warnings", No_Name, "Off", Unit);
      Dwrite_Call (File, 0, "pragma Elaborate_All", Unit);
      Dwrite_Eol  (File);
   end Dwrite_With_Elab_Clause;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
      function To_Filename (N : Name_Id) return Name_Id;
      function To_Filename (N : Name_Id) return Name_Id is
      begin
         Get_Name_String (N);

         --  Fold to lower case and replace dots by dashes

         for J in 1 .. Name_Len loop
            Name_Buffer (J) := To_Lower (Name_Buffer (J));

            if Name_Buffer (J) = '.' then
               Name_Buffer (J) := '-';
            end if;
         end loop;

         Krunch (Name_Buffer, Name_Len, Name_Len, False);

         return Name_Find;
      end To_Filename;

   begin
      Build_Stamp_File     := Str_To_Id ("glade.sta");
      Partition_Main_Name  := Str_To_Id ("Partition");
      Partition_Main_File  := To_Filename (Partition_Main_Name);
      Pkg_Name :=
        (None           => No_Name,
         PolyORB        => Str_To_Id (Get_PCS_Name),
         Access_Points  => Str_To_Id ("Access_Points"),
         Binding_Data   => Str_To_Id ("Binding_Data"),
         Initialization => Str_To_Id ("Initialization"),
         ORB            => Str_To_Id ("ORB"),
         POA_Config     => Str_To_Id ("POA_Config"),
         Setup          => Str_To_Id ("Setup"),
         Tasking        => Str_To_Id ("Tasking"));
   end Initialize;

   -----------------------------
   -- Mark_Units_On_Partition --
   -----------------------------

   procedure Mark_Units_On_Partition
     (PID : in PID_Type;
      Lib : in ALI_Id)
   is
      Current_ALI : ALI_Id;
      Continue    : Boolean;
      First_Unit  : Unit_Id;
      Last_Unit   : Unit_Id;

   begin

      if Debug_Mode then
         Message ("mark ali file", ALIs.Table (Lib).Afile);
      end if;

      First_Unit := ALIs.Table (Lib).First_Unit;
      Last_Unit  := ALIs.Table (Lib).Last_Unit;

      for U in ALIs.Table (Lib).First_Unit ..
               ALIs.Table (Lib).Last_Unit loop
         if Is_RCI_Or_SP_Unit (U)
           and then not Units.Table (U).Is_Generic
           and then Get_PID (Name (U)) /= PID
         then
            First_Unit := U;
            Last_Unit  := U;
            Callers.Increment_Last;
            Callers.Table (Callers.Last) := U;
            if Debug_Mode then
               Message ("insert caller", Name (U));
            end if;
            exit;
         end if;
      end loop;

      --  Mark this unit to avoid infinite recursive search.
      for U in First_Unit .. Last_Unit loop
         Set_PID (Units.Table (U).Uname, PID);
      end loop;

      for I in First_Unit .. Last_Unit loop
         for J in Units.Table (I).First_With .. Units.Table (I).Last_With loop

            --  Avoid generic units.
            Continue := not (Withs.Table (J).Afile = No_File);

            if Continue then

               --  An Afile name key is its ALI id.
               Current_ALI := Get_ALI_Id (Withs.Table (J).Afile);

               --  If this ALI file has not been loaded, then we do not
               --  need to check it (this means that we encountered an
               --  internal unit that was not specified in the configuration
               --  file and thus not recursively checked for consistency).
               if Current_ALI = No_ALI_Id then
                  Continue := False;
               end if;
            end if;

            if Continue then
               for K in ALIs.Table (Current_ALI).First_Unit ..
                        ALIs.Table (Current_ALI).Last_Unit loop

                  --  No need to search deeper. Already done.
                  if Get_PID (Units.Table (K).Uname) = PID then
                     Continue := False;
                     exit;
                  end if;
               end loop;
            end if;

            --  This unit has not been scanned
            if Continue then
               Mark_Units_On_Partition (PID, Current_ALI);
            end if;
         end loop;
      end loop;
   end Mark_Units_On_Partition;

   ----------
   -- Name --
   ----------

   function Name (U : Unit_Id) return Name_Id is
   begin
      return U_To_N (Units.Table (U).Uname);
   end Name;

   -----------------------
   -- Rebuild_Partition --
   -----------------------

   function Rebuild_Partition (PID : in PID_Type) return Boolean is
      Executable  : File_Name_Type
        renames Partitions.Table (PID).Executable_File;
      Most_Recent : File_Name_Type
        renames Partitions.Table (PID).Most_Recent;
      Partition   : Name_Id renames Partitions.Table (PID).Name;
      Stamp_File  : File_Name_Type;

      Ptr         : Source_Ptr;
      Buffer      : Source_Buffer_Ptr;
      Exec_Stamp1 : Time_Stamp_Type;
      Exec_Stamp2 : Time_Stamp_Type;
      Conf_Stamp1 : Time_Stamp_Type;
      Conf_Stamp2 : Time_Stamp_Type;
      Obj_Stamp1  : Time_Stamp_Type;
      Obj_Stamp2  : Time_Stamp_Type;

   begin
      if Verbose_Mode then
         Message ("check stamps for", Partition);
      end if;

      --  Check that executable exists and is up to date vs new object files.
      if not Is_Regular_File (Executable) then
         if Debug_Mode then
            Write_Missing_File (Executable);
         end if;
         return True;
      elsif Stamp (Most_Recent) > Stamp (Executable) then
         if Debug_Mode then
            Write_Stamp_Comparison (Most_Recent, Executable);
         end if;
         return True;
      end if;

      --  Check that stamp file exists.
      Stamp_File :=
        Dir (Partitions.Table (PID).Partition_Dir, Build_Stamp_File);
      if not Is_Regular_File (Stamp_File) then
         if Debug_Mode then
            Write_Missing_File (Stamp_File);
         end if;
         return True;
      end if;

      --  Check that stamps in stamp file corresponds to the
      --  executable file stamp and configuration file stamp.
      Read_Source_File (Stamp_File, First_Source_Ptr, Ptr, Buffer);

      --  Load executable file stamp.
      Ptr := Buffer.all'First;
      for I in Conf_Stamp1'Range loop
         if Buffer (Ptr) not in '0' .. '9' then
            return True;
         end if;
         Conf_Stamp1 (I) := Buffer (Ptr);
         Ptr := Ptr + 1;
      end loop;
      Conf_Stamp2 := Source_File_Stamp (Configuration_File);
      if Debug_Mode then
         Message ("load C =>", No_Name, String (Conf_Stamp1));
         Message ("find C =>", No_Name, String (Conf_Stamp2));
      end if;

      --  Compare this file stamp with the current executable file stamp.
      if Conf_Stamp1 /= Conf_Stamp2 then
         if Verbose_Mode then
            Message ("configuration file modified for partition", Partition);
         end if;
         return True;
      end if;

      --  Load new line (Unix, DOS or Windows).
      if Buffer (Ptr) /= ASCII.LF and then Buffer (Ptr) /= ASCII.CR then
         return True;
      end if;

      if Buffer (Ptr) = ASCII.CR then
         Ptr := Ptr + 1;
      end if;
      Ptr := Ptr + 1;

      --  Load configuration file stamp.
      for I in Exec_Stamp1'Range loop
         if Buffer (Ptr) not in '0' .. '9' then
            return True;
         end if;
         Exec_Stamp1 (I) := Buffer (Ptr);
         Ptr := Ptr + 1;
      end loop;
      Exec_Stamp2 := Source_File_Stamp (Executable);
      if Debug_Mode then
         Message ("load E =>", No_Name, String (Exec_Stamp1));
         Message ("read E =>", No_Name, String (Exec_Stamp2));
      end if;

      --  Compare this file stamp with the current configuration file stamp.
      if Exec_Stamp1 /= Exec_Stamp2 then
         if Verbose_Mode then
            Message ("executable file modified for partition", Partition);
         end if;
         return True;
      end if;

      --  Load new line (Unix, DOS or Windows).
      if Buffer (Ptr) /= ASCII.LF and then Buffer (Ptr) /= ASCII.CR then
         return True;
      end if;

      if Buffer (Ptr) = ASCII.CR then
         Ptr := Ptr + 1;
      end if;
      Ptr := Ptr + 1;

      --  Load most recent object file stamp.
      for I in Obj_Stamp1'Range loop
         if Buffer (Ptr) not in '0' .. '9' then
            return True;
         end if;
         Obj_Stamp1 (I) := Buffer (Ptr);
         Ptr := Ptr + 1;
      end loop;
      Obj_Stamp2 := Source_File_Stamp (Most_Recent);
      if Debug_Mode then
         Message ("load O =>", No_Name, String (Obj_Stamp1));
         Message ("find O =>", No_Name, String (Obj_Stamp2));
      end if;

      --  Compare this object stamp with the current object file stamp.
      if Obj_Stamp1 /= Obj_Stamp2 then
         if Verbose_Mode then
            Message ("most recent object modified for partition", Partition);
         end if;
         return True;
      end if;

      return False;
   end Rebuild_Partition;

end XE_Stubs;
