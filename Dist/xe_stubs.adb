------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--                             X E _ S T U B S                              --
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
with GNAT.OS_Lib;      use GNAT.OS_Lib;
with Namet;            use Namet;
with Osint;            use Osint;
with Types;            use Types;
with Unchecked_Deallocation;
with XE;               use XE;
with XE_Back;          use XE_Back;
with XE_Defs;          use XE_Defs;
with XE_Utils;         use XE_Utils;

package body XE_Stubs is

   Stdout : Boolean;

   procedure Copy_Stub
     (Source_Dir, Target_Dir : in File_Name_Type; A : in ALI_Id);
   --  Copy stub files (ali, object) from a source directory to a
   --  target directory. Preserve time stamps.

   procedure Create_Elaboration_File (PID : in PID_Type);
   --  Create the elaboration unit for the given partition. This unit
   --  overloads the default GARLIC settings.

   procedure Create_Executable_File (PID : in PID_Type);
   --  Compile main partition file and elaboration file.
   --  Bind and link partition to create executable.

   procedure Create_Partition_Main_File (PID : in PID_Type);
   --  Create a procedure which "withes" all the RCI receivers
   --  of the partition and insert the main procedure if needed.

   procedure Create_Stamp_File (PID : in PID_Type);
   --  Create a stamp file in which the executable file stamp and the
   --  configuration file stamp are stored.

   procedure Create_Stub (A : in ALI_Id);
   --  Create the caller stub and the receiver stub for a RCI unit.

   procedure Delete_Stub (Source_Dir, Base_Name : in File_Name_Type);
   --  Delete stub files (ali, object) from a source directory.

   procedure Dwrite_Eol (File   : in File_Descriptor;
                         Stdout : in Boolean := Building_Script)
     renames Write_Eol;
   --  Changed default parameter.

   procedure Dwrite_Line (FD  : in File_Descriptor;
                          Ind : in Int;
                          S1  : in String;
                          N1  : in Name_Id := No_Name;
                          S2  : in String  := "";
                          N2  : in Name_Id := No_Name;
                          S3  : in String  := "";
                          N3  : in Name_Id := No_Name);

   procedure Dwrite_Name (File   : in File_Descriptor;
                          Name   : in Name_Id;
                          Stdout : in Boolean := Building_Script)
     renames Write_Name;
   --  Changed default parameter.

   procedure Dwrite_Str (File   : in File_Descriptor;
                         Line   : in String;
                         Stdout : in Boolean := Building_Script)
     renames Write_Str;
   --  Changed default parameter.

   procedure Dwrite_With_Clause
     (File       : in File_Descriptor;
      Unit       : in String;
      Child      : in Name_Id := No_Name;
      Use_Clause : in Boolean := True);
   --  Add a with clause and possibly a use clause as well.

   function Rebuild_Partition (PID : in PID_Type) return Boolean;
   --  Check various file stamps to decide whether the partition
   --  executable should be regenerated.

   -----------
   -- Build --
   -----------

   procedure Build is
      Afile     : File_Name_Type;
      CUID      : CUID_Type;
      Directory : File_Name_Type;

   begin
      if not Is_Directory (Caller_Dir) then
         Create_Dir (Caller_Dir);
      end if;

      if not Is_Directory (Receiver_Dir) then
         Create_Dir (Receiver_Dir);
      end if;

      --  Generate all the stubs (objects and alis).
      for CUID in CUnit.First .. CUnit.Last loop
         if Unit.Table (CUnit.Table (CUID).My_Unit).RCI
           and then not Unit.Table (CUnit.Table (CUID).My_Unit).Is_Generic then
            Create_Stub (CUnit.Table (CUID).My_ALI);
         end if;
      end loop;

      --  Create and fill partition directories.
      for PID in Partitions.First + 1 .. Partitions.Last loop

         if Partitions.Table (PID).To_Build then
            Partitions.Table (PID).Executable_File :=
              Partitions.Table (PID).Name & Exe_Suffix;

            --  Create storage dir and update executable filename.
            Directory  := Get_Storage_Dir (PID);
            if Directory /= No_Storage_Dir then
               if not Is_Directory (Directory) then
                  Create_Dir (Directory);
               end if;
               Partitions.Table (PID).Executable_File :=
                 Dir (Directory, Partitions.Table (PID).Executable_File);
            end if;

            --  Create directory in which receiver stubs, main partition
            --  unit and elaboration unit are stored.
            Directory := Get_Partition_Dir (PID);
            Partitions.Table (PID).Partition_Dir := Directory;
            if not Is_Directory (Directory) then
               Create_Dir (Directory);
            end if;

            --  Mark all units present on this partition.
            CUID := Partitions.Table (PID).First_Unit;
            while CUID /= Null_CUID loop
               Mark_Units_On_Partition (PID, CUnit.Table (CUID).My_ALI);
               Set_PID (CUnit.Table (CUID).CUname, PID);
               CUID := CUnit.Table (CUID).Next;
            end loop;

            for U in Unit.First .. Unit.Last loop

               if Get_PID (Unit.Table (U).Uname) = PID then
                  Afile := ALIs.Table (Unit.Table (U).My_ALI).Afile;

                  if not Unit.Table (U).RCI then
                     Most_Recent_Stamp (PID, Afile);

                  elsif Get_PID (U_To_N (Unit.Table (U).Uname)) = PID then

                     --  Copy RCI receiver stubs when this unit has been
                     --  assigned on PID partition. RCI caller stubs are
                     --  not needed because GNATDIST add the caller directory
                     --  in its include path.

                     Copy_Stub
                       (Receiver_Dir, Directory, Unit.Table (U).My_ALI);
                     Most_Recent_Stamp (PID, Dir (Receiver_Dir, Afile));
                     Set_Light_PCS (PID, False);

                  else
                     Delete_Stub (Directory, Unit.Table (U).Sfile);
                     Most_Recent_Stamp (PID, Dir (Caller_Dir, Afile));
                     if Unit.Table (U).Has_RACW_Type then
                        Set_Light_PCS (PID, False);
                     end if;

                  end if;

               end if;

            end loop;

            if Rebuild_Partition (PID) then
               if not Quiet_Output then
                  Message ("building partition ", Partitions.Table (PID).Name);
               end if;

               Create_Partition_Main_File (PID);
               Create_Elaboration_File (PID);
               Create_Executable_File (PID);
               Create_Stamp_File (PID);
            end if;

         elsif Verbose_Mode then
            Message ("no need to build ", Partitions.Table (PID).Name);

         end if;

      end loop;

   end Build;

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

   -----------------------------
   -- Create_Elaboration_File --
   -----------------------------

   procedure Create_Elaboration_File (PID : in PID_Type) is
      Partition   : Partition_Name_Type;
      Elaboration : File_Name_Type;
      Task_Pool   : Task_Pool_Type;

      CID : CID_Type;
      FD  : File_Descriptor;

   begin
      Partition   := Partitions.Table (PID) .Name;
      Elaboration := Dir (Partitions.Table (PID).Partition_Dir,
                          Elaboration_File & ADB_Suffix);

      if Building_Script then
         Write_Str  (Standout, "cat >");
         Write_Name (Standout, Elaboration);
         Write_Str  (Standout, " <<__EOF__");
         Write_Eol  (Standout);
      end if;

      Create (FD, Elaboration);

      Stdout := Building_Script;

      --  Header.

      Dwrite_With_Clause (FD, "System.Garlic.Filters");
      Dwrite_With_Clause (FD, "System.Garlic.Heart");
      Dwrite_With_Clause (FD, "System.Garlic.Options");

      if Default_Registration_Filter /= No_Filter_Name then
         Dwrite_With_Clause
           (FD, "System.Garlic.Filters",
            GNAT_Style (Default_Registration_Filter), False);
      end if;

      if Get_Filter (PID) /= No_Filter_Name then
         Dwrite_With_Clause
           (FD, "System.Garlic.Filters",
            GNAT_Style (Get_Filter (PID)), False);
      end if;

      if Partitions.Table (PID).First_Channel /= Null_CID then
         CID := Partitions.Table (PID).First_Channel;
         while CID /= Null_CID loop
            if Get_Filter (CID) /= No_Filter_Name then
               Dwrite_With_Clause
                 (FD, "System.Garlic.Filters",
                  GNAT_Style (Get_Filter (CID)), False);
            end if;
            if Channels.Table (CID).Lower.My_Partition = PID then
               CID := Channels.Table (CID).Lower.Next_Channel;
            else
               CID := Channels.Table (CID).Upper.Next_Channel;
            end if;
         end loop;
      end if;

      Dwrite_Line (FD, 0, "package body ", Elaboration_Name, " is");
      Dwrite_Line (FD, 1, "procedure Initialize is");
      Dwrite_Line (FD, 1, "begin");

      --  If the partition holds the main unit, then it cannot be slave.
      --  Otherwise, it is.

      if PID = Main_Partition then
         Dwrite_Line (FD, 2, "Set_Is_Slave (False);");
      else
         Dwrite_Line (FD, 2, "Set_Is_Slave (True);");
      end if;

      --  The partition should not terminate.

      case Get_Termination (PID) is
         when Local_Termination =>
            Dwrite_Line (FD, 2, "Set_Termination (Local_Termination);");
         when Global_Termination =>
            Dwrite_Line (FD, 2, "Set_Termination (Global_Termination);");
         when Deferred_Termination =>
            Dwrite_Line (FD, 2, "Set_Termination (Deferred_Termination);");
         when Unknown_Termination =>
            null;
      end case;

      --  If a protocol has been specified, then use it (with its data
      --  if present).

      if Default_Protocol_Name /= No_Name then
         Get_Name_String (Default_Protocol_Name);
         if Default_Protocol_Data /= No_Name then
            Add_Str_To_Name_Buffer ("://");
            Get_Name_String_And_Append (Default_Protocol_Data);
         end if;
         Dwrite_Line (FD, 2, "Set_Boot_Server (""", Name_Find, """);");
      end if;

      Task_Pool := Get_Task_Pool (PID);
      if Task_Pool /= No_Task_Pool then
         Name_Len := 0;
         for I in 1 .. 3 loop
            Get_Name_String_And_Append (Task_Pool (I));
            exit when I = 3;
            Add_Char_To_Name_Buffer (',');
         end loop;
         Add_Str_To_Name_Buffer (");");
         Dwrite_Line (FD, 2, "Set_Task_Pool_Bound (", Name_Find);
      end if;

      Dwrite_Line (FD, 2, "Set_Partition_Name (""", Partition, """);");

      if Default_Registration_Filter /= No_Filter_Name then
         Dwrite_Line
           (FD, 2, "Set_Registration_Filter (""",
            Default_Registration_Filter, """);");
      end if;

      if Partitions.Table (Default_Partition).Filter /= No_Filter_Name then
         Dwrite_Line
           (FD, 2, "Set_Default_Filter (""",
            Partitions.Table (Default_Partition).Filter, """);");
      end if;

      if Partitions.Table (PID).Last_Channel /= Null_CID then
         CID := Partitions.Table (PID).First_Channel;
         declare
            Filter : Filter_Name_Type;
            Peer   : PID_Type;
         begin
            while CID /= Null_CID loop
               Filter := Get_Filter (CID);
               if Channels.Table (CID).Lower.My_Partition = PID then
                  Peer := Channels.Table (CID).Upper.My_Partition;
                  CID  := Channels.Table (CID).Lower.Next_Channel;
               else
                  Peer := Channels.Table (CID).Lower.My_Partition;
                  CID  := Channels.Table (CID).Upper.Next_Channel;
               end if;
               if Filter /= No_Filter_Name then
                  Dwrite_Line
                    (FD, 2, "Set_Channel_Filter (""",
                     Partitions.Table (Peer).Name,
                     """, """, Filter, """);");
               end if;
            end loop;
         end;
      end if;

      if Get_Light_PCS (PID) then
         Dwrite_Line (FD, 2, "Has_RCI_Pkg_Or_RACW_Var := False;");
      end if;

      --  Footer.

      Dwrite_Line (FD, 1, "end Initialize;");
      Dwrite_Line (FD, 0, "end ", Elaboration_Name, ";");

      if Building_Script then
         Write_Str (Standout, "__EOF__");
         Write_Eol (Standout);
      end if;

      Stdout := False;
      Close (FD);

   end Create_Elaboration_File;

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
        (Dir (Directory, Elaboration_File & ADB_Suffix),
         Dir (Directory, Elaboration_File & Obj_Suffix),
         (GNATLib_Compile_Flag,
          I_GARLIC_Dir)
         );

      Execute_Gcc
        (Dir (Directory, Partition_Main_File & ADB_Suffix),
         Dir (Directory, Partition_Main_File & Obj_Suffix),
         (Include, I_Caller_Dir, I_Current_Dir)
         );

      Execute_Bind
        (Dir (Directory, Partition_Main_File & ALI_Suffix),
         (Include, I_Caller_Dir, I_Current_Dir)
         );

      Execute_Link
        (Dir (Directory, Partition_Main_File & ALI_Suffix),
         Partitions.Table (PID).Executable_File,
         (Library, L_Caller_Dir, L_Current_Dir)
         );

      Free (Include);
      Free (Library);

   end Create_Executable_File;

   --------------------------------
   -- Create_Partition_Main_File --
   --------------------------------

   procedure Create_Partition_Main_File (PID : in PID_Type) is

      Main_File   : File_Name_Type;

      UID   : CUID_Type;
      Host  : Name_Id;
      Main  : Name_Id;

      FD : File_Descriptor;

   begin

      Main_File := Dir (Partitions.Table (PID).Partition_Dir,
                        Partition_Main_File & ADB_Suffix);

      if Building_Script then
         Write_Str  (Standout, "cat >");
         Write_Name (Standout, Main_File);
         Write_Str  (Standout, " <<__EOF__");
         Write_Eol  (Standout);
      end if;

      Stdout := Building_Script;
      Create (FD, Main_File);

      --  Force the RCI receivers to be present on the partition.
      UID := Partitions.Table (PID).First_Unit;
      while UID /= Null_CUID loop
         Set_PID (Unit.Table (CUnit.Table (UID).My_Unit).Uname,
                  CUnit.Table (UID).Partition);
         Dwrite_With_Clause (FD, "", CUnit.Table (UID).CUname, False);
         UID := CUnit.Table (UID).Next;
      end loop;

      --  Need the RCI callers to compare their version with the
      --  receiver version.

      --  First pass to 'with' the RCI callers.
      for U in CUnit.First .. CUnit.Last loop
         if Unit.Table (CUnit.Table (U).My_Unit).RCI then
            if CUnit.Table (U).Partition /= PID and then
              Get_PID (Unit.Table (CUnit.Table (U).My_Unit).Uname) = PID then
               Dwrite_With_Clause (FD, "", CUnit.Table (U).CUname, False);
            end if;
         else
            Set_PID (Unit.Table (CUnit.Table (U).My_Unit).Uname, Null_PID);
         end if;
      end loop;

      Dwrite_With_Clause (FD, "System.RPC", No_Name, False);
      Dwrite_With_Clause (FD, "System.Garlic.Heart", No_Name, False);
      Dwrite_With_Clause (FD, "System.Garlic.Startup", No_Name, False);
      Dwrite_Line (FD, 0, "pragma Elaborate_All (System.Garlic.Startup);");
      Dwrite_With_Clause (FD, "System.Garlic.Termination", No_Name, False);
      Dwrite_With_Clause (FD, "System.Partition_Interface", No_Name, False);
      Dwrite_With_Clause (FD, "System.Partition_Interface.Startup",
                          No_Name, False);

      Dwrite_Line (FD, 0, "procedure ", Partition_Main_Name, " is");
      Dwrite_Line (FD, 0, "begin");

      if PID = Main_Partition then
         if Default_Starter = Ada_Import and
            Partitions.First + 1 /= Partitions.Last then
            for Partition in Partitions.First + 1 .. Partitions.Last loop
               if Partition /= Main_Partition then
                  Dwrite_Line
                    (FD, 1, "System.Partition_Interface.Startup.Launch");
                  Name_Len := 0;
                  Add_Str_To_Name_Buffer ("(""");
                  Add_Str_To_Name_Buffer (Get_Rsh_Command);
                  Add_Str_To_Name_Buffer (""",");
                  Dwrite_Line (FD, 2, Name_Buffer (1 .. Name_Len));
                  Host := Get_Host (Partition);
                  if Host = No_Name then
                     Dwrite_Line (FD, 2, "False, """,
                                  Partitions.Table (Partition).Name, """,");
                  else
                     Dwrite_Line (FD, 2, "True, ", Host, ",");
                  end if;
                  Dwrite_Line (FD, 2, """", Get_Absolute_Exec (Partition),
                               " ", Get_Command_Line  (Partition), """);");
               end if;
            end loop;
         end if;

      end if;

      Dwrite_Line (FD, 1, "System.Garlic.Heart.Elaboration_Is_Terminated;");
      Dwrite_Line (FD, 1, "System.RPC.Establish_RPC_Receiver");
      Dwrite_Line (FD, 2, "(Partition'Partition_ID, null);");

      if Default_Version_Check then

         --  Version consistency between receiver and caller.
         --  Checks perform on all the rci caller stubs.

         for U in CUnit.First .. CUnit.Last loop
            if Unit.Table (CUnit.Table (U).My_Unit).RCI and then
              CUnit.Table (U).Partition /= PID and then
              Get_PID (Unit.Table (CUnit.Table (U).My_Unit).Uname) = PID then
               Dwrite_Line
                 (FD, 1, "System.Partition_Interface.Startup.Check (""",
                  CUnit.Table (U).CUname, """, ",
                  CUnit.Table (U).CUname, "'Version);");
               Set_PID (Unit.Table (CUnit.Table (U).My_Unit).Uname, Null_PID);
            end if;
         end loop;
      end if;

      if PID = Main_Partition then
         Dwrite_Line
           (FD, 1,
            "System.Partition_Interface.Startup.Run (",
            Main_Subprogram, "'access);");
      else
         Main := Partitions.Table (PID).Main_Subprogram;
         if Main = No_Main_Subprogram then
            Main := Partitions.Table (Default_Partition).Main_Subprogram;
         end if;
         if Main /= No_Name then
            Dwrite_Line (FD, 1, "   ", Main, ";");
         end if;
      end if;

      --  When we exit main subprogram, just terminate.
      if Get_Termination (PID) = Local_Termination then
         Dwrite_Line
           (FD, 1, "System.Garlic.Termination.Local_Termination;");
      end if;

      Dwrite_Line (FD, 0, "end ", Partition_Main_Name, ";");

      if Building_Script then
         Write_Str (Standout, "__EOF__");
         Write_Eol (Standout);
      end if;

      Stdout := False;
      Close (FD);

   end Create_Partition_Main_File;

   -----------------------
   -- Create_Stamp_File --
   -----------------------

   procedure Create_Stamp_File (PID : in PID_Type) is
      FD  : File_Descriptor;

   begin
      Create
        (FD, Dir (Partitions.Table (PID).Partition_Dir, Build_Stamp_File));
      Write_Str (FD, Stamp (Configuration_File));
      Write_Eol (FD);
      Write_Str (FD, Stamp (Partitions.Table (PID).Executable_File));
      Write_Eol (FD);
      Write_Str (FD, Stamp (Partitions.Table (PID).Most_Recent));
      Write_Eol (FD);
      Close     (FD);
      if Debug_Mode then
         Message ("save C => ", No_Name,
                  Stamp (Configuration_File));
         Message ("save E => ", No_Name,
                  Stamp (Partitions.Table (PID).Executable_File));
         Message ("save O => ", No_Name,
                  Stamp (Partitions.Table (PID).Most_Recent));
      end if;
   end Create_Stamp_File;

   -----------------
   -- Create_Stub --
   -----------------

   procedure Create_Stub (A : in ALI_Id) is
      Obsolete        : Boolean;
      Full_RCI_Spec   : File_Name_Type;
      Full_RCI_Body   : File_Name_Type;
      Full_ALI_File   : File_Name_Type;
      RCI_Spec        : File_Name_Type := No_File;
      RCI_Body        : File_Name_Type := No_File;
      ALI_File        : File_Name_Type;
      Caller_Object   : File_Name_Type;
      Caller_ALI      : File_Name_Type;
      Receiver_Object : File_Name_Type;
      Receiver_ALI    : File_Name_Type;
      Unit_Name       : Unit_Name_Type;

   begin

      --  Because of gnat.adc, use source filename to guess object
      --  filename.

      Unit_Name := U_To_N (Unit.Table (ALIs.Table (A).First_Unit).Uname);
      for U in ALIs.Table (A).First_Unit .. ALIs.Table (A).Last_Unit loop
         case Unit.Table (U).Utype is
            when Is_Spec =>
               RCI_Spec      := Unit.Table (U).Sfile;
               Full_RCI_Spec := Full_Source_Name (RCI_Spec);
            when Is_Spec_Only =>
               RCI_Spec      := Unit.Table (U).Sfile;
               Full_RCI_Spec := Full_Source_Name (RCI_Spec);
               RCI_Body      := RCI_Spec;
               Full_RCI_Body := Full_RCI_Spec;
            when Is_Body =>
               RCI_Body      := Unit.Table (U).Sfile;
               Full_RCI_Body := Full_Source_Name (RCI_Body);
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

      elsif not Obsolete
        and then Stamp (Full_RCI_Spec) > Stamp (Caller_Object) then
         if Verbose_Mode then
            Write_Stamp_Comparison (Full_RCI_Spec, Caller_Object);
         end if;
         Obsolete := True;
      end if;

      if not Obsolete and then not Is_Regular_File (Caller_ALI) then
         if Verbose_Mode then
            Write_Missing_File (Caller_ALI);
         end if;
         Obsolete := True;

      elsif not Obsolete
        and then Stamp (Full_RCI_Spec) > Stamp (Caller_ALI) then
         if Verbose_Mode then
            Write_Stamp_Comparison (Full_RCI_Spec, Caller_ALI);
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
         if not Quiet_Output then
            Message
              ("building ", Unit_Name, " caller stubs from ", Full_RCI_Spec);
         end if;

         declare
            Spec_Obj, Spec_ALI : File_Name_Type;
         begin

            --  Caller_ALI name may not match RCI_Spec name, because its
            --  name is based on the body source filename. With gnat.adc,
            --  spec and body may have different base names. The caller
            --  stub generation uses the spec source file and GNAT
            --  generates an object file which name is based on the source
            --  file name. In this case, the expected object file and the
            --  generated object file mismatch.

            Spec_ALI := Dir (Caller_Dir, Strip_Suffix (RCI_Spec) & ALI_Suffix);
            Spec_Obj := Dir (Caller_Dir, Strip_Suffix (RCI_Spec) & Obj_Suffix);
            Compile_RCI_Caller (Full_RCI_Spec, Spec_Obj);

            --  Rename the files when mismatch (see above).
            if Spec_ALI /= Caller_ALI then
               Copy_With_File_Stamp (Spec_ALI, Caller_ALI);
               Copy_With_File_Stamp (Spec_Obj, Caller_Object);
            end if;
         end;
      elsif not Quiet_Output then
         Message ("   ", Unit_Name, " caller stubs is up to date");
      end if;

      --  Do we need to generate the receiver stub and its ali.
      Obsolete := False;
      if not Obsolete and then not Is_Regular_File (Receiver_Object) then
         if Verbose_Mode then
            Write_Missing_File (Receiver_Object);
         end if;
         Obsolete := True;

      elsif not Obsolete
        and then Stamp (Full_RCI_Body) > Stamp (Receiver_Object) then
         if Verbose_Mode then
            Write_Stamp_Comparison (Full_RCI_Body, Receiver_Object);
         end if;
         Obsolete := True;
      end if;

      if not Obsolete and then not Is_Regular_File (Receiver_ALI) then
         if Verbose_Mode then
            Write_Missing_File (Receiver_ALI);
         end if;
         Obsolete := True;

      elsif not Obsolete
        and then Stamp (Full_RCI_Body) > Stamp (Receiver_ALI) then
         if Verbose_Mode then
            Write_Stamp_Comparison (Full_RCI_Body, Receiver_ALI);
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
         if not Quiet_Output then
            Message
              ("building ", Unit_Name, " receiver stubs from ", Full_RCI_Body);
         end if;

         Compile_RCI_Receiver (Full_RCI_Body, Receiver_Object);

      elsif not Quiet_Output then
         Message ("   ", Unit_Name, " receiver stubs is up to date");
      end if;

   end Create_Stub;

   -----------------
   -- Delete_Stub --
   -----------------

   procedure Delete_Stub (Source_Dir, Base_Name  : in File_Name_Type) is
      ALI_Src : File_Name_Type := Dir (Source_Dir, Base_Name & ALI_Suffix);
      Obj_Src : File_Name_Type := Dir (Source_Dir, Base_Name & Obj_Suffix);
   begin

      if Is_Regular_File (ALI_Src) then
         Delete (ALI_Src);
      end if;

      if Is_Regular_File (Obj_Src) then
         Delete (Obj_Src);
      end if;

   end Delete_Stub;

   -----------------
   -- Dwrite_Line --
   -----------------

   procedure Dwrite_Line
     (FD  : in File_Descriptor;
      Ind : in Int;
      S1  : in String;
      N1  : in Name_Id := No_Name;
      S2  : in String  := "";
      N2  : in Name_Id := No_Name;
      S3  : in String  := "";
      N3  : in Name_Id := No_Name) is
   begin
      for I in 1 .. Ind loop
         Dwrite_Str (FD, "   ", Stdout);
      end loop;
      Dwrite_Str  (FD, S1, Stdout);
      Dwrite_Name (FD, N1, Stdout);
      Dwrite_Str  (FD, S2, Stdout);
      Dwrite_Name (FD, N2, Stdout);
      Dwrite_Str  (FD, S3, Stdout);
      Dwrite_Name (FD, N3, Stdout);
      Dwrite_Eol  (FD, Stdout);
   end Dwrite_Line;

   ------------------------
   -- Dwrite_With_Clause --
   ------------------------

   procedure Dwrite_With_Clause
     (File       : in File_Descriptor;
      Unit       : in String;
      Child      : in Name_Id := No_Name;
      Use_Clause : in Boolean := True) is
   begin
      Name_Len := 0;
      Add_Str_To_Name_Buffer ("with ");
      Add_Str_To_Name_Buffer (Unit);
      if Child /= No_Name then
         if Unit /= "" then
            Add_Char_To_Name_Buffer ('.');
         end if;
         Get_Name_String_And_Append (Child);
      end if;
      if Use_Clause then
         Add_Str_To_Name_Buffer ("; use ");
         Add_Str_To_Name_Buffer (Unit);
         if Child /= No_Name then
            if Unit /= "" then
               Add_Char_To_Name_Buffer ('.');
            end if;
            Get_Name_String_And_Append (Child);
         end if;
      end if;
      Add_Str_To_Name_Buffer (";");
      Dwrite_Str (File, Name_Buffer (1 .. Name_Len), Stdout);
      Dwrite_Eol (File, Stdout);
   end Dwrite_With_Clause;

   -----------------------------
   -- Mark_Units_On_Partition --
   -----------------------------

   procedure Mark_Units_On_Partition
     (PID : in PID_Type;
      Lib : in ALI_Id) is

      Current_ALI : ALI_Id;
      Continue    : Boolean;

   begin

      if Debug_Mode then
         Message ("mark ali file ", ALIs.Table (Lib).Afile);
      end if;

      --  Mark this unit to avoid infinite recursive search.
      for I in ALIs.Table (Lib).First_Unit ..
               ALIs.Table (Lib).Last_Unit loop
         Set_PID (Unit.Table (I).Uname, PID);
      end loop;

      for I in ALIs.Table (Lib).First_Unit ..
               ALIs.Table (Lib).Last_Unit loop
         for J in Unit.Table (I).First_With ..
                  Unit.Table (I).Last_With loop

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

                  --  Look for a RCI units
                  if Unit.Table (K).RCI then

                     --  This one is a caller (mapped on a different
                     --  partition). Note that an unit name key is
                     --  a partition id if unit is RCI.
                     if Get_PID (Unit.Table (K).Uname) /= PID then
                        Get_Name_String (Unit.Table (K).Uname);
                        Set_PID (Unit.Table (K).Uname, PID);
                     end if;

                     --  No need to search deeper. This unit is not
                     --  on this partition.
                     Continue := False;
                     exit;

                  elsif Get_PID (Unit.Table (K).Uname) = PID then

                     --  No need to search deeper. Already done.
                     Continue := False;
                     exit;

                  end if;
               end loop;

               --  This unit is not a RCI unit or has not been scanned.
               if Continue then
                  Mark_Units_On_Partition (PID, Current_ALI);
               end if;
            end if;

         end loop;
      end loop;

   end Mark_Units_On_Partition;

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
         Message ("check stamps for ", Partition);
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
         Message ("load C => ", No_Name, String (Conf_Stamp1));
         Message ("find C => ", No_Name, String (Conf_Stamp2));
      end if;

      --  Compare this file stamp with the current executable file stamp.
      if Conf_Stamp1 /= Conf_Stamp2 then
         if Verbose_Mode then
            Message ("configuration file modified for partition ", Partition);
         end if;
         return True;
      end if;

      --  Load new line (Unix, DOS or Windows).
      if Buffer (Ptr) /= Ascii.LF and then Buffer (Ptr) = Ascii.CR then
         return True;
      end if;

      if Buffer (Ptr) = Ascii.CR then
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
         Message ("load E => ", No_Name, String (Exec_Stamp1));
         Message ("read E => ", No_Name, String (Exec_Stamp2));
      end if;

      --  Compare this file stamp with the current configuration file stamp.
      if Exec_Stamp1 /= Exec_Stamp2 then
         if Verbose_Mode then
            Message ("executable file modified for partition ", Partition);
         end if;
         return True;
      end if;

      --  Load new line.
      if Buffer (Ptr) /= Ascii.LF then
         return True;
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
         Message ("load O => ", No_Name, String (Obj_Stamp1));
         Message ("find O => ", No_Name, String (Obj_Stamp2));
      end if;

      --  Compare this object stamp with the current object file stamp.
      if Obj_Stamp1 /= Obj_Stamp2 then
         if Verbose_Mode then
            Message ("most recent object modified for partition ", Partition);
         end if;
         return True;
      end if;

      return False;
   end Rebuild_Partition;

end XE_Stubs;
