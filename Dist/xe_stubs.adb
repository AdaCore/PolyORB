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

with Namet;            use Namet;
with Osint;            use Osint;
with Output;           use Output;
with XE;               use XE;
with XE_Back;          use XE_Back;
with XE_Defs;          use XE_Defs;
with XE_Utils;         use XE_Utils;

with Unchecked_Deallocation;

package body XE_Stubs is

   Root_Dir     : String_Access;
   Root_Dir_Len : Natural;

   Directory    : File_Name_Type;
   --  Where partition files are stored.

   Executable : File_Name_Type;

   procedure Build_Partition
     (Partition  : in PID_Type;
      Executable : in File_Name_Type);
   --  Generates the partition ada main subprogram (generation
   --  of Ada code, compilation, bind and link).

   procedure Build_Stub (A : in ALI_Id);
   --  Create the caller stub and the receiver stub for a RCI unit.

   procedure Copy_Stub
     (Source_Dir, Target_Dir : in File_Name_Type; A : in ALI_Id);
   --  Copy all the stub files (base_name.*) from a source directory to
   --  a target directory. The suffixes used are .adb .o .ali.

   procedure Create_Elaboration_File (PID : in PID_Type);
   --  Create the elaboration unit for the given partition.

   procedure Create_Partition_Main_File (PID : in PID_Type);
   --  Create a procedure which "withes" all the RCI receivers
   --  of the partition and insert the main procedure if needed.

   procedure Deallocate is new Unchecked_Deallocation (String, String_Access);

   procedure Delete_Stub (Source_Dir, Base_Name : in File_Name_Type);
   --  Delete all the stub files (base_name.*) from a source directory. The
   --  suffixes used are .adb .o .ali.

   procedure Dwrite_Eol (File   : in File_Descriptor;
                         Stdout : in Boolean := Building_Script)
     renames Write_Eol;
   --  Changed default parameter.

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

   procedure Update_Switch (S : in out String_Access);
   --  For a given '-I' switch (or equivalent -L -a*), update it
   --  if it is a relative path and add ../../.. at the beginning.

   -----------
   -- Build --
   -----------

   procedure Build is

      CUID  : CUID_Type;

   begin

      Get_Name_String (Original_Dir & Dir_Sep_Id);
      Root_Dir_Len := Name_Len;
      Root_Dir := new String'(Name_Buffer (1 .. Root_Dir_Len));

      if not Is_Directory (Caller_Dir) then
         Create_Dir (Caller_Dir);
      end if;

      if not Is_Directory (Receiver_Dir) then
         Create_Dir (Receiver_Dir);
      end if;

      --  Generate all the stubs (bodies, objects and alis). At this level,
      --  we ensure that all conf. units are ada units.
      for CUID in CUnit.First .. CUnit.Last loop
         if Unit.Table (CUnit.Table (CUID).My_Unit).RCI
           and then not Unit.Table (CUnit.Table (CUID).My_Unit).Is_Generic then
            if Verbose_Mode then
               Message ("building ", CUnit.Table (CUID).CUname, " stubs");
            end if;
            Build_Stub (CUnit.Table (CUID).My_ALI);
         end if;
      end loop;

      --  Create and fill partition directories.
      for PID in Partitions.First + 1 .. Partitions.Last loop

         if Partitions.Table (PID).To_Build then

            Directory := Get_Partition_Dir (PID);

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

                  if not Unit.Table (U).RCI then

                     Most_Recent_Stamp
                       (PID, ALIs.Table (Unit.Table (U).My_ALI).Afile);

                  elsif Get_PID (U_To_N (Unit.Table (U).Uname)) = PID then

                     --  Copy RCI receiver stubs when this unit has been
                     --  assigned on PID partition. RCI caller stubs are
                     --  not needed because GNATDIST add the caller directory
                     --  in its include path.

                     Copy_Stub
                       (Receiver_Dir,
                        Directory,
                        Unit.Table (U).My_ALI);

                     Most_Recent_Stamp
                       (PID, Directory & Dir_Sep_Id &
                             Receiver_Dir & Dir_Sep_Id &
                             Lib_File_Name (Unit.Table (U).Sfile));

                     Set_Light_PCS (PID, False);

                  else

                     Delete_Stub
                       (Directory,
                        Unit.Table (U).Sfile);

                     Most_Recent_Stamp
                       (PID, Directory & Dir_Sep_Id &
                             Caller_Dir & Dir_Sep_Id &
                             Lib_File_Name (Unit.Table (U).Sfile));

                     if Unit.Table (U).Has_RACW_Type then
                        Set_Light_PCS (PID, False);
                     end if;

                  end if;

               end if;

            end loop;

            Create_Partition_Main_File (PID);
            Create_Elaboration_File (PID);

            --  Bind and link each partition.

            Executable := Partitions.Table (PID).Name;

            if Partitions.Table (PID).Storage_Dir = No_Storage_Dir then
               Directory := Partitions.Table (Default_Partition).Storage_Dir;
            else
               Directory := Partitions.Table (PID).Storage_Dir;
            end if;

            if Directory  /= No_Storage_Dir then
               if not Is_Directory (Directory) then
                  Create_Dir (Directory);
               end if;
               Executable := Directory & Dir_Sep_Id & Executable;
            end if;

            Build_Partition (PID, Executable);

         elsif Verbose_Mode then

            Message ("no need to build ", Partitions.Table (PID).Name);

         end if;

      end loop;

   end Build;

   ---------------------
   -- Build_Partition --
   ---------------------

   procedure Build_Partition
     (Partition  : in PID_Type;
      Executable : in File_Name_Type) is

      Part_Name  : Name_Id := Partitions.Table (Partition).Name;
      Exec_File  : File_Name_Type := Executable;

      Directory  : File_Name_Type
        := DSA_Dir & Dir_Sep_Id & Configuration & Dir_Sep_Id & Part_Name;

      Stamp_File : File_Name_Type
        := Directory & Dir_Sep_Id & Build_Stamp_File;

   begin

      if not Is_Regular_File (Executable) then
         if Verbose_Mode then
            Message ("", Executable, " doesn't exist");
         end if;

      elsif not Is_Regular_File (Stamp_File) then
         if Verbose_Mode then
            Message ("", Stamp_File, " doesn't exist");
         end if;

      elsif Partitions.Table (Partition).Most_Recent > Executable then
         if Verbose_Mode then
            Write_Stamp_Comparison
              (Partitions.Table (Partition).Most_Recent, Executable);
         end if;

      elsif Executable > Stamp_File then
         if Verbose_Mode then
            Write_Stamp_Comparison (Executable, Stamp_File);
         end if;

      else
         return;
      end if;

      if not Quiet_Output then
         Message ("building partition ", Part_Name);
      end if;

      Produce_Partition_Executable (Part_Name, Configuration, Exec_File);

   end Build_Partition;

   ----------------
   -- Build_Stub --
   ----------------

   procedure Build_Stub (A : in ALI_Id) is

      Obsolete        : Boolean;
      Full_RCI_Spec   : File_Name_Type;
      Full_RCI_Body   : File_Name_Type;
      Full_ALI_File   : File_Name_Type;
      RCI_Spec        : File_Name_Type := No_File;
      RCI_Body        : File_Name_Type := No_File;
      Caller_Object   : File_Name_Type;
      Caller_ALI      : File_Name_Type;
      Receiver_Object : File_Name_Type;
      Receiver_ALI    : File_Name_Type;
      Unit_Name       : Unit_Name_Type;

   begin

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

      Full_ALI_File   := ALIs.Table (A).Ofile_Full_Name;

      --  Caller and receiver object filenames can be different because
      --  of gnat.adc.

      Caller_Object   :=
        Caller_Dir & Strip_Suffix (Dir_Sep_Id & RCI_Spec) & Obj_Suffix;
      Receiver_Object :=
        Receiver_Dir & Strip_Suffix (Dir_Sep_Id & RCI_Body) & Obj_Suffix;

      Caller_ALI      := Strip_Suffix (Caller_Object) & ALI_Suffix;
      Receiver_ALI    := Strip_Suffix (Receiver_Object) & ALI_Suffix;

      --  Do we need to regenerate the caller stub and its ali.
      Obsolete := False;
      if not Obsolete and then not Is_Regular_File (Caller_Object) then
         if Verbose_Mode then
            Write_Missing_File (Caller_Object);
         end if;
         Obsolete := True;
      elsif not Obsolete and then Full_RCI_Spec > Caller_Object then
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
      elsif not Obsolete and then Full_RCI_Spec > Caller_ALI then
         if Verbose_Mode then
            Write_Stamp_Comparison (Full_RCI_Spec, Caller_ALI);
         end if;
         Obsolete := True;
      end if;

      if not Obsolete and then Full_ALI_File > Caller_ALI then
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

         Compile_RCI_Caller (Full_RCI_Spec, Caller_Object);

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
      elsif not Obsolete and then Full_RCI_Body > Receiver_Object then
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
      elsif not Obsolete and then Full_RCI_Body > Receiver_ALI then
         if Verbose_Mode then
            Write_Stamp_Comparison (Full_RCI_Body, Receiver_ALI);
         end if;
         Obsolete := True;
      end if;
      if not Obsolete and then Full_ALI_File > Receiver_ALI then
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

   end Build_Stub;

   ---------------
   -- Copy_Stub --
   ---------------

   procedure Copy_Stub
     (Source_Dir, Target_Dir : in File_Name_Type; A : in ALI_Id) is

      ALI_Src, ALI_Tgt, Obj_Src, Obj_Tgt, ADB_Src, ADB_Tgt : File_Name_Type;

   begin

      for U in ALIs.Table (A).First_Unit .. ALIs.Table (A).Last_Unit loop
         case Unit.Table (U).Utype is
            when Is_Body =>
               ADB_Src := Source_Dir & Dir_Sep_Id & Unit.Table (U).Sfile;
               ADB_Tgt := Target_Dir & Dir_Sep_Id & Unit.Table (U).Sfile;
            when Is_Spec_Only =>
               ADB_Src := Source_Dir & Dir_Sep_Id & Unit.Table (U).Sfile;
               ADB_Tgt := Target_Dir & Dir_Sep_Id & Unit.Table (U).Sfile;
            when others =>
               null;
         end case;
      end loop;

      ALI_Src := Strip_Suffix (ADB_Src) & ALI_Suffix;
      ALI_Tgt := Strip_Suffix (ADB_Tgt) & ALI_Suffix;

      Obj_Src := Strip_Suffix (ADB_Src) & Obj_Suffix;
      Obj_Tgt := Strip_Suffix (ADB_Tgt) & Obj_Suffix;

      --  Copy the stubs from source directory to the target directory.

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
      Most_Recent : File_Name_Type;

      Task_Pool   : Task_Pool_Type;

      CID : CID_Type;
      FD  : File_Descriptor;

   begin

      Partition   := Partitions.Table (PID) .Name;
      Elaboration := Directory & Dir_Sep_Id & Elaboration_File & ADB_Suffix;
      Most_Recent := Partitions.Table (PID).Most_Recent;

      if not Is_Regular_File (Elaboration) then
         if Verbose_Mode then
            Write_Missing_File (Elaboration);
         end if;
      elsif Most_Recent > Elaboration then
         if Verbose_Mode then
            Write_Stamp_Comparison (Most_Recent, Elaboration);
         end if;
      else
         return;
      end if;

      if Building_Script then
         Write_Str  (Standout, "cat >");
         Write_Name (Standout, Elaboration);
         Write_Str  (Standout, " <<__EOF__");
         Write_Eol  (Standout);
      end if;

      Create (FD, Elaboration);

      --  Header.

      Dwrite_Str  (FD, "with System.Garlic.Options;");
      Dwrite_Eol  (FD);
      Dwrite_Str  (FD, "with System.Garlic.Heart;");
      Dwrite_Eol  (FD);
      Dwrite_Str  (FD, "with System.Garlic.Filters;");
      Dwrite_Eol  (FD);

      if Default_Registration_Filter /= No_Filter_Name then
         Dwrite_Str  (FD, "--  Specific registration filters");
         Dwrite_Eol  (FD);
         Dwrite_Str  (FD, "with System.Garlic.Filters.");
         Dwrite_Name (FD, Default_Registration_Filter);
         Dwrite_Str  (FD, ";");
         Dwrite_Eol  (FD);
      end if;

      if Get_Filter (PID) /= No_Filter_Name then
         Dwrite_Str  (FD, "--  Specific partition filters");
         Dwrite_Eol  (FD);
         Dwrite_Str  (FD, "with System.Garlic.Filters.");
         Dwrite_Name (FD, Get_Filter (PID));
         Dwrite_Str  (FD, ";");
         Dwrite_Eol  (FD);
      end if;

      if Partitions.Table (PID).First_Channel /= Null_CID then
         Dwrite_Str  (FD, "--  Specific channel filters");
         Dwrite_Eol  (FD);
         CID := Partitions.Table (PID).First_Channel;
         while CID /= Null_CID loop
            if Get_Filter (CID) /= No_Filter_Name then
               Dwrite_Str  (FD, "with System.Garlic.Filters.");
               Dwrite_Name (FD, Get_Filter (CID));
               Dwrite_Str  (FD, ";");
               Dwrite_Eol  (FD);
            end if;
            if Channels.Table (CID).Lower.My_Partition = PID then
               CID := Channels.Table (CID).Lower.Next_Channel;
            else
               CID := Channels.Table (CID).Upper.Next_Channel;
            end if;
         end loop;
      end if;

      Dwrite_Str  (FD, "use System.Garlic.Options;");
      Dwrite_Eol  (FD);
      Dwrite_Str  (FD, "use System.Garlic.Heart;");
      Dwrite_Eol  (FD);
      Dwrite_Str  (FD, "use System.Garlic.Filters;");
      Dwrite_Eol  (FD);
      Dwrite_Str  (FD, "package body ");
      Dwrite_Name (FD, Elaboration_Name);
      Dwrite_Str  (FD, " is");
      Dwrite_Eol  (FD);
      Dwrite_Str  (FD, "   procedure Initialize is");
      Dwrite_Eol  (FD);
      Dwrite_Str  (FD, "   begin");
      Dwrite_Eol  (FD);

      --  If the partition holds the main unit, then it cannot be slave.
      --  Otherwise, it is.

      if PID = Main_Partition then
         Dwrite_Str (FD, "      Set_Is_Slave (False);");
      else
         Dwrite_Str (FD, "      Set_Is_Slave (True);");
      end if;
      Dwrite_Eol (FD);

      --  The partition should not terminate.

      case Get_Termination (PID) is
         when Local_Termination =>
            Dwrite_Str (FD, "      Set_Termination (Local_Termination);");
         when Global_Termination =>
            Dwrite_Str (FD, "      Set_Termination (Global_Termination);");
         when Deferred_Termination =>
            Dwrite_Str (FD, "      Set_Termination (Deferred_Termination);");
         when Unknown_Termination =>
            null;
      end case;

      Dwrite_Eol (FD);

      --  If a protocol has been specified, then use it (with its data
      --  if present).

      if Default_Protocol_Name /= No_Name then
         Dwrite_Str  (FD, "      Set_Boot_Server (""");
         Dwrite_Name (FD, Default_Protocol_Name);
         if Default_Protocol_Data /= No_Name then
            Dwrite_Str  (FD, "://");
            Dwrite_Name (FD, Default_Protocol_Data);
         end if;
         Dwrite_Str  (FD, """);");
         Dwrite_Eol  (FD);
      end if;

      Task_Pool := Get_Task_Pool (PID);
      if Task_Pool /= No_Task_Pool then
         Dwrite_Str  (FD, "      Task_Pool_Low_Bound := ");
         Dwrite_Name (FD, Task_Pool (1));
         Dwrite_Str  (FD, ";");
         Dwrite_Eol  (FD);
         Dwrite_Str  (FD, "      Task_Pool_High_Bound := ");
         Dwrite_Name (FD, Task_Pool (2));
         Dwrite_Str  (FD, ";");
         Dwrite_Eol  (FD);
         Dwrite_Str  (FD, "      Task_Pool_Max_Bound := ");
         Dwrite_Name (FD, Task_Pool (3));
         Dwrite_Str  (FD, ";");
         Dwrite_Eol  (FD);
      end if;

      Dwrite_Str     (FD, "      Set_Partition_Name (""");
      Dwrite_Name    (FD, Partition);
      Dwrite_Str     (FD, """);");
      Dwrite_Eol     (FD);

      if Default_Registration_Filter /= No_Filter_Name then
         Dwrite_Str     (FD, "      Set_Registration_Filter (""");
         Dwrite_Name    (FD, Default_Registration_Filter);
         Dwrite_Str     (FD, """);");
         Dwrite_Eol     (FD);
      end if;

      if Partitions.Table (Default_Partition).Filter /= No_Filter_Name then
         Dwrite_Str     (FD, "      Set_Default_Filter (""");
         Dwrite_Name    (FD, Partitions.Table (Default_Partition).Filter);
         Dwrite_Str     (FD, """);");
         Dwrite_Eol     (FD);
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
                  Dwrite_Str  (FD, "      Set_Channel_Filter (""");
                  Dwrite_Name (FD, Partitions.Table (Peer).Name);
                  Dwrite_Str  (FD, """, """);
                  Dwrite_Name (FD, Filter);
                  Dwrite_Str  (FD, """);");
                  Dwrite_Eol  (FD);
               end if;
            end loop;
         end;
      end if;

      if Get_Light_PCS (PID) then
         Dwrite_Str  (FD, "      Has_RCI_Pkg_Or_RACW_Var := False;");
         Dwrite_Eol  (FD);
      end if;

      --  Footer.
      Dwrite_Str  (FD, "   end Initialize;");
      Dwrite_Eol  (FD);
      Dwrite_Str  (FD, "end ");
      Dwrite_Name (FD, Elaboration_Name);
      Dwrite_Str  (FD, ";");
      Dwrite_Eol  (FD);

      if Building_Script then
         Write_Str (Standout, "__EOF__");
         Write_Eol (Standout);
      end if;

      Close (FD);

      Most_Recent_Stamp (PID, Elaboration);

   end Create_Elaboration_File;

   --------------------------------
   -- Create_Partition_Main_File --
   --------------------------------

   procedure Create_Partition_Main_File (PID : in PID_Type) is

      Partition   : Partition_Name_Type;
      Main_File   : File_Name_Type;
      Most_Recent : File_Name_Type;

      UID   : CUID_Type;
      Host  : Name_Id;
      Main  : Name_Id;

      FD : File_Descriptor;

   begin

      Partition   := Partitions.Table (PID).Name;
      Main_File   := Directory & Dir_Sep_Id & Partition_Main_File & ADB_Suffix;
      Most_Recent := Partitions.Table (PID).Most_Recent;

      if not Is_Regular_File (Main_File) then
         if Verbose_Mode then
            Write_Missing_File (Main_File);
         end if;
      elsif Most_Recent > Main_File then
         if Verbose_Mode then
            Write_Stamp_Comparison (Most_Recent, Main_File);
         end if;
      else
         return;
      end if;

      if Building_Script then
         Write_Str  (Standout, "cat >");
         Write_Name (Standout, Main_File);
         Write_Str  (Standout, " <<__EOF__");
         Write_Eol  (Standout);
      end if;

      Create (FD, Main_File);

      --  Force the RCI receivers to be present on the partition.
      Dwrite_Eol (FD);
      Dwrite_Str (FD, "--  RCI receivers and non-RCI units");
      Dwrite_Eol (FD);
      UID := Partitions.Table (PID).First_Unit;
      while UID /= Null_CUID loop
         Set_PID (Unit.Table (CUnit.Table (UID).My_Unit).Uname,
                  CUnit.Table (UID).Partition);
         Dwrite_Str  (FD, "with ");
         Dwrite_Name (FD, CUnit.Table (UID).CUname);
         Dwrite_Str  (FD, ";");
         Dwrite_Eol  (FD);
         UID := CUnit.Table (UID).Next;
      end loop;

      --  Need the RCI callers to compare their version with the
      --  receiver version.
      Dwrite_Eol (FD);
      Dwrite_Str (FD, "--  RCI caller units");
      Dwrite_Eol (FD);

      --  First pass to 'with' the RCI callers.
      for U in CUnit.First .. CUnit.Last loop
         if Unit.Table (CUnit.Table (U).My_Unit).RCI then
            if CUnit.Table (U).Partition /= PID and then
              Get_PID (Unit.Table (CUnit.Table (U).My_Unit).Uname) = PID then
               Dwrite_Str  (FD, "with ");
               Dwrite_Name (FD, CUnit.Table (U).CUname);
               Dwrite_Str  (FD, "; ");
               Dwrite_Eol  (FD);
            end if;
         else
            Set_PID (Unit.Table (CUnit.Table (U).My_Unit).Uname, Null_PID);
         end if;
      end loop;

      Dwrite_Eol (FD);
      Dwrite_Str (FD, "--  System");
      Dwrite_Eol (FD);
      Dwrite_Str (FD, "with system.garlic.startup;");
      Dwrite_Eol (FD);
      Dwrite_Str (FD, "pragma elaborate_all (system.garlic.startup);");
      Dwrite_Eol (FD);
      Dwrite_Str (FD, "with system.garlic.heart;");
      Dwrite_Eol (FD);
      Dwrite_Str (FD, "with system.garlic.termination;");
      Dwrite_Eol (FD);
      Dwrite_Str (FD, "with system.garlic.types;");
      Dwrite_Eol (FD);
      Dwrite_Str (FD, "with system.partition_interface;");
      Dwrite_Eol (FD);
      Dwrite_Str (FD, "with ada.exceptions;");
      Dwrite_Eol (FD);

      Dwrite_Str (FD, "with system.io;");
      Dwrite_Eol (FD);

      if PID = Main_Partition and then Default_Starter = Ada_Import then
         Dwrite_Str (FD, "with system.garlic.remote;");
         Dwrite_Eol (FD);
         Dwrite_Str (FD, "with system.garlic.options;");
         Dwrite_Eol (FD);
      end if;

      Dwrite_Str  (FD, "procedure ");
      Dwrite_Name (FD, Partition_Main_Name);
      Dwrite_Str  (FD, " is");
      Dwrite_Eol  (FD);

      if PID = Main_Partition then
         Dwrite_Str (FD, "   what    : ada.exceptions.exception_id;");
         Dwrite_Eol (FD);
         Dwrite_Str (FD, "   message : system.garlic.types.string_access;");
         Dwrite_Eol (FD);
      end if;

      Dwrite_Str  (FD, "begin");
      Dwrite_Eol  (FD);

      if PID = Main_Partition then
         if Default_Starter = Ada_Import and
            Partitions.First + 1 /= Partitions.Last then
            Dwrite_Str (FD, "   if not system.garlic.options");
            Dwrite_Str (FD, ".nolaunch then");
            Dwrite_Eol (FD);
            for Partition in Partitions.First + 1 .. Partitions.Last loop
               if Partition /= Main_Partition then
                  Dwrite_Str  (FD, "      system.garlic.remote.full_launch");
                  Dwrite_Eol  (FD);
                  Dwrite_Str  (FD, "         (launcher        => """);
                  Dwrite_Str  (FD, Get_Rsh_Command);
                  Dwrite_Str  (FD, """,");
                  Dwrite_Eol  (FD);
                  Host := Get_Host (Partition);
                  if Host = No_Name then
                     Dwrite_Str  (FD, "          host            => ");
                     Dwrite_Str  (FD, "system.garlic.remote.get_host (""");
                     Dwrite_Name (FD, Partitions.Table (Partition) .Name);
                     Dwrite_Str  (FD, """),");
                  else
                     Dwrite_Str  (FD, "          host            => ");
                     Dwrite_Name (FD, Host);
                     Dwrite_Str  (FD, ",");
                  end if;
                  Dwrite_Eol  (FD);
                  Dwrite_Str  (FD, "          executable_name => """);
                  Dwrite_Name (FD, Get_Absolute_Exec (Partition));
                  Dwrite_Name (FD, Get_Command_Line  (Partition));
                  Dwrite_Str  (FD, """,");
                  Dwrite_Eol  (FD);
                  Dwrite_Str  (FD, "          boot_server  => ");
                  Dwrite_Str  (FD, "system.garlic.heart.get_boot_server);");
                  Dwrite_Eol  (FD);
               end if;
            end loop;
            Dwrite_Str (FD, "   end if;");
            Dwrite_Eol (FD);
         end if;

      end if;

      Dwrite_Str (FD, "   system.garlic.heart.elaboration_is_terminated;");
      Dwrite_Eol (FD);

      if Default_Version_Check then

         --  Version consistency between receiver and caller.
         --  Checks perform on all the rci caller stubs.
         for U in CUnit.First .. CUnit.Last loop
            if Unit.Table (CUnit.Table (U).My_Unit).RCI and then
              CUnit.Table (U).Partition /= PID and then
              Get_PID (Unit.Table (CUnit.Table (U).My_Unit).Uname) = PID then
               Dwrite_Str  (FD, "   if ");
               Dwrite_Name (FD, CUnit.Table (U).CUname);
               Dwrite_Str  (FD, "'version /= ");
               Dwrite_Str  (FD, "system.partition_interface.");
               Dwrite_Eol  (FD);
               Dwrite_Str  (FD, "      ");
               Dwrite_Str  (FD, "get_active_version (""");
               Dwrite_Name (FD, CUnit.Table (U).CUname);
               Dwrite_Str  (FD, """) then");
               Dwrite_Eol  (FD);
               Dwrite_Str  (FD, "      system.garlic.heart.soft_shutdown;");
               Dwrite_Eol  (FD);
               Dwrite_Str  (FD, "      ada.exceptions.raise_exception");
               Dwrite_Eol  (FD);
               Dwrite_Str  (FD, "         (program_error'identity,");
               Dwrite_Eol  (FD);
               Dwrite_Str
                 (FD, "          ""Versions differ for RCI unit """"");
               Dwrite_Name (FD, CUnit.Table (U).CUname);
               Dwrite_Str  (FD, """"""");");
               Dwrite_Eol  (FD);
               Dwrite_Str  (FD, "   end if;");
               Dwrite_Eol  (FD);
               Set_PID (Unit.Table (CUnit.Table (U).My_Unit).Uname, Null_PID);
            end if;
         end loop;
      end if;

      if PID = Main_Partition then
         Dwrite_Str  (FD, "   select");
         Dwrite_Eol  (FD);
         Dwrite_Str  (FD, "      system.garlic.heart.fatal_error.occurred(");
         Dwrite_Str  (FD, "what, message);");
         Dwrite_Eol  (FD);
         Dwrite_Str  (FD, "      system.garlic.heart.soft_shutdown;");
         Dwrite_Eol  (FD);
         Dwrite_Str  (FD, "      ada.exceptions.raise_exception");
         Dwrite_Eol  (FD);
         Dwrite_Str  (FD, "         (what, message.all);");
         Dwrite_Eol  (FD);
         Dwrite_Str  (FD, "   then abort");
         Dwrite_Eol  (FD);
         Dwrite_Str  (FD, "      ");
         Dwrite_Name (FD, Main_Subprogram);
         Dwrite_Str  (FD, ";");
         Dwrite_Eol  (FD);
         Dwrite_Str  (FD, "   end select;");
         Dwrite_Eol  (FD);
      else
         Main := Partitions.Table (PID).Main_Subprogram;
         if Main = No_Main_Subprogram then
            Main := Partitions.Table (Default_Partition).Main_Subprogram;
         end if;
         if Main /= No_Name then
            Dwrite_Str  (FD, "   ");
            Dwrite_Name (FD, Main);
            Dwrite_Str  (FD, ";");
            Dwrite_Eol  (FD);
         end if;
      end if;

      for B in False .. True loop

         --  When we exit main subprogram, just terminate.
         if Get_Termination (PID) = Local_Termination then
            Dwrite_Str (FD, "   system.garlic.termination.local_termination;");
            Dwrite_Eol (FD);
         end if;

         exit when B;

         Dwrite_Str  (FD, "   exception when others =>");
         Dwrite_Eol  (FD);

      end loop;

      Dwrite_Str  (FD, "      raise;");
      Dwrite_Eol  (FD);
      Dwrite_Str  (FD, "end ");
      Dwrite_Name (FD, Partition_Main_Name);
      Dwrite_Str  (FD, ";");
      Dwrite_Eol  (FD);

      if Building_Script then
         Write_Str (Standout, "__EOF__");
         Write_Eol (Standout);
      end if;

      Close (FD);

      Most_Recent_Stamp (PID, Main_File);

   end Create_Partition_Main_File;

   -----------------
   -- Delete_Stub --
   -----------------

   procedure Delete_Stub (Source_Dir, Base_Name  : in File_Name_Type) is
      ALI_Src : File_Name_Type
        := Source_Dir & Dir_Sep_Id & Base_Name & ALI_Suffix;
      Obj_Src : File_Name_Type
        :=  Source_Dir & Dir_Sep_Id & Base_Name & Obj_Suffix;
      ADB_Src : File_Name_Type
        := Source_Dir & Dir_Sep_Id & Base_Name & ADB_Suffix;
   begin

      if Is_Regular_File (ALI_Src) then
         Delete (ALI_Src);
      end if;

      if Is_Regular_File (Obj_Src) then
         Delete (Obj_Src);
      end if;

      if Is_Regular_File (ADB_Src) then
         Delete (ADB_Src);
      end if;

   end Delete_Stub;

   -----------------------------
   -- Mark_Units_On_Partition --
   -----------------------------

   procedure Mark_Units_On_Partition
     (PID : in PID_Type;
      ALI : in ALI_Id) is

      Current_ALI : ALI_Id;
      Continue    : Boolean;

   begin

      if Debug_Mode then
         Message ("mark ali file ", ALIs.Table (ALI).Afile);
      end if;

      --  Mark this unit to avoid infinite recursive search.
      for I in ALIs.Table (ALI).First_Unit ..
               ALIs.Table (ALI).Last_Unit loop
         Set_PID (Unit.Table (I).Uname, PID);
      end loop;

      for I in ALIs.Table (ALI).First_Unit ..
               ALIs.Table (ALI).Last_Unit loop
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

   -------------------
   -- Update_Switch --
   -------------------

   procedure Update_Switch (S : in out String_Access) is

      procedure Update (S : in out String_Access; I : Natural);

      procedure Update (S : in out String_Access; I : Natural) is

         T : String (S'First .. S'Last + Root_Dir_Len);
         N : Natural := I - 1;

      begin
         T (S'First .. N) := S (S'First .. N);
         N := N + 1;
         T (N .. N + Root_Dir_Len) := Root_Dir.all;
         N := N + Root_Dir_Len;
         T (N .. T'Last) := S (I .. S'Last);
         Deallocate (S);
         S := new String'(T);
      end Update;

      N : Natural := S'First + 1;

   begin
      case S (N) is
         when 'a' =>
            case S (N + 1) is
               when 'L' | 'O' | 'I' =>
                  if S (N + 2) /= Separator then
                     Update (S, N + 2);
                  end if;
               when others =>
                  null;
            end case;
         when 'I' | 'L' | 'A' =>
            if S (N + 1) /= Separator and then
               S (N + 1) /= '-' then
               Update (S, N + 1);
            end if;
         when others =>
            null;
      end case;
   end Update_Switch;

end XE_Stubs;
