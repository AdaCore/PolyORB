------------------------------------------------------------------------------
--                                                                          --
--                          GNATDIST COMPONENTS                             --
--                                                                          --
--                            X E _ S T U B S                               --
--                                                                          --
--                                B o d y                                   --
--                                                                          --
--                           $Revision$                              --
--                                                                          --
--           Copyright (C) 1996 Free Software Foundation, Inc.              --
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
--              GNATDIST is maintained by ACT Europe.                       --
--            (email:distribution@act-europe.gnat.com).                     --
--                                                                          --
------------------------------------------------------------------------------

with Fname;            use Fname;
with Osint;            use Osint;
with Namet;            use Namet;
with Output;           use Output;
with ALI;              use ALI;
with Types;            use Types;
with XE_Defs;          use XE_Defs;
with XE_Utils;         use XE_Utils;
with XE;               use XE;
with GNAT.Os_Lib;      use GNAT.Os_Lib;
with Unchecked_Deallocation;

procedure XE_Stubs is

   Separator : Character renames GNAT.Os_Lib.Directory_Separator;

   procedure Deallocate is new Unchecked_Deallocation (String, String_Access);

   --  Local subprograms

   procedure Mark_RCI_Callers
     (PID : PID_Type;
      ALI : ALI_Id);
   --  Starting from an ali file, search though all the dependency
   --  chain to mark RCI callers. This is specially useful to
   --  know the version checks to perform.

   procedure Create_Main_Unit (PID : in PID_Type);
   --  Create a procedure which "withes" all the RCI receivers
   --  of the partition and insert the main procedure if needed.

   procedure Copy_Stub (Source_Dir, Target_Dir, Base_Name : in File_Name_Type);
   --  Copy all the stub files (base_name.*) from a source directory to
   --  a target directory. The suffixes used are .adb .o .ali.

   procedure Delete_Stub (Source_Dir, Base_Name : in File_Name_Type);
   --  Delete all the stub files (base_name.*) from a source directory. The
   --  suffixes used are .adb .o .ali.

   procedure Build_Stub (Base_Name : in File_Name_Type;
                         Spec_Only : in Boolean);
   --  Create the caller stub and the receiver stub for a RCI unit.

   procedure Build_Elaboration_File (PID : in PID_Type);
   --  Build the elaboration unit for the given partition.

   procedure Update_Switch (S : in out String_Access);
   --  For a given '-I' switch (or equivalent -L -a*), update it
   --  if it is a relative path and add ../.. at the beginning.

   procedure Dwrite_Str (File   : in File_Descriptor;
                         Line   : in String;
                         Stdout : in Boolean := Building_Script)
     renames Write_Str;
   --  Changed default parameter.

   procedure Dwrite_Name (File   : in File_Descriptor;
                          Name   : in Name_Id;
                          Stdout : in Boolean := Building_Script)
     renames Write_Name;
   --  Changed default parameter.

   procedure Dwrite_Eol (File   : in File_Descriptor;
                         Stdout : in Boolean := Building_Script)
     renames Write_Eol;
   --  Changed default parameter.

   ------------------------
   -- Write_Caller_Withs --
   ------------------------

   procedure Mark_RCI_Callers
     (PID : PID_Type;
      ALI : ALI_Id) is

      Current_ALI : ALI_Id;
      Continue    : Boolean;

   begin

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
                     --  partition. Note that an unit name key is
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
                  Mark_RCI_Callers (PID, Current_ALI);
               end if;
            end if;

         end loop;
      end loop;

   end Mark_RCI_Callers;

   ----------------------
   -- Create_Main_Unit --
   ----------------------

   procedure Create_Main_Unit (PID : in PID_Type) is

      PName : constant Partition_Name_Type := Partitions.Table (PID).Name;
      UID   : CUID_Type;
      FD    : File_Descriptor;
      Fname : File_Name_Type := DSA_Dir & Dir_Sep_Id &
                                PName & Dir_Sep_Id &
                                Configuration & ADB_Suffix;
      Host  : Name_Id;
      Main  : Name_Id;

   begin

      if not Is_Regular_File (Fname) then
         if Verbose_Mode then
            Write_Program_Name;
            Write_Str (": ");
            Write_Name (Fname);
            Write_Str (" does not exist");
            Write_Eol;
         end if;
      elsif Most_Recent_Stamp > Source_File_Stamp (Fname) then
         if Verbose_Mode then
            Write_Program_Name;
            Write_Str (": ");
            Write_Name (Configuration_File);
            Write_Stamp (Configuration_File);
            Write_Str (" is more recent than ");
            Write_Name (Fname);
            Write_Stamp (Fname);
            Write_Eol;
         end if;
      else
         return;
      end if;

      if not Is_Directory (DSA_Dir & Dir_Sep_Id & PName) then
         Create_Dir (DSA_Dir & Dir_Sep_Id & PName);
      end if;

      if Building_Script then
         Write_Str  (Standout, "cat >");
         Write_Name (Standout, Fname);
         Write_Str  (Standout, " <<__EOF__");
         Write_Eol  (Standout);
      end if;

      Create (FD, Fname);

      --  Force the RCI receivers to be present on the partition.
      Dwrite_Eol (FD);
      Dwrite_Str (FD, "--  RCI receiver and non-RCI units");
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

      UID := Partitions.Table (PID).First_Unit;
      while UID /= Null_CUID loop
         Mark_RCI_Callers (PID, CUnit.Table (UID).My_ALI);
         UID := CUnit.Table (UID).Next;
      end loop;

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
      Dwrite_Str (FD, "with system.partition_interface;");
      Dwrite_Eol (FD);
      Dwrite_Str (FD, "with ada.exceptions;");
      Dwrite_Eol (FD);

      --  XXXXX: debug.
      Dwrite_Str (FD, "with system.io;");
      Dwrite_Eol (FD);

      if PID = Main_Partition and then Starter_Method = Ada_Starter then
         Dwrite_Str (FD, "with system.garlic.remote;");
         Dwrite_Eol (FD);
         Dwrite_Str (FD, "with system.garlic.options;");
         Dwrite_Eol (FD);
      end if;

      Dwrite_Str  (FD, "procedure ");
      Dwrite_Name (FD, Configuration);
      Dwrite_Str  (FD, " is");
      Dwrite_Eol  (FD);

      if PID = Main_Partition then
         Dwrite_Str (FD, "   what    : ada.exceptions.exception_id;");
         Dwrite_Eol (FD);
         Dwrite_Str (FD, "   message : system.garlic.heart.string_ptr;");
         Dwrite_Eol (FD);
      end if;

      Dwrite_Str  (FD, "begin");
      Dwrite_Eol  (FD);

      if PID = Main_Partition then
         if Starter_Method = Ada_Starter and
            Partitions.First /= Partitions.Last then
            Dwrite_Str (FD, "   if not system.garlic.options");
            Dwrite_Str (FD, ".get_nolaunch then");
            Dwrite_Eol (FD);
            for Partition in Partitions.First .. Partitions.Last loop
               if Partition /= Main_Partition then
                  Dwrite_Str  (FD, "      system.garlic.remote.full_launch");
                  Dwrite_Eol  (FD);
                  Host := Get_Host (Partition);
                  if Host = No_Name then
                     Dwrite_Str  (FD, "         (host            => ");
                     Dwrite_Str  (FD, "system.garlic.remote.get_host (""");
                     Dwrite_Name (FD, Partitions.Table (Partition) .Name);
                     Dwrite_Str  (FD, """),");
                  else
                     Dwrite_Str  (FD, "         (host            => ");
                     Dwrite_Name (FD, Get_Host (Partition));
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

      if Version_Checks then

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
                 (FD, "          ""Versions differ for partition """"");
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
            Main := Default_Main;
         end if;
         if Main /= No_Name then
            Dwrite_Str  (FD, "   ");
            Dwrite_Name (FD, Main);
            Dwrite_Str  (FD, ";");
            Dwrite_Eol  (FD);
         end if;
      end if;

      Dwrite_Str  (FD, "end ");
      Dwrite_Name (FD, Configuration);
      Dwrite_Str  (FD, ";");
      Dwrite_Eol  (FD);

      if Building_Script then
         Write_Str (Standout, "__EOF__");
         Write_Eol (Standout);
      end if;

      Close (FD);

      if not Quiet_Output then
         Write_Program_Name;
         Write_Str  (": building ");
         Write_Name (PName);
         Write_Str  (" main procedure");
         Write_Eol;
      end if;

   end Create_Main_Unit;

   ---------------
   -- Copy_Stub --
   ---------------

   procedure Copy_Stub (Source_Dir, Target_Dir, Base_Name  : in Name_Id) is
      ALI_Src : File_Name_Type
        := Source_Dir & Dir_Sep_Id & Base_Name & ALI_Suffix;
      ALI_Tgt : File_Name_Type
        := Target_Dir & Dir_Sep_Id & Base_Name & ALI_Suffix;
      Obj_Src : File_Name_Type
        :=  Source_Dir & Dir_Sep_Id & Base_Name & Obj_Suffix;
      Obj_Tgt : File_Name_Type
        := Target_Dir & Dir_Sep_Id & Base_Name & Obj_Suffix;
      ADB_Src : File_Name_Type
        := Source_Dir & Dir_Sep_Id & Base_Name & ADB_Suffix;
      ADB_Tgt : File_Name_Type
        := Target_Dir & Dir_Sep_Id & Base_Name & ADB_Suffix;
   begin

      --  Copy the stubs from source directory to the target directory.

      if not Is_Regular_File (ALI_Src) then
         Write_Program_Name;
         Write_Str  (": ");
         Write_Name (ALI_Src);
         Write_Str  (" not found");
         Write_Eol;
         raise Fatal_Error;
      else
         Copy_With_File_Stamp (ALI_Src, ALI_Tgt);
      end if;

      if not Is_Regular_File (Obj_Src) then
         Write_Program_Name;
         Write_Str  (": ");
         Write_Name (Obj_Src);
         Write_Str  (" not found");
         Write_Eol;
         raise Fatal_Error;
      else
         Copy_With_File_Stamp (Obj_Src, Obj_Tgt);
      end if;

      if not Is_Regular_File (ADB_Src) then
         Write_Program_Name;
         Write_Str  (": ");
         Write_Name (ADB_Src);
         Write_Str  (" not found");
         Write_Eol;
         raise Fatal_Error;
      else
         Copy_With_File_Stamp (ADB_Src, ADB_Tgt);
      end if;

   end Copy_Stub;

   -----------------
   -- Delete_Stub --
   -----------------

   procedure Delete_Stub (Source_Dir, Base_Name  : in Name_Id) is
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

   ----------------
   -- Build_Stub --
   ----------------

   procedure Build_Stub (Base_Name : in Name_Id;
                         Spec_Only : in Boolean) is

      Obsolete        : Boolean := False;
      Full_RCI_Spec   : Name_Id;
      Full_RCI_Body   : Name_Id;
      Full_ALI_File   : Name_Id;
      RCI_Spec        : Name_Id;
      RCI_Body        : Name_Id;
      Caller_Body     : Name_Id;
      Caller_Object   : Name_Id;
      Caller_ALI      : Name_Id;
      Receiver_Body   : Name_Id;
      Receiver_Object : Name_Id;
      Receiver_ALI    : Name_Id;

   begin

      RCI_Spec        := Base_Name & ADS_Suffix;
      RCI_Body        := Base_Name & ADB_Suffix;
      Full_RCI_Spec   := Full_Source_Name (RCI_Spec);
      Full_RCI_Body   := Full_Source_Name (RCI_Body);
      Caller_Body     := Caller_Dir & Dir_Sep_Id & Base_Name & ADB_Suffix;
      Receiver_Body   := Receiver_Dir & Dir_Sep_Id & Base_Name & ADB_Suffix;
      Caller_Object   := Caller_Dir & Dir_Sep_Id & Base_Name & Obj_Suffix;
      Receiver_Object := Receiver_Dir & Dir_Sep_Id & Base_Name & Obj_Suffix;
      Caller_ALI      := Caller_Dir & Dir_Sep_Id & Base_Name & ALI_Suffix;
      Receiver_ALI    := Receiver_Dir & Dir_Sep_Id & Base_Name & ALI_Suffix;
      Full_ALI_File   := Base_Name & ALI_Suffix;

      --  Do we need to regenerate the caller stub and its ali.
      if not Obsolete and then not Is_Regular_File (Caller_Body) then
         if Verbose_Mode then
            Write_Program_Name;
            Write_Str (": ");
            Write_Name (Caller_Body);
            Write_Str (" does not exist");
            Write_Eol;
         end if;
         Obsolete := True;
      elsif not Obsolete and then More_Recent (Full_RCI_Spec, Caller_Body) then
         if Verbose_Mode then
            Write_Program_Name;
            Write_Str (": ");
            Write_Name (Full_RCI_Spec);
            Write_Stamp (Full_RCI_Spec);
            Write_Str (" is more recent than ");
            Write_Name (Caller_Body);
            Write_Stamp (Caller_Body);
            Write_Eol;
         end if;
         Obsolete := True;
      end if;

      if not Obsolete and then not Is_Regular_File (Caller_Object) then
         if Verbose_Mode then
            Write_Program_Name;
            Write_Str (": ");
            Write_Name (Caller_Object);
            Write_Str (" does not exist");
            Write_Eol;
         end if;
         Obsolete := True;
      elsif not Obsolete and then More_Recent (Caller_Body, Caller_Object) then
         if Verbose_Mode then
            Write_Program_Name;
            Write_Str (": ");
            Write_Name (Caller_Body);
            Write_Stamp (Caller_Body);
            Write_Str (" is more recent than ");
            Write_Name (Caller_Object);
            Write_Stamp (Caller_Object);
            Write_Eol;
         end if;
         Obsolete := True;
      end if;

      if not Obsolete and then not Is_Regular_File (Caller_ALI) then
         if Verbose_Mode then
            Write_Program_Name;
            Write_Str (": ");
            Write_Name (Caller_ALI);
            Write_Str (" does not exist");
            Write_Eol;
         end if;
         Obsolete := True;
      elsif not Obsolete and then More_Recent (Caller_Body, Caller_ALI) then
         if Verbose_Mode then
            Write_Program_Name;
            Write_Str (": ");
            Write_Name (Caller_Body);
            Write_Stamp (Caller_Body);
            Write_Str (" is more recent than ");
            Write_Name (Caller_ALI);
            Write_Stamp (Caller_ALI);
            Write_Eol;
         end if;
         Obsolete := True;
      end if;

      if not Obsolete and then More_Recent (Full_ALI_File, Caller_ALI) then
         if Verbose_Mode then
            Write_Program_Name;
            Write_Str (": ");
            Write_Name (Full_ALI_File);
            Write_Stamp (Full_ALI_File);
            Write_Str (" is more recent than ");
            Write_Name (Caller_ALI);
            Write_Stamp (Caller_ALI);
            Write_Eol;
         end if;
         Obsolete := True;
      end if;

      if Obsolete then

         if not Quiet_Output then
            Write_Program_Name;
            Write_Str  (": building ");
            Write_Name (Caller_Body);
            Write_Str  (" from ");
            Write_Name (Full_RCI_Spec);
            Write_Eol;
         end if;

         Change_Dir (Caller_Dir);
         Build_RCI_Caller
           (RCI_Body, G_Parent_Dir & Dir_Sep_Id & Full_RCI_Spec);
         Compile_RCI_Caller (RCI_Body);
         Change_Dir (G_Parent_Dir);

      elsif not Quiet_Output then
         Write_Program_Name;
         Write_Str  (":    ");
         Write_Name (Caller_Body);
         Write_Str  (" caller stub is up to date");
         Write_Eol;
      end if;

      --  If no RCI body is available, use RCI spec.
      if Spec_Only then
         Full_RCI_Body := Full_RCI_Spec;
      end if;

      --  Do we need to generate the receiver stub and its ali.
      Obsolete := False;

      if not Obsolete and then not Is_Regular_File (Receiver_Body) then
         if Verbose_Mode then
            Write_Program_Name;
            Write_Str (": ");
            Write_Name (Receiver_Body);
            Write_Str (" does not exist");
            Write_Eol;
         end if;
         Obsolete := True;
      elsif not Obsolete and then
         More_Recent (Full_RCI_Body, Receiver_Body) then
         if Verbose_Mode then
            Write_Program_Name;
            Write_Str (": ");
            Write_Name (Full_RCI_Body);
            Write_Stamp (Full_RCI_Body);
            Write_Str (" is more recent than ");
            Write_Name (Receiver_Body);
            Write_Stamp (Receiver_Body);
            Write_Eol;
         end if;
         Obsolete := True;
      end if;
      if not Obsolete and then not Is_Regular_File (Receiver_Object) then
         if Verbose_Mode then
            Write_Program_Name;
            Write_Str (": ");
            Write_Name (Receiver_Object);
            Write_Str (" does not exist");
            Write_Eol;
         end if;
         Obsolete := True;
      elsif not Obsolete and then
         More_Recent (Receiver_Body, Receiver_Object) then
         if Verbose_Mode then
            Write_Program_Name;
            Write_Str (": ");
            Write_Name (Receiver_Body);
            Write_Stamp (Receiver_Body);
            Write_Str (" is more recent than ");
            Write_Name (Receiver_Object);
            Write_Stamp (Receiver_Object);
            Write_Eol;
         end if;
         Obsolete := True;
      end if;
      if not Obsolete and then not Is_Regular_File (Receiver_ALI) then
         if Verbose_Mode then
            Write_Program_Name;
            Write_Str (": ");
            Write_Name (Receiver_ALI);
            Write_Str (" does not exist");
            Write_Eol;
         end if;
         Obsolete := True;
      elsif not Obsolete and then
         More_Recent (Receiver_Body, Receiver_ALI) then
         if Verbose_Mode then
            Write_Program_Name;
            Write_Str (": ");
            Write_Name (Receiver_Body);
            Write_Stamp (Receiver_Body);
            Write_Str (" is more recent than ");
            Write_Name (Receiver_ALI);
            Write_Stamp (Receiver_ALI);
            Write_Eol;
         end if;
         Obsolete := True;
      end if;
      if not Obsolete and then
         More_Recent (Full_ALI_File, Receiver_ALI) then
         if Verbose_Mode then
            Write_Program_Name;
            Write_Str (": ");
            Write_Name (Full_ALI_File);
            Write_Stamp (Full_ALI_File);
            Write_Str (" is more recent than ");
            Write_Name (Receiver_ALI);
            Write_Stamp (Receiver_ALI);
            Write_Eol;
         end if;
         Obsolete := True;
      end if;

      if Obsolete then

         if not Quiet_Output then
            Write_Program_Name;
            Write_Str  (": building ");
            Write_Name (Receiver_Body);
            Write_Str  (" from ");
            Write_Name (Full_RCI_Body);
            Write_Eol;
         end if;

         Change_Dir (Receiver_Dir);
         Build_RCI_Receiver
           (RCI_Body, G_Parent_Dir & Dir_Sep_Id & Full_RCI_Body);
         Compile_RCI_Receiver (RCI_Body);
         Change_Dir (G_Parent_Dir);

      elsif not Quiet_Output then
         Write_Program_Name;
         Write_Str  (":    ");
         Write_Name (Receiver_Body);
         Write_Str  (" receiver stub is up to date");
         Write_Eol;
      end if;

   end Build_Stub;

   -----------------------------
   --  Build_Elaboration_File --
   -----------------------------

   procedure Build_Elaboration_File (PID : in PID_Type) is

      PName : constant Partition_Name_Type := Partitions.Table (PID) .Name;
      FName : constant File_Name_Type      := Elaboration_Name & ADB_Suffix;
      FD    : File_Descriptor;

   begin

      if not Is_Directory (DSA_Dir & Dir_Sep_Id & PName) then
         Create_Dir (DSA_Dir & Dir_Sep_Id & PName);
      end if;

      Change_Dir (DSA_Dir & Dir_Sep_Id & PName);

      if Building_Script then
         Write_Str  (Standout, "cat >");
         Write_Name (Standout, FName);
         Write_Str  (Standout, " <<__EOF__");
         Write_Eol  (Standout);
      end if;

      Create (FD, FName);

      --  Header.

      Dwrite_Str  (FD, "with System.Garlic.Options;");
      Dwrite_Eol  (FD);
      Dwrite_Str  (FD, "use System.Garlic.Options;");
      Dwrite_Eol  (FD);
      Dwrite_Str  (FD, "package body ");
      Dwrite_Name (FD, Elaboration_Full_Name);
      Dwrite_Str  (FD, " is");
      Dwrite_Eol  (FD);
      Dwrite_Str  (FD, "begin");
      Dwrite_Eol  (FD);

      --  If the partition holds the main unit, then it cannot be slave.
      --  Otherwise, it is.

      if PID = Main_Partition then
         Dwrite_Str (FD, "   Set_Is_Slave (False);");
      else
         Dwrite_Str (FD, "   Set_Is_Slave (True);");
      end if;
      Dwrite_Eol (FD);

      --  The partition should not terminate.

      if Get_Permanent (PID) then
         Dwrite_Str (FD, "   Set_Permanent (True);");
      else
         Dwrite_Str (FD, "   Set_Permanent (True);");
      end if;
      Dwrite_Eol (FD);

      --  If a protocol has been specified, then use it (with its data
      --  if present).

      if Protocol_Name /= No_Name then
         Dwrite_Str  (FD, "   Set_Boot_Server (""");
         Dwrite_Name (FD, Protocol_Name);
         if Protocol_Data /= No_Name then
            Dwrite_Str  (FD, "://");
            Dwrite_Name (FD, Protocol_Data);
         end if;
         Dwrite_Str  (FD, """);");
         Dwrite_Eol  (FD);
      end if;

      --  Footer.
      Dwrite_Str  (FD, "end ");
      Dwrite_Name (FD, Elaboration_Full_Name);
      Dwrite_Str  (FD, ";");
      Dwrite_Eol  (FD);

      if Building_Script then
         Write_Str (Standout, "__EOF__");
         Write_Eol (Standout);
      end if;

      Compile_Regular_File (FName);

      Change_Dir (G_Parent_Dir);

      Close (FD);

   end Build_Elaboration_File;

   procedure Update_Switch (S : in out String_Access) is

      procedure Update (S : in out String_Access; I : Natural);

      procedure Update (S : in out String_Access; I : Natural) is

         T : String (S'First .. S'Last + 6);
         N : Natural := I - 1;

      begin
         T (S'First .. N) := S (S'First .. N);
         N := N + 1;
         T (N .. N + 5) := "../../";
         N := N + 6;
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

begin

   if not Is_Directory (Caller_Dir) then
      Create_Dir (Caller_Dir);
   end if;

   if not Is_Directory (Receiver_Dir) then
      Create_Dir (Receiver_Dir);
   end if;

   --  At this point, everything is performed in dsa/<dir>. We update
   --  all the relative paths (-I and -L).

   for S in Gcc_Switches.First .. Gcc_Switches.Last loop
      Update_Switch (Gcc_Switches.Table (S));
   end loop;

   for S in Linker_Switches.First .. Linker_Switches.Last loop
      Update_Switch (Linker_Switches.Table (S));
   end loop;

   for S in Binder_Switches.First .. Binder_Switches.Last loop
      Update_Switch (Binder_Switches.Table (S));
   end loop;

   --  Generate all the stubs (bodies, objects and alis). At this level,
   --  we ensure that all conf. units are ada units.
   for CUID in CUnit.First .. CUnit.Last loop
      if Unit.Table (CUnit.Table (CUID).My_Unit).RCI then
         if Verbose_Mode then
            Write_Program_Name;
            Write_Str (": building ");
            Write_Name (CUnit.Table (CUID).CUname);
            Write_Str (" stubs");
            Write_Eol;
         end if;
         Build_Stub
           (Get_Unit_Sfile (CUnit.Table (CUID).My_Unit),
            Unit.Table (CUnit.Table (CUID).My_Unit).Utype = Is_Spec_Only);
      end if;
   end loop;

   --  Create and fill partition directories.
   for PID in Partitions.First .. Partitions.Last loop

      if Partitions.Table (PID).To_Build then

         if not Is_Directory
           (DSA_Dir & Dir_Sep_Id & Partitions.Table (PID).Name) then
            Create_Dir (DSA_Dir & Dir_Sep_Id & Partitions.Table (PID).Name);
         end if;

         Create_Main_Unit (PID);

         Build_Elaboration_File (PID);

         --  Copy RCI receiver stubs when this unit has been assigned on
         --  PID partition. RCI caller stubs are not needed because GNATDIST
         --  add the caller directory in its include path.
         for UID in CUnit.First .. CUnit.Last loop
            if Unit.Table (CUnit.Table (UID).My_Unit).RCI then
               if CUnit.Table (UID).Partition = PID then
                  Copy_Stub
                    (Receiver_Dir,
                     DSA_Dir & Dir_Sep_Id & Partitions.Table (PID).Name,
                     Get_Unit_Sfile (CUnit.Table (UID).My_Unit));
               else
                  Delete_Stub
                    (DSA_Dir & Dir_Sep_Id & Partitions.Table (PID).Name,
                     Get_Unit_Sfile (CUnit.Table (UID).My_Unit));
               end if;
            end if;
         end loop;

         --  Bind and link each partition.
         declare

            Exec_Name : Name_Id := Partitions.Table (PID).Name;
            Dir_Name  : Name_Id;

         begin

            if Partitions.Table (PID).Storage_Dir = No_Storage_Dir then
               Dir_Name := Default_Storage_Dir;
            else
               Dir_Name := Partitions.Table (PID).Storage_Dir;
            end if;

            --  Bind and link are performed in the partition directory
            --  dsa/<partition_name>. We compute relative output.
            if Dir_Name  = No_Storage_Dir then

               Exec_Name := G_Parent_Dir & Dir_Sep_Id & Exec_Name;

            else

               declare

                  Str : String (1 .. Strlen (Dir_Name));

               begin

                  Get_Name_String (Dir_Name);
                  Str := Name_Buffer (1 .. Name_Len);

                  --  Is it a relative storage directory ?
                  if Str (1) /= Separator then

                     --  Create dir before changing directory,
                     if not Is_Directory (Dir_Name) then
                        Create_Dir (Dir_Name);
                     end if;
                     Dir_Name := G_Parent_Dir & Dir_Sep_Id & Dir_Name;

                  elsif not Is_Directory (Dir_Name) then

                     Create_Dir (Dir_Name);

                  end if;

                  Exec_Name := Dir_Name & Dir_Sep_Id & Exec_Name;

               end;

            end if;

            if not Quiet_Output then
               Write_Program_Name;
               Write_Str  (": building partition ");
               Write_Name (Partitions.Table (PID).Name);
               Write_Eol;
            end if;

            Build_Partition (Partitions.Table (PID).Name, Exec_Name);

         end;
      end if;

   end loop;

end XE_Stubs;




