------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--                              X E _ M A I N                               --
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

with XE;              use XE;
with XE_Back;         use XE_Back;
with XE_Back.GARLIC;  use XE_Back.GARLIC;
with XE_Back.PolyORB; use XE_Back.PolyORB;
with XE_Defs;         use XE_Defs;
with XE_Flags;        use XE_Flags;
with XE_Front;        use XE_Front;
with XE_IO;           use XE_IO;
with XE_Names;        use XE_Names;
with XE_Parse;        use XE_Parse;
with XE_Types;        use XE_Types;
with XE_Sem;          use XE_Sem;
with XE_Scan;         use XE_Scan;
with XE_Stdcnf;       use XE_Stdcnf;
with XE_Utils;        use XE_Utils;
with XE_Units;        use XE_Units;
with XE_Usage;

procedure XE_Main is
   Partition          : Partition_Id;
   Backend            : XE_Back.Backend_Access;

   Raised_Usage_Error : Boolean := False;
   --  If command line processing raises Usage_Error, we want to defer
   --  propagation until after calling Show_Dist_Args if in debug mode.

begin
   XE_Names.Initialize;
   begin
      XE_Utils.Initialize;
      Backend := XE_Back.Find_Backend (Get_PCS_Name);
      Set_PCS_Dist_Flags (Backend);
   exception
      when Usage_Error =>
         Raised_Usage_Error := True;
   end;

   if Debug_Mode then
      XE_Utils.Show_Dist_Args;
   end if;

   if Raised_Usage_Error then
      raise Usage_Error;
   end if;

   if XE_Utils.Number_Of_Files = 0 then
      XE_Usage;
      Exit_Program (E_Success);
   end if;

   XE.Initialize;
   XE_Scan.Initialize;
   XE_Parse.Initialize;
   XE_Stdcnf.Initialize;
   XE_Front.Initialize;

   --  Look for the configuration file that is Next_Main_Source or
   --  Next_Main_Source + ".cfg" if the latter does not exist.

   Configuration_File_Name := Next_Main_Source;

   if not Is_Regular_File (Configuration_File_Name) then
      Get_Name_String (Configuration_File_Name);
      Add_Str_To_Name_Buffer (Cfg_Suffix);
      Configuration_File_Name := Name_Find;

      if not Is_Regular_File (Configuration_File_Name) then
         Message ("file", Quote (Next_Main_Source), "not found");
         raise Fatal_Error;
      end if;

   elsif Name_Buffer (Name_Len - 3 .. Name_Len) /= Cfg_Suffix then
      if not Quiet_Mode then
         Message
           ("configuration file name should be",
            Quote (Strip_Suffix (Configuration_File_Name) & Cfg_Suffix_Id));
         raise Fatal_Error;
      end if;
   end if;

   --  Parse the configuration file and fill the different tables

   Parse;
   Frontend;

   --  Configuration name and configuration file name do not match

   Get_Name_String (Configuration_File_Name);
   Name_Len := Name_Len - Cfg_Suffix'Length;
   if not Quiet_Mode
     and then Configuration /= Name_Find
   then
      Message ("configuration file name should be",
               Quote (Configuration & Cfg_Suffix_Id));
      raise Fatal_Error;
   end if;

   --  Look for a partition list on the command line. Only those
   --  partitions are build. If no partition list is given, then
   --  generate all of them.

   if More_Source_Files then
      for J in Partitions.First + 1 .. Partitions.Last loop
         Partitions.Table (J).To_Build := False;
      end loop;

      --  As a result of the parsing, the name table info of a
      --  partition name contains its partition id. If there is no
      --  partition id attached to this name, then it is not a
      --  declared partition.

      while More_Source_Files loop
         Partition := Get_Partition_Id (Next_Main_Source);
         if Partition = No_Partition_Id then
            Message ("unknown partition", Quote (Next_Main_Source));
            raise Fatal_Error;
         end if;
         Partitions.Table (Partition).To_Build := True;
      end loop;

      --  Check whether we build the whole configuration

      for J in Partitions.First + 1 .. Partitions.Last loop
         if not Partitions.Table (J).To_Build then
            Partitions.Table (Default_Partition_Id).To_Build := False;
            exit;
         end if;
      end loop;
   end if;

   --  Check consistency once we know which partitions to build. Some
   --  parts of configuration may be missing because we partially
   --  build the distributed system.

   XE_Back.Initialize (Backend);
   Analyze;
   XE_Back.Run_Backend (Backend);

   --  Everything went fine

   Exit_Program (E_Success);

exception
   when Scanning_Error =>
      Message ("*** scanning failed");
      Exit_Program (E_Fatal);
   when Parsing_Error =>
      Message ("*** parsing failed");
      Exit_Program (E_Fatal);
   when Partitioning_Error =>
      Message ("*** partitioning failed");
      Exit_Program (E_Fatal);
   when Usage_Error =>
      Message ("*** wrong argument(s)");
      Exit_Program (E_Fatal);
   when Not_Yet_Implemented =>
      Message ("*** unimplemented feature");
      Exit_Program (E_Fatal);
   when Fatal_Error =>
      Message ("*** can't continue");
      Exit_Program (E_Fatal);
   when Compilation_Error =>
      Exit_Program (E_Fatal);
   when others =>
      Remove_All_Temp_Files;
      raise;
end XE_Main;
