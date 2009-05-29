------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                              X E _ M A I N                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 1995-2009, Free Software Foundation, Inc.          --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
-- not, write to the Free Software Foundation, 51 Franklin Street, Fifth    --
-- Floor, Boston, MA 02111-1301, USA.                                       --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Exceptions;          use Ada.Exceptions;

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

   function NS (N : Name_Id) return String renames XE_Names.Get_Name_String;

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

   XE_Back.Register_Storages (Backend);

   --  Look for the configuration file that is Next_Main_Source or
   --  Next_Main_Source + ".cfg" if the former does not exist.

   Configuration_File_Name := Next_Main_Source;

   if not Is_Regular_File (Configuration_File_Name) then
      --  Not found: try to add ".cfg"

      Get_Name_String (Configuration_File_Name);
      Add_Str_To_Name_Buffer (Cfg_Suffix);
      Configuration_File_Name := Name_Find;

      if not Is_Regular_File (Configuration_File_Name) then
         raise Fatal_Error
           with "file " & NS (Quote (Configuration_File_Name)) & " not found";
      end if;
   end if;

   --  Parse the configuration file and fill the different tables

   Parse;
   Frontend;

   --  Configuration name and configuration file name do not match (case
   --  insensitively, to mimic the way project files work)

   Get_Name_String (Strip_Directory (Configuration_File_Name));
   Name_Len := Name_Len - Cfg_Suffix'Length;
   if To_Lower (Get_Name_String (Configuration)) /=
      To_Lower (Get_Name_String (Name_Find))
   then
      raise Fatal_Error
        with "configuration file name should be "
          & NS (Quote (Configuration & Cfg_Suffix_Id));
   end if;

   --  Look for a partition list on the command line. Only those partitions are
   --  built. If no partition list is given, then generate all of them.

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
            raise Fatal_Error
              with "unknown partition " & NS (Quote (Next_Main_Source));
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

   --  Check consistency once we know which partitions to build. Some parts of
   --  configuration may be missing because we partially build the distributed
   --  system.

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

   when E : Fatal_Error =>
      Message (Ada.Exceptions.Exception_Message (E));
      Message ("*** can't continue");
      Exit_Program (E_Fatal);

   when Compilation_Error =>
      Exit_Program (E_Fatal);

   when others =>
      Remove_All_Temp_Files;
      raise;
end XE_Main;
