------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--                             X E _ B U I L D                              --
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

with GNAT.OS_Lib;      use GNAT.OS_Lib;
with Make;             use Make;
with Namet;            use Namet;
with Osint;            use Osint;
with Types;            use Types;
with XE;               use XE;
with XE_Back;          use XE_Back;
with XE_Check;         use XE_Check;
with XE_Parse;         use XE_Parse;
with XE_Scan;          use XE_Scan;
with XE_Stdcnf;        use XE_Stdcnf;
with XE_Stubs;         use XE_Stubs;
with XE_Utils;         use XE_Utils;

with XE_Lead;
with XE_Usage;

procedure XE_Build is

   Suffix    : constant String := ".cfg";

begin

   XE_Utils.Initialize;

   if Number_Of_Files = 0 then
      XE_Usage;

   else

      --  Initialization of differents modules.

      XE.Initialize;
      XE_Scan.Initialize;
      XE_Parse.Initialize;
      XE_Stdcnf.Initialize;
      XE_Back.Initialize;
      XE_Check.Initialize;

      --  Look for the configuration file :
      --     Next_Main_Source or Next_Main_Source + ".cfg" if the latter
      --     does not exist.

      declare
         N : Name_Id := Next_Main_Source;
         L : Integer;
         S : Integer := Suffix'Length;
      begin

         Get_Name_String (N);
         L := Name_Len;

         --  Remove suffix if needed.

         if L > S and then Name_Buffer (L - S + 1 .. L) = Suffix then
            L := L - S;
            N := Name_Find;
         else
            Name_Buffer (L + 1 .. L + S) := Suffix;
            Name_Len := L + S;
            N := Name_Find;
         end if;

         --  If the filename is not already correct.
         if not Is_Regular_File (N) then

            Message ("", Quote (N), "not found");
            Exit_Program (E_Fatal);
         else
            Configuration_File := N;
         end if;
      end;

      if Building_Script then
         Write_Str (Standout, "#! /bin/sh");
         Write_Eol (Standout);
      end if;

      Parse;
      Back;

      --  The configuration name and the configuration file name don't match.

      Get_Name_String (Configuration_File);
      Name_Len := Name_Len - 4;
      if Configuration /= Name_Find then
         if not Quiet_Mode then
            Message ("configuration file name should be",
                     Quote (Configuration & Str_To_Id (Suffix)));
         end if;
         raise Fatal_Error;
      end if;

      --  Look for a partition list on the command line. Only those
      --  partitions are going to be generated. If no partition list is
      --  given, then generate all of them.

      if More_Source_Files then
         for P in Partitions.First + 1 .. Partitions.Last loop
            Partitions.Table (P).To_Build := False;
         end loop;
         while More_Source_Files loop

            --  At this level, the key associated to a partition name is
            --  its table index.

            declare
               N : Name_Id  := Next_Main_Source;
               P : PID_Type := Get_PID (N);
            begin
               if P = Null_PID then
                  Message ("", N, " is not a partition");
                  raise Fatal_Error;
               end if;
               Partitions.Table (P).To_Build := True;
            end;

         end loop;

         --  Do we build the full configuration?
         for P in Partitions.First + 1 .. Partitions.Last loop
            if not Partitions.Table (P).To_Build then
               Partitions.Table (Default_Partition).To_Build := False;
               exit;
            end if;
         end loop;
      end if;

      --  Check consistency now that we know the partitions to build

      Check;

      if not Quiet_Mode then
         Show_Configuration;
      end if;

      XE_Stubs.Build;
      XE_Lead;

      Exit_Program (E_Success);

   end if;

exception
   when Scanning_Error =>
      Message ("*** scanning failed");
      Exit_Program (E_Fatal);
   when Parsing_Error =>
      Message ("*** parsing failed");
      Exit_Program (E_Fatal);
   when Partitioning_Error =>
      Message ("*** partitionning failed");
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
   when Compilation_Failed =>
      Exit_Program (E_Fatal);
   when others =>
      Message ("*** unknown error");
      raise;  --  hope GNAT will output its name

end XE_Build;
