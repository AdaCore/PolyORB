------------------------------------------------------------------------------
--                                                                          --
--                          GNATDIST COMPONENTS                             --
--                                                                          --
--                             X E _ B U I L D                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision$                              --
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
with Csets;
with ALI;              use ALI;
with Namet;            use Namet;
with Osint;            use Osint;
with Types;            use Types;
with Output;           use Output;
with XE_Usage;
with XE_Parse;         use XE_Parse;
with XE_Back;          use XE_Back;
with XE_Scan;          use XE_Scan;
with XE_Stubs;
with XE_Lead;
with XE_Check;         use XE_Check;
with XE_Utils;         use XE_Utils;
with GNAT.Os_Lib;      use GNAT.Os_Lib;
with XE;               use XE;
with Opt;
procedure XE_Build is

   Prefix    : constant String := ".cfg";
   File_Name : File_Name_Type;

begin

   Initialize (Make);
   Namet.Initialize;
   Csets.Initialize;

   if Building_Script then
      Verbose_Mode := False;
      Quiet_Output := True;
   end if;

   Opt.Check_Source_Files := False;
   Opt.All_Sources        := False;

   if Number_Of_Files = 0 then
      XE_Usage;

   else
      XE_Utils.Initialize;
      XE_Scan.Initialize;
      XE_Parse.Initialize;
      XE_Check.Initialize;

      File_Name := Next_Main_Source;
      Get_Name_String (File_Name);
      declare
         L1 : Natural := Name_Len;
         L2 : Natural := Name_Len + Prefix'Length;
         N1 : String (1 .. L1);
         N2 : String (1 .. L2);
      begin
         N1 := Name_Buffer (N1'Range);
         N2 (N1'Range) := N1;
         N2 (L1 + 1 .. L2) := Prefix;
         if not Is_Regular_File (N1) then
            if Is_Regular_File (N2) then
               Name_Len := L2;
               Name_Buffer (N2'Range) := N2;
               File_Name := Name_Find;
            else
               if L1 > Prefix'Length and then
                 N1 (L1 - Prefix'Length + 1 .. L1) = Prefix then
                  L1 := L1 - Prefix'Length;
               end if;
               Write_Program_Name;
               Write_Str (": ");
               Write_Str (N1 (1 .. L1));
               Write_Str ("[");
               Write_Str (Prefix);
               Write_Str ("] not found ");
               Write_Eol;
               Exit_Program (E_Fatal);
            end if;
         end if;
      end;

      Configuration_File := File_Name;

      Parse;
      Back;

      Get_Name_String (Configuration_File);
      Name_Len := Name_Len - 4;
      if Configuration /= Name_Find then
         if not Quiet_Output then
            Write_Program_Name;
            Write_Str (": Filename is ");
            Write_Name (Configuration_File);
            Write_Str (", but should be ");
            Write_Name (Configuration);
            Write_Str (".cfg");
            Write_Eol;
         end if;
         raise Fatal_Error;
      end if;

      Check;

      if not Quiet_Output then
         Show_Configuration;
      end if;

      if More_Source_Files then
         for P in Partitions.First .. Partitions.Last loop
            Partitions.Table (P).To_Build := False;
         end loop;
         while More_Source_Files loop
            declare
               N : Name_Id  := Next_Main_Source;
               P : PID_Type := Get_PID (N);
            begin
               if P = Null_PID then
                  Write_Program_Name;
                  Write_Name (N);
                  Write_Str (" is not a partition");
                  raise Fatal_Error;
               end if;
               Partitions.Table (P).To_Build := True;
            end;
         end loop;
      end if;

      XE_Stubs;

      XE_Lead;

      Exit_Program (E_Success);

   end if;

exception
   when Scanning_Error =>
      Write_Program_Name;
      Write_Str (": *** scanning failed");
      Write_Eol;
      Exit_Program (E_Fatal);
   when Parsing_Error =>
      Write_Program_Name;
      Write_Str (": *** parsing failed");
      Write_Eol;
      Exit_Program (E_Fatal);
   when Partitioning_Error =>
      Write_Program_Name;
      Write_Str (": *** partitionning failed");
      Write_Eol;
      Exit_Program (E_Fatal);
   when Usage_Error =>
      Write_Program_Name;
      Write_Str (": *** wrong argument(s)");
      Write_Eol;
      Exit_Program (E_Fatal);
   when Not_Yet_Implemented =>
      Write_Program_Name;
      Write_Str (": *** unimplemented feature");
      Write_Eol;
      Exit_Program (E_Fatal);
   when Fatal_Error =>
      Write_Program_Name;
      Write_Str (": *** can't continue");
      Write_Eol;
      Exit_Program (E_Fatal);
   when others =>
      Write_Program_Name;
      Write_Str (": *** unknown error");
      Write_Eol;
      raise;  --  hope GNAT will output its name

end XE_Build;
