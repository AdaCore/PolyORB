------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--                              X E _ L E A D                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision$
--                                                                          --
--         Copyright (C) 1996-2001 Free Software Foundation, Inc.           --
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

with GNAT.OS_Lib; use GNAT.OS_Lib;
with Types;       use Types;
with XE;          use XE;
with XE_Back;     use XE_Back;
with XE_Utils;    use XE_Utils;

procedure XE_Lead is

   FD : File_Descriptor;

   procedure Set_Boot_Server (Partition : in PID_Type);

   procedure Set_Host        (Partition : in PID_Type);

   procedure Set_Launcher    (Partition : in PID_Type);

   ---------------------
   -- Set_Boot_Server --
   ---------------------

   procedure Set_Boot_Server
     (Partition : in PID_Type)
   is
      LID : LID_Type;

   begin
      if Def_Boot_Location_First = Null_LID then
         Write_Str  (FD, "BOOT_LOCATION=tcp://`hostname`:");
         Write_Str  (FD, "`echo 000$$ | sed 's,^.*\(...\),5\1,'`");
         Write_Eol  (FD);
      else
         LID := Partitions.Table (Partition).F_Net_Location;
         Write_Str  (FD, "BOOT_LOCATION='");
         loop
            Write_Name (FD, Locations.Table (LID).Major);
            Write_Str  (FD, "://");
            Write_Name (FD, Locations.Table (LID).Minor);
            LID := Locations.Table (LID).Next;
            exit when LID = Null_LID;
            Write_Str  (FD, " ");
         end loop;
         Write_Str (FD, "'");
         Write_Eol (FD);
      end if;
   end Set_Boot_Server;

   --------------
   -- Set_Host --
   --------------

   procedure Set_Host (Partition : in PID_Type) is
      Host : Name_Id := Get_Host (Partition);

   begin
      if Host = No_Name then
         Write_Str  (FD, "echo '");
         Write_Name (FD, Partitions.Table (Partition).Name);
         Write_Str  (FD, " host: '");
         Write_Eol  (FD);
         Write_Str  (FD, "read ");
         Write_Name (FD, Partitions.Table (Partition).Name);
         Write_Str  (FD, "_HOST");
         Write_Eol  (FD);

      else
         Write_Name (FD, Partitions.Table (Partition).Name);
         Write_Str  (FD, "_HOST=");
         Write_Name (FD, Host);
         Write_Eol  (FD);

      end if;
   end Set_Host;

   ------------------
   -- Set_Launcher --
   ------------------

   procedure Set_Launcher (Partition  : in PID_Type) is
      Ext_Quote : Character := '"';  -- "
      Int_Quote : Character := ''';  -- '

   begin

      --  For the main partition, the command should be
      --    "<pn>" --boot_location "<bs>" <cline>
      --  For other partitions, it should be
      --    <rshcmd> <host> <rshopts> "'<pn>' --detach ...
      --         ... --boot_location '<bs>' <cline> &" ...
      --         ... < /dev/null > /dev/null 2>&1

      if Partition = Main_Partition then
         Int_Quote := '"';  -- "
      end if;

      if Partition /= Main_Partition then
         Write_Name (FD, Get_Rsh_Command);
         Write_Str  (FD, " $");
         Write_Name (FD, Partitions.Table (Partition).Name);
         Write_Str  (FD, "_HOST ");
         Write_Name (FD, Get_Rsh_Options);
         Write_Str  (FD, ' ' & Ext_Quote);
      end if;

      --  Write_Str  (FD, (1 => Int_Quote));
      Write_Name (FD, Get_Absolute_Exec (Partition));
      --  Write_Str  (FD, (1 => Int_Quote));

      Write_Str  (FD, " --boot_location " &
                  Int_Quote & "$BOOT_LOCATION" & Int_Quote);
      Write_Name (FD, Get_Command_Line (Partition));
      if Partition /= Main_Partition then
         Write_Str  (FD, " --detach &" & Ext_Quote);
         Write_Str  (FD, " < /dev/null > /dev/null 2>&1");
      end if;

      Write_Eol (FD);

   end Set_Launcher;

begin
   if Default_Starter /= None_Import and then not Quiet_Mode then
      Message ("generating starter", Main_Subprogram);
   end if;

   case Default_Starter is

      when Shell_Import =>

         Unlink_File (Main_Subprogram);

         if Building_Script then
            FD := Standout;
            Write_Str  (FD, "cat >");
            Write_Name (FD, Main_Subprogram);
            Write_Str  (FD, " <<__EOF__");
            Write_Eol  (FD);
         else
            Create (FD, Main_Subprogram, True);
         end if;

         Write_Str (FD, "#! /bin/sh");
         Write_Eol (FD);
         Write_Str (FD, "PATH=/usr/ucb:${PATH}");
         Write_Eol (FD);

         for Partition in Partitions.First + 1 .. Partitions.Last loop
            if Partition /= Main_Partition then
               Set_Host (Partition => Partition);
            end if;
         end loop;

         Set_Boot_Server (Main_Partition);
         for Partition in Partitions.First + 1 .. Partitions.Last loop
            if Partition /= Main_Partition then
               Set_Launcher (Partition  => Partition);
            end if;
         end loop;
         Set_Launcher (Partition  => Main_Partition);

         if Building_Script then
            Write_Str (FD, "__EOF__");
            Write_Eol (FD);
         else
            Close (FD);
         end if;

      when Ada_Import =>

         for PID in Partitions.First + 1 .. Partitions.Last loop
            if PID = Main_Partition then
               declare
                  P               : Partition_Name_Type :=
                    Partitions.Table (PID).Name & Exe_Suffix;
                  Main_Executable : Main_Subprogram_Type :=
                    Main_Subprogram & Exe_Suffix;
                  D               : Directory_Name_Type;
               begin
                  D := Partitions.Table (PID).Directory;
                  if D = No_Directory then
                     D := Partitions.Table (Default_Partition).Directory;
                  end if;
                  if D = No_Directory then
                     Copy_With_File_Stamp
                       (Source         => P,
                        Target         => Main_Executable,
                        Maybe_Symbolic => True);
                  else
                     Copy_With_File_Stamp
                       (Source         => Dir (D, P),
                        Target         => Main_Executable,
                        Maybe_Symbolic => True);
                  end if;
                  exit;
               end;
            end if;
         end loop;

      when None_Import => null;

   end case;
end XE_Lead;
