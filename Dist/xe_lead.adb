------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--                              X E _ L E A D                               --
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

with Namet;       use Namet;
with Osint;       use Osint;
with Output;      use Output;
with XE;          use XE;
with XE_Back;     use XE_Back;
with XE_Defs;     use XE_Defs;
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
     (Partition : in PID_Type) is
   begin
      if Default_Protocol_Name = No_Name then
         Write_Str  (FD, "BOOT_SERVER=tcp://`hostname`:5555");
         Write_Eol  (FD);
      else
         Write_Str  (FD, "BOOT_SERVER=");
         Write_Name (FD, Default_Protocol_Name);
         Write_Str  (FD, "://");
         Write_Name (FD, Default_Protocol_Data);
         Write_Eol  (FD);
      end if;
   end Set_Boot_Server;

   --------------
   -- Set_Host --
   --------------

   procedure Set_Host        (Partition : in PID_Type) is
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
         Write_Name (FD, Get_Host (Partition));
         Write_Eol  (FD);

      end if;
   end Set_Host;

   ------------------
   -- Set_Launcher --
   ------------------

   procedure Set_Launcher (Partition  : in PID_Type) is
   begin

      if Partition /= Main_Partition then
         Write_Str  (FD, Get_Rsh_Command);
         Write_Str  (FD, " $");
         Write_Name (FD, Partitions.Table (Partition).Name);
         Write_Str  (FD, "_HOST -n """);
      end if;

      Write_Name (FD, Get_Absolute_Exec (Partition));

      Write_Str  (FD, " --boot_server $BOOT_SERVER");
      Write_Name (FD, Get_Command_Line (Partition));
      if Partition /= Main_Partition then
         Write_Str (FD, " --detach --slave &""");
      end if;

      Write_Eol (FD);
   end Set_Launcher;

begin

   if Default_Starter /= None_Import and then not Quiet_Output then
      Write_Program_Name;
      Write_Str  (": generating starter ");
      Write_Name (Main_Subprogram);
      Write_Eol;
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

         for Partition in Partitions.First .. Partitions.Last loop
            if Partition /= Main_Partition then
               Set_Host (Partition => Partition);
            end if;
         end loop;

         Set_Boot_Server (Main_Partition);
         for Partition in Partitions.First .. Partitions.Last loop
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

         for PID in Partitions.First .. Partitions.Last loop
            if PID = Main_Partition then
               declare
                  PName : constant Partition_Name_Type :=
                    Partitions.Table (PID).Name;
                  Dir   : Storage_Dir_Name_Type;
               begin
                  Dir := Partitions.Table (PID).Storage_Dir;
                  if Dir = No_Storage_Dir then
                     Dir := Default_Storage_Dir;
                  end if;
                  if Dir = No_Storage_Dir then
                     Copy_With_File_Stamp
                       (Source         => PName,
                        Target         => Main_Subprogram,
                        Maybe_Symbolic => True);
                  else
                     Copy_With_File_Stamp
                       (Source         => Dir & Dir_Sep_Id & PName,
                        Target         => Main_Subprogram,
                        Maybe_Symbolic => True);
                  end if;
                  exit;
               end;
            end if;
         end loop;

      when None_Import => null;

   end case;

end XE_Lead;
