------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--                             X E _ U S A G E                              --
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

with XE;             use XE;
with Osint;          use Osint;
with Output;         use Output;

procedure XE_Usage is

begin

   if Verbose_Mode then
      Write_Str ("GNATDIST @dsa_version@ / GNAT @gnat_version@");
      Write_Str (" Copyright 1996 Free Software Foundation, Inc.");
      Write_Eol;
   end if;
   Write_Str ("Usage: ");
   Write_Program_Name;
   Write_Str (" [options] name[.cfg] {[partition]}");
   Write_Str (" {[-cargs opts] [-bargs opts] [-largs opts]}");
   Write_Eol;
   Write_Eol;

   Write_Str ("  -M   Output commands to build application manually");
   Write_Eol;
   Write_Str ("  -n   No file stamp consistency check");
   Write_Eol;
   Write_Str ("  -q   Be quiet, do not display partitioning operations");
   Write_Eol;
   Write_Str ("  -v   Motivate all executed commands");
   Write_Eol;
   Write_Eol;

   Write_Str ("  name[.cfg]  Configuration file");
   Write_Eol;
   Write_Eol;

   Write_Str ("  -cargs opts      Arguments to be passed to the compiler");
   Write_Eol;
   Write_Str ("  -bargs opts      Arguments to be passed to the binder");
   Write_Eol;
   Write_Str ("  -largs opts      Arguments to be passed to the linker");
   Write_Eol;

   Exit_Program (E_Fatal);

end XE_Usage;
