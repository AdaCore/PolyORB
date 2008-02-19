------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                             X E _ U S A G E                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 1995-2008, Free Software Foundation, Inc.          --
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

with XE_IO;    use XE_IO;
with XE_Defs.Defaults;
with XE_Flags; use XE_Flags;

procedure XE_Usage is
   Version : constant String := "5.04w";
   Length  : Positive := Version'First;

begin
   if Verbose_Mode then
      while Length < Version'Last and then Version (Length + 1) /= ' ' loop
         Length := Length + 1;
      end loop;
      Write_Eol;
      Write_Str ("GNATDIST ");
      Write_Str (Version (Version'First .. Length));
      Write_Str (" / ");
      Write_Str (Version (Version'First .. Length));
      Write_Eol;
      Write_Str ("Copyright 1996-2005 Free Software Foundation, Inc.");
      Write_Eol;
      Write_Eol;
   end if;

   Write_Str ("Usage: ");
   Write_Program_Name;
   Write_Str (" [options] name[.cfg] {[partition]}");
   Write_Str (" {[-cargs opts] [-bargs opts] [-largs opts]}");
   Write_Eol;
   Write_Eol;

   Write_Str ("  name is a configuration file name from which you can");
   Write_Str (" omit the .cfg suffix");
   Write_Eol;
   Write_Eol;

   Write_Str ("gnatdist switches:");
   Write_Eol;

   Write_Str ("  -a        Consider all files, even readonly ali files");
   Write_Eol;
   Write_Str ("  -f        Force recompilations");
   Write_Eol;
   Write_Str ("  -q        Be quiet, do not display partitioning operations");
   Write_Eol;
   Write_Str ("  -v        Motivate all executed commands");
   Write_Eol;
   Write_Str ("  -t        Keep all temporary files");
   Write_Eol;
   Write_Str ("  --PCS=... "
              & "Select PCS variant (default: "
              & XE_Defs.Defaults.Default_PCS_Name & ")");
   Write_Eol;
   Write_Eol;

   Write_Str ("Other switches are passed directly to gnatmake");
   Write_Eol;
   Write_Eol;

   Write_Str ("Source & Library search path switches:");
   Write_Eol;

   Write_Str ("  -aLdir  Skip missing library sources if ali in dir");
   Write_Eol;

   Write_Str ("  -aOdir  Specify library/object files search path");
   Write_Eol;

   Write_Str ("  -aIdir  Specify source files search path");
   Write_Eol;

   Write_Str ("  -Idir   Like -aIdir -aOdir");
   Write_Eol;

   Write_Str ("  -I-     Don't look for sources & library files");
   Write_Str (" in the default directory");
   Write_Eol;

   Write_Str ("  -Ldir   Look for program libraries also in dir");
   Write_Eol;
   Write_Eol;

   Write_Str ("To pass an arbitrary switch to the Compiler, ");
   Write_Str ("Binder or Linker:");
   Write_Eol;

   Write_Str ("  -cargs opts   opts are passed to the compiler");
   Write_Eol;

   Write_Str ("  -bargs opts   opts are passed to the binder");
   Write_Eol;

   Write_Str ("  -largs opts   opts are passed to the linker");
   Write_Eol;
end XE_Usage;
