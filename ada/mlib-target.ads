------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                          M L I B . T A R G E T                           --
--                            (Default Version)                             --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision$
--                                                                          --
--              Copyright (C) 1999, Ada Core Technologies, Inc.             --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- It is now maintained by Ada Core Technologies Inc (http://www.gnat.com). --
--                                                                          --
------------------------------------------------------------------------------

--  This package provides a set of target dependent routines to build
--  static, dynamic and shared libraries.
--  It also collects all the target dependent options used by GNATMLIB

package MLib.Target is

   Default_DLL_Address : constant String := "";
   --  default address for non relocatable DLL

   Dynamic_Option      : constant String := "-shared";
   --  gcc option to create a dynamic library

   Base_Option         : constant String := "";

   Libgnat             : constant String := "libgnat.a";
   --  System dependent static GNAT library

   Archive_Ext         : constant String := "a";
   --  System dependent static library extension

   Object_Ext          : constant String := "o";
   --  System dependent object extension

   DLL_Ext             : constant String := "so";
   --  System dependent dynamic library extension

   PIC_Option          : constant String := "-fPIC";
   --  Position independent code option

   procedure Build_Dynamic_Library
     (Ofiles       : in Argument_List;
      Foreign      : in Argument_List;
      Afiles       : in Argument_List;
      Options      : in Argument_List;
      Lib_Filename : in String;
      Lib_Address  : in String  := "";
      Relocatable  : in Boolean := False);
   --  Build a dynamic/relocatable library

end MLib.Target;
