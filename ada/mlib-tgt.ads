------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             M L I B . T G T                              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision$
--                                                                          --
--              Copyright (C) 2001-2002, Ada Core Technologies, Inc.        --
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

--  There are several versions for the body of this package.

--  In the default version, libraries are not supported, so function
--  Libraries_Are_Supported returns False.

with GNAT.OS_Lib; use GNAT.OS_Lib;

package MLib.Tgt is

   function Libraries_Are_Supported return Boolean;
   --  Indicates if building libraries by gnatmake and gnatmlib
   --  are supported by the GNAT implementation for the OS.

   function Default_DLL_Address return String;
   --  Default address for non relocatable DLL.
   --  For OSes where a dynamic library is always relocatable,
   --  this function returns an empty string.

   function Dynamic_Option return String;
   --  gcc option to create a dynamic library.
   --  For Unix, returns "-shared", for Windows returns "-mdll".

   function Libgnat return String;
   --  System dependent static GNAT library

   function Archive_Ext return  String;
   --  System dependent static library extension, without leading dot.
   --  For Unix and Windows, return "a".

   function Object_Ext return String;
   --  System dependent object extension, without leadien dot.
   --  On Unix, returns "o".

   function DLL_Ext return String;
   --  System dependent dynamic library extension, without leading dot.
   --  On Unix, returns "so", on Windows, returns "dll".

   function PIC_Option return String;
   --  Position independent code option

   function Is_Object_Ext (Ext : String) return Boolean;
   --  Returns True iff Ext is an object file extension

   function Is_C_Ext (Ext : String) return Boolean;
   --  Returns True iff Ext is a C file extension.

   function Is_Archive_Ext (Ext : String) return Boolean;
   --  Returns True iff Ext is an extension for a library

   function Linker_Library_Path_Option
     (Directory : String)
      return      String_Access;
   --  Linker option to specify to the linker the library directory path
   --  for Directory.

   procedure Build_Dynamic_Library
     (Ofiles       : Argument_List;
      Foreign      : Argument_List;
      Afiles       : Argument_List;
      Options      : Argument_List;
      Lib_Filename : String;
      Lib_Dir      : String;
      Lib_Address  : String  := "";
      Lib_Version  : String  := "";
      Relocatable  : Boolean := False);
   --  Build a dynamic/relocatable library.
   --
   --  Ofiles is the list of all object files in the library.
   --  Foreign is the list of non Ada object files (also included in Ofiles).
   --  Afiles is the list of ALI files for the Ada object files.
   --  Options is a list of options to be passed to the tool (gcc or other)
   --  that effectively builds the dynamic library.
   --  Lib_Filename is the name of the library, without any prefix or
   --  extension. For example, on Unix, if Lib_Filename is "toto", the name of
   --  the library file will be "libtoto.so".
   --  Lib_Dir is the directory path where the library will be located.
   --  Lib_Address is the base address of the library for a non relocatable
   --  library, given as an hexadecimal string.
   --  For OSes that support symbolic links, Lib_Version, if non null, is
   --  the actual file name of the library. For example on Unix,
   --  if Lib_Filename is "toto" and Lib_Version is "libtoto.so.2.1",
   --  "libtoto.so" will be a symbolic link to "libtoto.so.2.1" which will
   --  be the actual library file.
   --  Relocatable indicates if the library should be relocatable or not,
   --  for those OSes that actually support non relocatable dynamic libraries.
   --  Note: Depending on the OS, some of the parameters may not be taken
   --  into account. For example, on Linux, Foreign, Afiles Lib_Address and
   --  relocatable are ignored.

end MLib.Tgt;
