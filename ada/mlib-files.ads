------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                            M L I B . F I L E S                           --
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

--  This package provides a set of routines to deal with file extensions

package MLib.Files is

   function Get_Ext (Filename : in String) return String;
   --  return extention of filename.

   function Is_Ada (Filename : in String) return Boolean;
   --  test if Filename is an Ada file (.ads or .adb).

   function Is_Obj (Filename : in String) return Boolean;
   --  test if Filename is an object file (.o, .obj or Object_Ext).

   function Is_C (Filename : in String) return Boolean;
   --  test if Filename is a C file (.c)

   function Is_Archive (Filename : String) return Boolean;
   --  test if filename is an archive (.a, .dll or .so)

   function Ext_To (Filename : in String; New_Ext : in String := "")
     return String;
   --  return Filename with the extention change to New_Ext.

end MLib.Files;
