------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              G N A T V S N                               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision$                           --
--                                                                          --
--          Copyright (C) 1992-1999 Free Software Foundation, Inc.          --
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

--  This package spec holds version information for GNAT, GNATBIND and
--  GNATMAKE. It is updated whenever the release number is changed.

package Gnatvsn is

   Gnat_Version_String : constant String := "3.13w (19990612)";
   --  Version output when GNAT (compiler), or its related tools, including
   --  GNATBIND, GNATCHOP, GNATFIND, GNATLINK, GNATMAKE, GNATXREF, are run
   --  (with appropriate verbose option switch set).
   --
   --  WARNING: some gnatmail scripts (at least make-bin and corcs) rely on
   --  the format of this string. Any change must be coordinated with
   --  a gnatmail maintainer.

   Ver_Len_Max : constant := 32;
   --  Longest possible length for Gnat_Version_String in this or any
   --  other version of GNAT. This is used by the binder to establish
   --  space to store any possible version string value for checks. This
   --  value should never be decreased in the future, but it would be
   --  OK to increase it if absolutely necessary.

   Library_Version : constant String := "GNAT Lib v3.12  ";
   --  Library version. This value must be updated whenever any change to the
   --  compiler affects the library formats in such a way as to obsolete
   --  previously compiled library modules.

end Gnatvsn;
