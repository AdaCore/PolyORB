------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              P R J . S T R                               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision$
--                                                                          --
--             Copyright (C) 2001 Free Software Foundation, Inc.            --
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
--
--  This package implements parsing of string expressions in project files.

with Types;  use Types;

private package Prj.Str is

   function Value
     (Project     : Project_Data;
      Pkg         : Package_Id;
      Do_Not_Skip : Boolean)
     return       Variable_Value;
   --  Parse a single string or string list expression

   function Value
     (Project    : Project_Data;
      Pkg        : Package_Id)
      return       Name_Id;

   function Value
     (Project    : Project_Data;
      Pkg        : Package_Id)
      return       String;

   function Value
     (Project    : Project_Data;
      Pkg        : Package_Id)
      return       String_Id;
   --  Parse a Single String Expression:
   --
   --   string_expression ::=
   --     string_term {& string_term}
   --
   --   string_term ::=
   --     literal_string |
   --     <single_variable_>name |
   --     external_value
   --
   --   external_value ::=
   --     _external_ (string_expression [,string_expression])
   --
   --  Before: the current token is the first of the expression.
   --  After:  the current token is the first one after the expression.
   --
   --  The three functions Value do the same job, but return the result
   --  as different types.

end Prj.Str;


----------------------
-- REVISION HISTORY --
----------------------

--  ----------------------------
--  revision 1.1
--  date: 2000/12/18 14:07:08;  author: pautet;  state: Exp;
--  update GNAT sources for GLADE developpers
--  ----------------------------
--  revision 1.2
--  date: 2001/01/24 16:14:57;  author: pautet;
--  update gnat local copy for GLADE team
--  ----------------------------
--  revision 1.3
--  date: 2001/03/07 14:14:28;  author: tardieu;
--  Add sinfo-cn.ad[bs] and update other sources for GLADE developers.
--  ----------------------------
--  New changes after this line.  Each line starts with: "--  "
