------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             P R J . S T R T                              --
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

with Prj.Tree;  use Prj.Tree;

private package Prj.Strt is

   procedure Parse_String_Type_List
     (First_String : out Project_Node_Id);
   --  Get the list of literal string that are allowed for
   --  a typed string.
   --  Report an error if
   --    - a literal string is not found at the beginning of the list
   --      or after a comma
   --    - two literal strings in the list are equal

   procedure Start_New_Case_Construction
     (String_Type : Project_Node_Id);
   --  This procedure is called at the beginning of a case construction
   --  It indicates the allowed literal strings for the case labels

   procedure Parse_Choice_List
     (First_Choice : out Project_Node_Id);
   --  Get the label for a choice list
   --  Report an error if
   --    - a case label is not a literal string
   --    - a case label is not in the typed string list
   --    - the same case label is repeated in the same case construction

   procedure Parse_Expression
     (Expression      : out Project_Node_Id;
      Current_Project : Project_Node_Id;
      Current_Package : Project_Node_Id);
   --  simple string or string list

   procedure Parse_Variable_Reference
     (Variable        : out Project_Node_Id;
      Current_Project : Project_Node_Id;
      Current_Package : Project_Node_Id);
   --  Parse a variable reference. Used internally (in expressions)
   --  and for case variables (in Prj.Dect).

end Prj.Strt;
