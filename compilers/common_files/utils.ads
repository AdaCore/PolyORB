------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                                U T I L S                                 --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2004-2012, Free Software Foundation, Inc.          --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.                                               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

with Types; use Types;

package Utils is

   function Image (N : Int) return String;
   --  Return the image of N without the annoying leading blank

   procedure Capitalize (S : in out String);
   --  Change in S any leading character or any successor of an
   --  underscore into its corresponding uppercase character.

   function Quoted (S : String; D : Character := '"') return String; --  "
   function Quoted (S : String; D : Character := '"') return Name_Id; --  "
   function Quoted (N : Name_Id; D : Character := '"') return String; --  "
   function Quoted (N : Name_Id; D : Character := '"') return Name_Id; --  "
   --  Embrace string S or name N with character D

   procedure To_Lower (S : in out String);
   function To_Lower (N : Name_Id) return Name_Id;

   function Is_Dir_Separator (C : Character) return Boolean;
   --  Returns True if C is a directory separator. Always True for '/', since
   --  this is acceptable on Windows even though the "standard" one is '\'.

   function Simple_Command_Name return String;
   --  Returns the simple name of the command. Same as
   --  Ada.Command_Line.Command_Name, except this strips off the directory and
   --  extension, if any.

end Utils;
