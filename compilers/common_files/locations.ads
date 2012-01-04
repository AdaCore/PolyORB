------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                            L O C A T I O N S                             --
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

package Locations is

   type Location is record
      File  : Name_Id;
      Dir   : Name_Id;
      Line  : Nat;
      First : Text_Ptr;
      Last  : Text_Ptr;
      Scan  : Text_Ptr;
   end record;

   No_Location : constant Location := (No_Name, No_Name, 0, 0, 0, 0);

   function Image (Loc : Location) return String;

   procedure Set_New_Location
     (Loc  : in out Location;
      Name : Name_Id;
      Line : Int);

   function "<" (Op1, Op2 : Location) return Boolean;
   --  If Op1 and Op2 are in the same file, returns True if and only if Op1 is
   --  before Op2. Returns False if Op1 and Op2 are not in the same file.

end Locations;
