------------------------------------------------------------------------------
--                                                                          --
--                          GNATDIST COMPONENTS                             --
--                                                                          --
--                             X E _ S C A N                                --
--                                                                          --
--                                S p e c                                   --
--                                                                          --
--                           $Revision$                              --
--                                                                          --
--           Copyright (C) 1996 Free Software Foundation, Inc.              --
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
--              GNATDIST is maintained by ACT Europe.                       --
--            (email:distribution@act-europe.gnat.com).                     --
--                                                                          --
------------------------------------------------------------------------------

with Types;        use Types;
with XE;           use XE;
package XE_Scan is

   type Location_Type is record
      Line  : Int;
      First : Source_Ptr;
      Last  : Source_Ptr;
   end record;

   procedure Initialize;
   --  Load all kind of keyworks

   procedure Load_File (File : in File_Name_Type);
   --  Load this file in a memory buffer.

   procedure Next_Token;
   --  Find next token and update internal variables.

   function  Get_Token_Location return Location_Type;
   procedure Set_Token_Location (Where : in Location_Type);

   procedure Write_Location (Where : in Location_Type);
   --  Display line and column where the error occured

   procedure Write_Token (T : Token_Type);

   Token_Name     : Name_Id;
   Token          : Token_Type;

   Null_Location  : constant Location_Type := (0, 0, 0);

end XE_Scan;
