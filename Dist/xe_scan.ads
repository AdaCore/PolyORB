------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--                              X E _ S C A N                               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision$
--                                                                          --
--         Copyright (C) 1996-1999 Free Software Foundation, Inc.           --
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
--                 GLADE  is maintained by ACT Europe.                      --
--                 (email: glade-report@act-europe.fr)                      --
--                                                                          --
------------------------------------------------------------------------------

with Types;
with XE;

package XE_Scan is

   Token_Name     : Types.Name_Id;
   Token          : XE.Token_Type;

   type Location_Type is
      record
         Line  : Types.Int;
         First : Types.Source_Ptr;
         Last  : Types.Source_Ptr;
      end record;

   Null_Location  : constant Location_Type := (0, 0, 0);

   function  Get_Token_Location return Location_Type;

   procedure Initialize;
   --  Load all kind of keywords

   procedure Load_File (File : in Types.File_Name_Type);
   --  Load this file in a memory buffer

   procedure Location_To_XY
     (Where : in  Location_Type;
      Loc_X : out Types.Int;
      Loc_Y : out Types.Int);

   procedure Next_Token;
   --  Find next token and update internal variables

   procedure Set_Token_Location (Where : in Location_Type);

   procedure Write_Location (Where : in Location_Type);
   --  Display line and column where the error occured

   procedure Write_Token (T : in XE.Token_Type);

end XE_Scan;
