------------------------------------------------------------------------------
--                                                                          --
--                          GNATDIST COMPONENTS                             --
--                                                                          --
--                              X E _ P C S                                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            1.9                              --
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
with Opt;
with Fname;
with Namet;            use Namet;
with Osint;            use Osint;
with Types;            use Types;
with Output;           use Output;
with ALI;              use ALI;
with GNAT.Os_Lib;      use GNAT.Os_Lib;
with XE_Utils;         use XE_Utils;
with XE;               use XE;
procedure XE_PCS is
   Text      : Text_Buffer_Ptr;
   RNS_Lib   : File_Name_Type;
   RNS_Ali   : ALI_Id;
   RNS_Name  : Unit_Name_Type;
   Partition : PID_Type;
begin
   null; --  XXXXX
end XE_PCS;


