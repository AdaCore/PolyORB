------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                   M O M A . C O N F I G U R A T I O N                    --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--             Copyright (C) 1999-2002 Free Software Fundation              --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
-- not, write to the Free Software Foundation, 59 Temple Place - Suite 330, --
-- Boston, MA 02111-1307, USA.                                              --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
--              PolyORB is maintained by ENST Paris University.             --
--                                                                          --
------------------------------------------------------------------------------

--  $Id$

with MOMA.Types;
with PolyORB.Configuration;

package MOMA.Configuration is

   procedure Load_Configuration_File (Conf_File_Name : String)
    renames PolyORB.Configuration.Load_Configuration_File;

   function Get_Message_Pool (Number : Natural)
                              return MOMA.Types.Message_Pool;

   function Get_Name (Pool : MOMA.Types.Message_Pool)
                      return MOMA.Types.String;
   pragma Inline (Get_Name);

   function Get_Type (Pool : MOMA.Types.Message_Pool)
                      return MOMA.Types.Pool_Type;
   pragma Inline (Get_Type);

   function Get_Persistence (Pool : MOMA.Types.Message_Pool)
                           return MOMA.Types.Persistence_Mode;
   pragma Inline (Get_Persistence);

end MOMA.Configuration;
