------------------------------------------------------------------------------
--                                                                          --
--                            GLADE EXAMPLES                                --
--                                                                          --
--                               T Y P E S                                  --
--                                                                          --
--                                S p e c                                   --
--                                                                          --
--                           $Revision$
--                                                                          --
--           Copyright (C) 1996 Free Software Foundation, Inc.              --
--                                                                          --
-- GLADE  is  free software;  you  can redistribute  it  and/or  modify  it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 2,  or  (at your option) any later --
-- version.  GLADE  is  distributed in the hope that it will be useful, but --
-- WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHANTABI- --
-- LITY or FITNESS  FOR A PARTICULAR PURPOSE.  See the  GNU General  Public --
-- License  for more details.  You should  have received a copy of the  GNU --
-- General  Public  License  distributed with GLADE;  see file COPYING.  If --
-- not, write to the Free Software Foundation, 59 Temple Place - Suite 330, --
-- Boston, MA 02111-1307, USA.                                              --
--                                                                          --
--                GLADE is maintained by ACT Europe.                        --
--            (email:distribution@act-europe.gnat.com).                     --
--                                                                          --
------------------------------------------------------------------------------

package Types is
   pragma Pure;
   type Std_Session is new Natural;
   type Std_Number  is range 0 .. 100;
   type Std_Node is new Natural;
end Types;
