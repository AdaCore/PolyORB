------------------------------------------------------------------------------
--                                                                          --
--                            GLADE EXAMPLES                                --
--                                                                          --
--                              M A S T E R                                 --
--                                                                          --
--                                B o d y                                   --
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

with Prime_1;
with System.IO;
with Types;
use Types;
procedure master is
   Success : Boolean;
   Where   : Std_Node;
begin
   for Number in Std_Number(2) .. Std_Number(100) loop
      Prime_1.Initiate (Number, Where, Success);
      if Success then
	 System.IO.Put_Line (Std_Number'Image (Number) & " (node" &
        	             Std_Node'Image (Where) & ")");
      end if;
   end loop;
end Master;
