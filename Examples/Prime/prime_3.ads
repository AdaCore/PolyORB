------------------------------------------------------------------------------
--                                                                          --
--                            GLADE EXAMPLES                                --
--                                                                          --
--                             P R I M E _ 3                                --
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
with Types;
use Types;
package Prime_3 is
   pragma Remote_Call_Interface;

   procedure Begin_Session
     (Number  : Std_Number;
      Session : Std_Session);

   pragma Asynchronous (Begin_Session);

   procedure End_Session
     (Session : Std_Session;
      Node    : Std_Node;
      Result  : Boolean);

   pragma Asynchronous (End_Session);

   procedure Initiate
     (Number : Std_Number;
      Node   : out Std_Node; 
      Result : out Boolean);

end Prime_3;
