------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--          POLYORB.GIOP_P.TAGGED_COMPONENTS.SSL_SEC_TRANS.CREATE           --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2007-2017, Free Software Foundation, Inc.          --
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

with PolyORB.Sockets;

package body PolyORB.GIOP_P.Tagged_Components.SSL_Sec_Trans.Create is

   ---------------
   -- Create_TC --
   ---------------

   procedure Create_TC
     (Param      : Parameter_Component;
      TC         : in out TC_SSL_Sec_Trans;
      Error      : out Boolean)
   is
      use PolyORB.Sockets;
   begin
      --  -supports <value> -requires <values> -port <port_value>
      Error := False;

      TC.Target_Supports := Association_Options'Value
        (Param.SSL_Supports.all);

      TC.Target_Requires := Association_Options'Value
        (Param.SSL_Requires.all);

      TC.Port := Port_Type (Param.Address.Port);
   end Create_TC;

end PolyORB.GIOP_P.Tagged_Components.SSL_Sec_Trans.Create;
