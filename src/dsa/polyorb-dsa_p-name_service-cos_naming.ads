------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                  POLYORB.DSA_P.NAME_SERVICE.COS_NAMING                   --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2010-2011, Free Software Foundation, Inc.          --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
-- not, write to the Free Software Foundation, 51 Franklin Street, Fifth    --
-- Floor, Boston, MA 02111-1301, USA.                                       --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

--  This package represents the CORBA COS Naming context, that uses the
--  concept of a centralized name server.

with PolyORB.References;
with PolyORB.DSA_P.Name_Service;
with PolyORB.Services.Naming;
package PolyORB.DSA_P.Name_Service.COS_Naming is

   type COS_Name_Server is new Name_Server with null record;

   procedure Nameserver_Register
     (Name_Ctx : access COS_Name_Server;
      Name : String;
      Kind : String;
      Obj  : PolyORB.References.Ref);
   --  Register object with the specified (Name, Kind) pair into the
   --  DSA naming context.

   function Nameserver_Lookup
     (Name_Ctx : access COS_Name_Server;
      Name     : String;
      Kind     : String;
      Initial  : Boolean := True) return PolyORB.References.Ref;
   --  Look up the specified (Name, Kind) pair from the DSA naming context.
   --  If Initial is True, repeat lookup until a valid reference is obtained,
   --  and raise an exception if maximum retry count is reached, else just
   --  return an empty ref if name server retruns an empty or invalid result.

   function To_Name (Id, Kind : String) return PolyORB.Services.Naming.Name;
   --  Construct a name consisting of a single name component with the given
   --  id and kind.

end PolyORB.DSA_P.Name_Service.COS_Naming;
