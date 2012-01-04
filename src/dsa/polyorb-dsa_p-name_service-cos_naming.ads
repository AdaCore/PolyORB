------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                  POLYORB.DSA_P.NAME_SERVICE.COS_NAMING                   --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2010-2012, Free Software Foundation, Inc.          --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.                                               --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
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
