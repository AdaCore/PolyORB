------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--      P O L Y O R B . C O R B A _ P . S E C U R I T Y _ P O L I C Y       --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2005-2006, Free Software Foundation, Inc.          --
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

with CORBA.Policy;
with PolyORB.References;
with PolyORB.Security.Credentials;
with PolyORB.Security.Types;

package PolyORB.CORBA_P.Security_Policy is

   type Client_Policy (Length : Natural) is record
      Client_Requires        : PolyORB.Security.Types.Association_Options;
      Invocation_Credentials :
        PolyORB.Security.Credentials.Credentials_List (1 .. Length);
   end record;

   function Get_Client_Policy
     (Object : PolyORB.References.Ref)
      return Client_Policy;

   type Convert_Client_Policy is
     access function (Policy : CORBA.Policy.Ref) return Client_Policy;

   procedure Register_Client_Policy
     (The_Type  : CORBA.PolicyType;
      Convertor : Convert_Client_Policy);

end PolyORB.CORBA_P.Security_Policy;
