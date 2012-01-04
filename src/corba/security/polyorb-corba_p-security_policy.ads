------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--      P O L Y O R B . C O R B A _ P . S E C U R I T Y _ P O L I C Y       --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2005-2012, Free Software Foundation, Inc.          --
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
