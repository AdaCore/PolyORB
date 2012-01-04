------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--              P O L Y O R B . T E S T _ O B J E C T _ P O A               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2002-2012, Free Software Foundation, Inc.          --
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

--  A simple test server object that uses the POA.

with PolyORB.Requests;
with PolyORB.Servants;
with PolyORB.Types;

package PolyORB.Test_Object_POA is

   pragma Elaborate_Body;

   type My_Object is new PolyORB.Servants.Servant with null record;

   function echoString
     (O : My_Object;
      S : PolyORB.Types.String)
     return PolyORB.Types.String;

   function echoInteger
     (O : My_Object;
      I : PolyORB.Types.Long)
     return PolyORB.Types.Long;

   overriding function Execute_Servant
     (Obj : not null access My_Object;
      Req : Requests.Request_Access) return Boolean;

end PolyORB.Test_Object_POA;
