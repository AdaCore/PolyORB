------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--              P O L Y O R B . T E S T _ O B J E C T _ S O A               --
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

--  A simple test server object that uses the SOA.

with PolyORB.Obj_Adapters.Simple;
with PolyORB.Requests;
with PolyORB.Servants;
with PolyORB.Types;

package PolyORB.Test_Object_SOA is

   pragma Elaborate_Body;

   use PolyORB.Types;

   type My_Object is new PolyORB.Servants.Servant with null record;

   function waitAndEchoString
     (O : My_Object;
      S : Types.String;
      T : Types.Long)
     return Types.String;

   function echoString
     (O : My_Object;
      S : Types.String)
     return Types.String;

   function echoInteger
     (O : My_Object;
      I : Types.Long)
     return Types.Long;

   overriding function Execute_Servant
     (Obj : not null access My_Object;
      Req : Requests.Request_Access) return Boolean;

   function If_Desc
     return Obj_Adapters.Simple.Interface_Description;
   pragma Inline (If_Desc);

end PolyORB.Test_Object_SOA;
