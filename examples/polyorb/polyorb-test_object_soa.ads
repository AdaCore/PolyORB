------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--              P O L Y O R B . T E S T _ O B J E C T _ S O A               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--             Copyright (C) 1999-2002 Free Software Fundation              --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
-- not, write to the Free Software Foundation, 59 Temple Place - Suite 330, --
-- Boston, MA 02111-1307, USA.                                              --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
--              PolyORB is maintained by ENST Paris University.             --
--                                                                          --
------------------------------------------------------------------------------

--  A simple test server object that uses the SOA.

--  $Id$

with PolyORB.Components;
with PolyORB.Obj_Adapters.Simple;
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

   function Execute_Servant
     (Obj : access My_Object;
      Msg : Components.Message'Class)
     return Components.Message'Class;

   function If_Desc
     return Obj_Adapters.Simple.Interface_Description;
   pragma Inline (If_Desc);

end PolyORB.Test_Object_SOA;

