------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--        P O L Y O R B . F I L T E R S . A W S _ I N T E R F A C E         --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                Copyright (C) 2001 Free Software Fundation                --
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

--  Messages exchanged by AWS-based filters and components.

--  $Id$

with AWS.Response;

with PolyORB.Filters.Interface;
with PolyORB.HTTP_Methods;
with PolyORB.Types;

package PolyORB.Filters.AWS_Interface is

   use PolyORB.Filters.Interface;

   type AWS_Request_Out is new Root_Data_Unit with record
      Request_Method : PolyORB.HTTP_Methods.Method;
      Relative_URI   : Types.String;
      Data           : Types.String;
      SOAP_Action    : Types.String;
--       User : ;
--       Passwd : ;
--       Proxy : ;
--       Proxy_User : ;
--       Proxy_Passwd : ;
   end record;

   type AWS_Response_Out is new Root_Data_Unit with record
      --  Direction: from upper to lower.
      --  Semantics: send AWS response out.
      Data : AWS.Response.Data;
   end record;

end PolyORB.Filters.AWS_Interface;
