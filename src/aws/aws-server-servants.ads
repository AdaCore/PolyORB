------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                  A W S . S E R V E R . S E R V A N T S                   --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2009-2010, Free Software Foundation, Inc.          --
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

pragma Ada_2005;

with PolyORB; use PolyORB;
with PolyORB.Requests;

package AWS.Server.Servants is

   type Web_Servant is new HTTP with null record;
   type SOAP_Servant is new HTTP with null record;
   --  2 servants are associated to an AWS Web server

private

   type Web_Servant_Acc is access all Web_Servant;
   type SOAP_Servant_Acc is access all SOAP_Servant;

   overriding function Execute_Servant
     (S   : not null access Web_Servant;
      Req : Requests.Request_Access) return Boolean;

   overriding function Execute_Servant
     (S   : not null access SOAP_Servant;
      Req : Requests.Request_Access) return Boolean;

end AWS.Server.Servants;
