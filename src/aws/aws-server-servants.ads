------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                  A W S . S E R V E R . S E R V A N T S                   --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2009-2012, Free Software Foundation, Inc.          --
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
