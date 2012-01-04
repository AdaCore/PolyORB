------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                          S O A P . C L I E N T                           --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2000-2012, Free Software Foundation, Inc.          --
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

with AWS.Client;
with SOAP.Message.Payload;
with SOAP.Message.Response;

package SOAP.Client is

   Not_Specified : constant String;

   function Call
     (URL        : String;
      P          : Message.Payload.Object;
      SOAPAction : String         := Not_Specified)
      return Message.Response.Object'Class;
   --  Send a SOAP HTTP request to URL address. The P is the Payload and
   --  SOAPAction is the required HTTP field. If it is not specified then the
   --  URI (URL resource) will be used for the SOAPAction field. The complete
   --  format is "URL & '#' & Procedure_Name" (Procedure_Name is retrieved
   --  from the Payload object.

   function Call
     (Connection : access AWS.Client.HTTP_Connection;
      P          : Message.Payload.Object)
      return Message.Response.Object'Class;
   --  Idem as above, but use an already opened connection.

private

   Not_Specified : constant String := "";

end SOAP.Client;
