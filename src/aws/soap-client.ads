------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                          S O A P . C L I E N T                           --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2000-2006, Free Software Foundation, Inc.          --
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
