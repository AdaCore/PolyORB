------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                       A W S . S T A T U S . S E T                        --
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

--  This package is used when parsing the HTTP protocol from the client. It is
--  used to keep the values for the currently handled HTTP parameters.

with SOAP.Message.Payload;

package AWS.Status.Set is

   procedure Reset (D : in out Data);
   --  Reset the status data for a new use.

   procedure Free (D : in out Data);
   --  Free all allocated memory.

   procedure Keep_Alive
     (D    : in out Data;
      Flag : Boolean);
   --  Set the Keep-Alive flag for the current HTTP connection.

   procedure Session
     (D : in out Data);
   --  Generate new Session ID

   procedure Peername
     (D        : in out Data;
      Peername : String);
   --  Set peername field

   procedure Request
     (D            : in out Data;
      Method       : Request_Method;
      URI          : String;
      HTTP_Version : String);
   --  Set values for the request line:
   --
   --  GET URI[?parametrers] [HTTP/1.0 or HTTP/1.1]
   --  POST URI [HTTP/1.0 or HTTP/1.1]

   procedure Parameters (D : in out Data; Set : AWS.Parameters.List);
   --  Associate the parameters in Set to the status data

   procedure Binary
     (D         : in out Data;
      Parameter : Stream_Element_Array);
   --  This procedure is used to store any binary data sent with the
   --  request. For example this will be used by the PUT method if a binary
   --  file is sent to the server.

   procedure Payload
     (D       : in out Data;
      Payload : SOAP.Message.Payload.Object);
   --  Set the Payload message.

end AWS.Status.Set;
