------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                       A W S . S T A T U S . S E T                        --
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
