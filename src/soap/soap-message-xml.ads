------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                         Copyright (C) 2000-2001                          --
--                                ACT-Europe                                --
--                                                                          --
--  Authors: Dmitriy Anisimkov - Pascal Obry                                --
--                                                                          --
--  This library is free software; you can redistribute it and/or modify    --
--  it under the terms of the GNU General Public License as published by    --
--  the Free Software Foundation; either version 2 of the License, or (at   --
--  your option) any later version.                                         --
--                                                                          --
--  This library is distributed in the hope that it will be useful, but     --
--  WITHOUT ANY WARRANTY; without even the implied warranty of              --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU       --
--  General Public License for more details.                                --
--                                                                          --
--  You should have received a copy of the GNU General Public License       --
--  along with this library; if not, write to the Free Software Foundation, --
--  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.          --
--                                                                          --
--  As a special exception, if other files instantiate generics from this   --
--  unit, or you link this unit with other files to produce an executable,  --
--  this  unit  does not  by itself cause  the resulting executable to be   --
--  covered by the GNU General Public License. This exception does not      --
--  however invalidate any other reasons why the executable file  might be  --
--  covered by the  GNU Public License.                                     --
------------------------------------------------------------------------------

--  $Id$

with SOAP.Message.Payload;
with SOAP.Message.Response;
with Input_Sources;

with PolyORB.Any.NVList;

package SOAP.Message.XML is

   function Load_Payload
     (Source : access Input_Sources.Input_Source'Class;
      Args   : in     PolyORB.Any.NVList.Ref)
     return Message.Payload.Object_Access;
   --  Build a Payload object by parsing an XML payload from source.
   --  Args is expected to designate a list of empty Any's,
   --  whose typecodes are used to determine how to decode the
   --  XML elements into typed data. On return, the values
   --  of these Any's are set according to the decoded XML
   --  elements.

   function Load_Response
     (Source : access Input_Sources.Input_Source'Class;
      Args   : in     PolyORB.Any.NVList.Ref)
     return Message.Response.Object_Access;
   --  Build a Response object (either a standard response or an error
   --  response) by parsing an XML response from Source.
   --  Args are used as above (for returned arguments).
   --  XXX warning, return value vs. out args? Does the return
   --  value need to be the first OUT element of the Args list?

   function Image (Obj : in Object'Class) return String;
   --  Returns XML representation of object O.

   function Image (Obj : in Object'Class) return Unbounded_String;
   --  Idem as above but returns an Unbounded_String instead of a String.

end SOAP.Message.XML;
