------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--           P O L Y O R B . S O A P _ P . M E S S A G E . X M L            --
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

with PolyORB.SOAP_P.Message.Payload;
with PolyORB.SOAP_P.Message.Response;
with Input_Sources;

with PolyORB.Any.NVList;

package PolyORB.SOAP_P.Message.XML is

   procedure Load_Payload
     (Source    : access Input_Sources.Input_Source'Class;
      Args      : in out PolyORB.Any.NVList.Ref;
      R_Payload :    out Message.Payload.Object_Access);
   --  Build a Payload object by parsing an XML payload from source.
   --  Args is expected to designate a list of empty Any's,
   --  whose typecodes are used to determine how to decode the
   --  XML elements into typed data. On return, the values
   --  of these Any's are set according to the decoded XML
   --  elements.

   function Load_Response
     (Source : access Input_Sources.Input_Source'Class;
      Args   : PolyORB.Any.NVList.Ref)
     return Message.Response.Object_Access;
   --  Build a Response object (either a standard response or an error
   --  response) by parsing an XML response from Source.
   --  Args are used as above (for returned arguments).
   --  XXX warning, return value vs. out args? Does the return
   --  value need to be the first OUT element of the Args list?

   procedure Load_Response
     (Source : access Input_Sources.Input_Source'Class;
      Args   : in out PolyORB.Any.NVList.Ref);
   --  Same as the function, except that we do not build any
   --  Object_Access, and we edit the Args list with the arguments
   --  found in the xml tree

   function Image (Obj : Object'Class) return String;
   --  Returns XML representation of object O.

   function Image (Obj : Object'Class) return Unbounded_String;
   --  Idem as above but returns an Unbounded_String instead of a String.

end PolyORB.SOAP_P.Message.XML;
