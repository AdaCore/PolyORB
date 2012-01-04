------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--          S O A P . M E S S A G E . R E S P O N S E . E R R O R           --
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

with SOAP.Message.Payload;
with SOAP.Message.Response;

package SOAP.Message.Response.Error is

   type Object is new Message.Response.Object with private;

   type Faultcode is new String;

   function From (P : Message.Payload.Object) return Object;
   --  Build an Error response from a Payload object.

   function XML_Image (E : Object) return Unbounded_String;
   --  Returns the Fault env and associated data (faultcode, faultstring...).

   function Build
     (Faultcode   : Error.Faultcode;
      Faultstring : String)
      return Object;
   --  Returns an Error object built using Faultcode and Faultstring.

   function Is_Error (E : Object) return Boolean;
   --  Always returns True. This overrides  Response.Object's method.

   -----------------
   -- Fault Codes --
   -----------------

   function Version_Mismatch (Subname : String := "") return Faultcode;
   --  Returns the Version_Mismatch faultcode.

   function Must_Understand (Subname : String := "") return Faultcode;
   --  Returns the Must_Understand faultcode.

   function Client (Subname : String := "") return Faultcode;
   --  Returns the Client faultcode.

   function Server (Subname : String := "") return Faultcode;
   --  Returns the Server faultcode.

private

   type Object is new Message.Response.Object with null record;

end SOAP.Message.Response.Error;
