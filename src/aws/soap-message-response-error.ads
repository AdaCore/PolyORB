------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--          S O A P . M E S S A G E . R E S P O N S E . E R R O R           --
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
