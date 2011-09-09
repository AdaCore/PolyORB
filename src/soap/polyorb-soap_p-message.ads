------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--               P O L Y O R B . S O A P _ P . M E S S A G E                --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2000-2011, Free Software Foundation, Inc.          --
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

with Ada.Strings.Unbounded;

with PolyORB.SOAP_P.Parameters;

package PolyORB.SOAP_P.Message is

   use Ada.Strings.Unbounded;

   type Object is tagged record
      Name_Space   : Unbounded_String;
      Wrapper_Name : Unbounded_String;
      P            : SOAP_P.Parameters.List;
   end record;

   function XML_Image (M : Object) return Unbounded_String;
   --  Returns the XML image for the wrapper and parameters. This is designed
   --  to be used by Payload and Response object.

   --  Note: the following accessors are deprecated, they are here for
   --  legacy reasons, as Object used to be a private type.

   function Name_Space   (M : Object'Class) return String;
   --  Returns message Namespace.

   function Wrapper_Name (M : Object'class) return String;
   --  Returns wrapper name.

   function Parameters   (M : Object'class) return SOAP_P.Parameters.List;
   --  Returns the parameters

   procedure Set_Name_Space
     (M    : in out Object'Class;
      Name : String);
   --  Set message's name space

   procedure Set_Wrapper_Name
     (M     : in out Object'Class;
      Name  : String);
   --  Set message's wrapper name

   procedure Set_Parameters
     (M     : in out Object'Class;
      P_Set : SOAP_P.Parameters.List);
   --  Set message's parameters

end PolyORB.SOAP_P.Message;
