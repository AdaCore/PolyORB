------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                         S O A P . M E S S A G E                          --
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

with Ada.Strings.Unbounded;

with SOAP.Parameters;

package SOAP.Message is

   use Ada.Strings.Unbounded;

   type Object is tagged private;

   Default_Name_Space : constant String := "http://mns.org/";
   --  Default name space used by AWS if none as been specified.

   function XML_Image (M : Object) return Unbounded_String;
   --  Returns the XML image for the wrapper and parameters. This is designed
   --  to be used by Payload and Response object.

   function Name_Space   (M : Object'Class) return String;
   --  Returns message Namespace.

   function Wrapper_Name (M : Object'class) return String;
   --  Returns wrapper name.

   function Parameters   (M : Object'class) return SOAP.Parameters.List;
   --  Returns the parameter.

   procedure Set_Name_Space
     (M    : in out Object'Class;
      Name : String);
   --  Set message's Namespace.

   procedure Set_Wrapper_Name
     (M     : in out Object'Class;
      Name  : String);
   --  Set message's wrapper name.

   procedure Set_Parameters
     (M     : in out Object'Class;
      P_Set : SOAP.Parameters.List);
   --  Set message's parameters.

private

   type Object is tagged record
      Name_Space   : Unbounded_String
        := To_Unbounded_String (Default_Name_Space);
      Wrapper_Name : Unbounded_String;
      P            : SOAP.Parameters.List;
   end record;

end SOAP.Message;
