------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                         S O A P . M E S S A G E                          --
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
