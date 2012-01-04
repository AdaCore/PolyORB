------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--   P O L Y O R B . Q O S . E X C E P T I O N _ I N F O R M A T I O N S    --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2006-2012, Free Software Foundation, Inc.          --
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

with Ada.Exceptions;

with PolyORB.Requests;
with PolyORB.Types;

package PolyORB.QoS.Exception_Informations is

   pragma Elaborate_Body;

   type QoS_Ada_Exception_Information_Parameter is
     new QoS_Parameter (Ada_Exception_Information) with
   record
      Exception_Information : PolyORB.Types.String;
   end record;

   type QoS_Ada_Exception_Information_Parameter_Access is
     access all QoS_Ada_Exception_Information_Parameter'Class;

   procedure Set_Exception_Information
     (Request    : in out Requests.Request;
      Occurrence : Ada.Exceptions.Exception_Occurrence);
   --  Add additional exception information to Ada_Exception_Information reply
   --  service context.

   function Get_Exception_Information
     (R : Requests.Request) return String;
   --  Utility function to extract the above QoS parameter from a request.
   --  A zero-length string is returned if no such QoS parameter is present.

   function Get_Exception_Message
     (R : Requests.Request) return String;
   --  Utility function to extract just the Exception_Message part from the
   --  Exception_Information. Like the above, returns a zero length string
   --  if no suitable QoS parameter is available.

end PolyORB.QoS.Exception_Informations;
