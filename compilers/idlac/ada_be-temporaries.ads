------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                   A D A _ B E . T E M P O R A R I E S                    --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2001-2006, Free Software Foundation, Inc.          --
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

package Ada_Be.Temporaries is

   pragma Preelaborate;

   function T_Handler return String;
   function T_Returns return String;
   function T_Send_Request_Result return String;
   function T_Self_Ref return String;
   function T_Exception_Repo_Id return String;
   function T_Members return String;
   function T_Impl_Object_Ptr return String;
   function T_Value_Operation return String;

   function T_Request return String;
   function T_Ctx return String;
   function T_Argument return String;
   function T_Arg_Any return String;
   function T_Arg_CC return String;
   function T_Arg_List return String;
   function T_Arg_Name return String;
   function T_Content return String;
   function T_Excp_List return String;
   function T_Helpers return String;
   function T_Indices return String;
   function T_Lengths return String;
   function T_Ptr return String;
   function T_Result return String;
   function T_Result_Name return String;

   function T_J return String;

end Ada_Be.Temporaries;
