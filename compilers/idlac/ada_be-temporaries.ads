------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                   A D A _ B E . T E M P O R A R I E S                    --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2001-2004 Free Software Foundation, Inc.           --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
-- not, write to the Free Software Foundation, 59 Temple Place - Suite 330, --
-- Boston, MA 02111-1307, USA.                                              --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
--                PolyORB is maintained by ACT Europe.                      --
--                    (email: sales@act-europe.fr)                          --
--                                                                          --
------------------------------------------------------------------------------

--  $Id$

package Ada_Be.Temporaries is

   pragma Pure;
   pragma Warnings (Off);

   T_Handler             : constant String;
   T_Returns             : constant String;
   T_Send_Request_Result : constant String;
   T_Self_Ref            : constant String;
   T_Exception_Repo_Id   : constant String;
   T_Members             : constant String;
   T_Impl_Object_Ptr     : constant String;
   T_Value_Operation     : constant String;

   T_Request             : constant String;
   T_Ctx                 : constant String;
   T_Argument            : constant String;
   T_Arg_Name            : constant String;
   T_Arg_List            : constant String;
   T_Excp_List           : constant String;
   T_Result              : constant String;
   T_Result_Name         : constant String;
   T_Operation_Name      : constant String;

   T_J                   : constant String;

private

   T_Handler             : constant String := "Handler_Ü";
   T_Returns             : constant String := "Return_Ü";
   T_Self_Ref            : constant String := "Self_Ref_Ü";
   T_Send_Request_Result : constant String := "Send_Request_Result_Ü";
   T_Exception_Repo_Id   : constant String := "Exception_Repo_Id_Ü";
   T_Members             : constant String := "Members_Ü";
   T_Impl_Object_Ptr     : constant String := "Object_Ü";
   T_Value_Operation     : constant String := "Op_Ü";


   T_Request             : constant String := "Request_Ü";
   T_Ctx                 : constant String := "Ctx_Ü";
   T_Argument            : constant String := "Argument_Ü_";
   T_Arg_Name            : constant String := "Arg_Name_Ü_";
   T_Arg_List            : constant String := "Arg_List_Ü";
   T_Excp_List           : constant String := "Excp_List_Ü";
   T_Result              : constant String := "Result_Ü";
   T_Result_Name         : constant String := "Result_Name_Ü";
   T_Operation_Name      : constant String := "Operation_Name_Ü";

   T_J                   : constant String := "J_Ü";

end Ada_Be.Temporaries;
