------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--                   A D A _ B E . T E M P O R A R I E S                    --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1999-2000 ENST Paris University, France.          --
--                                                                          --
-- AdaBroker is free software; you  can  redistribute  it and/or modify it  --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. AdaBroker  is distributed  in the hope that it will be  useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with AdaBroker; see file COPYING. If  --
-- not, write to the Free Software Foundation, 59 Temple Place - Suite 330, --
-- Boston, MA 02111-1307, USA.                                              --
--                                                                          --
--             AdaBroker is maintained by ENST Paris University.            --
--                     (email: broker@inf.enst.fr)                          --
--                                                                          --
------------------------------------------------------------------------------

package Ada_Be.Temporaries is

   pragma Pure;

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

private

   T_Handler             : constant String := "Handler_�";
   T_Returns             : constant String := "Return_�";
   T_Self_Ref            : constant String := "Self_Ref_�";
   T_Send_Request_Result : constant String := "Send_Request_Result_�";
   T_Exception_Repo_Id   : constant String := "Exception_Repo_Id_�";
   T_Members             : constant String := "Members_�";
   T_Impl_Object_Ptr     : constant String := "Object_�";
   T_Value_Operation     : constant String := "Op_�";


   T_Request             : constant String := "Request_�";
   T_Ctx                 : constant String := "Ctx_�";
   T_Argument            : constant String := "Argument_�_";
   T_Arg_Name            : constant String := "Arg_Name_�_";
   T_Arg_List            : constant String := "Arg_List_�";
   T_Excp_List           : constant String := "Excp_List_�";
   T_Result              : constant String := "Result_�";
   T_Result_Name         : constant String := "Result_Name_�";
   T_Operation_Name      : constant String := "Operation_Name_�";


end Ada_Be.Temporaries;
