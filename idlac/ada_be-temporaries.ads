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
   T_Repository_Id       : constant String;
   T_Exception_Repo_Id   : constant String;
   T_Members             : constant String;

private

   T_Handler             : constant String := "Handler_é";
   T_Returns             : constant String := "Return_é";
   T_Send_Request_Result : constant String := "Send_Request_Result_é";
   T_Repository_Id       : constant String := "Repository_Id_é";
   T_Exception_Repo_Id   : constant String := "Exception_Repo_Id_é";
   T_Members             : constant String := "Members_é";

end Ada_Be.Temporaries;
