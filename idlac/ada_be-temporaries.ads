package Ada_Be.Temporaries is

   pragma Pure;

   T_Handler             : constant String;
   T_Returns             : constant String;
   T_Send_Request_Result : constant String;
   T_Repository_Id       : constant String;
   T_Exception_Repo_Id   : constant String;
   T_Members             : constant String;

private

   T_Handler             : constant String := "Handler_�";
   T_Returns             : constant String := "Return_�";
   T_Send_Request_Result : constant String := "Send_Request_Result_�";
   T_Repository_Id       : constant String := "Repository_Id_�";
   T_Exception_Repo_Id   : constant String := "Exception_Repo_Id_�";
   T_Members             : constant String := "Members_�";

end Ada_Be.Temporaries;
