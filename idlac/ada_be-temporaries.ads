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
