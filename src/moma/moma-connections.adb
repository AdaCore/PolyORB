package body MOMA.Connections is

   ----------------------
   --  Close Procedure --
   ----------------------
   procedure Close is
   begin
      null;
   end Close;

   -----------------------------
   --  Get_Client_Id Function --
   -----------------------------
   function Get_Client_Id return String is
      Temp : String := "Temp";
   begin
      return Temp;
   end Get_Client_Id;

   ------------------------------
   --  Set_Client_Id Procedure --
   ------------------------------
   procedure Set_Client_Id (Client_Id : String) is
   begin
      null;
   end Set_Client_Id;

   ----------------------
   --  Start Procedure --
   ----------------------
   procedure Start is
   begin
      null;
   end Start;

   ---------------------
   --  Stop Procedure --
   ---------------------
   procedure Stop is
   begin
      null;
   end Stop;

   -----------------------------
   --  Get_Meta_Data Function --
   -----------------------------
   function Get_Meta_Data return Meta_Data is
      Temp : Meta_Data := 0;
   begin
      return Temp;
   end Get_Meta_Data;


end MOMA.Connections;
