with MOMA.Message_Consumers;
with MOMA.Sessions;
with Temp; use Temp;

package MOMA.Connections is

   ---------------------------------
   --  Abstract Object Connection --
   ---------------------------------
   type Connection is abstract tagged private;

   ----------------------
   --  Close Procedure --
   ----------------------
   procedure Close;

   -----------------------------
   --  Get_Client_Id Function --
   -----------------------------
   function Get_Client_Id return String;

   ------------------------------
   --  Set_Client_Id Procedure --
   ------------------------------
   procedure Set_Client_Id (Client_Id : String);

   ----------------------
   --  Start Procedure --
   ----------------------
   procedure Start;

   ---------------------
   --  Stop Procedure --
   ---------------------
   procedure Stop;

   -----------------------------
   --  Get_Meta_Data Function --
   -----------------------------
   function Get_Meta_Data return Meta_Data;


   --  Abstract Procedures and Functions --

   ----------------------------------------
   --  Abstract Create_Consumer Function --
   ----------------------------------------
   function Create_Consumer return Message_Consumers.Message_Consumer
      is abstract;

   ----------------------------------------
   --  Abstract Create_Session Function --
   ----------------------------------------
   function Create_Session (Transacted : Boolean;
                            Ackowledge_Mode : Acknowledge_Type)
                           return Sessions.Session
      is abstract;


private
   type Connection is abstract tagged null record;

end MOMA.Connections;
