with MOMA.Messages;
with Ada.Calendar; use Ada.Calendar;

package MOMA.Message_Consumers is

   ---------------------------------------
   --  Abstract Message_Consumer Object --
   ---------------------------------------
   type Message_Consumer is abstract tagged private;

   ---------------------
   -- Close Procedure --
   ---------------------
   procedure Close;

   ------------------------------------
   --  Get_Message_Selector Function --
   ------------------------------------
   function Get_Message_Selector return String;

   ------------------------
   --  Receive Functions --
   ------------------------
   function Receive return Messages.Message is abstract;

   function Receive (Timeout : Time) return Messages.Message is abstract;

   -------------------------------
   --  Receive_No_Wait Function --
   -------------------------------
   function Receive_No_Wait return Messages.Message is abstract;

private
   type Message_Consumer is abstract tagged null record;

end MOMA.Message_Consumers;
