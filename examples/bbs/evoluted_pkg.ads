with Common; use Common;

package Evoluted_Pkg is

   ----------------------------------------------
   -- Global variables for automatic test mode --
   ----------------------------------------------

   protected Received_Counter is
      procedure Set_Expected (N : Integer);
      procedure Message_Received;
      entry Wait_For_Completion;
   private
      Expected : Integer := 0;
   end Received_Counter;

   type Instrumented_Penpal is new Penpal_Type with null record;
   procedure New_Message
     (Sender    : in String;
      Recipient : access Instrumented_Penpal;
      Message   : in String);

   Penpal : aliased Instrumented_Penpal;
   --  The penpal representing the user

   procedure Mainloop;
   --  Enter the mainloop

end Evoluted_Pkg;
