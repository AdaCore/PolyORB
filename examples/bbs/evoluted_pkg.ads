with Common; use Common;

package Evoluted_Pkg is

   ----------------------------------------------
   -- Global variables for automatic test mode --
   ----------------------------------------------

   Expected_Messages : Integer;
   --  Number of messages we expect to receive.

   Recv_Done : Boolean := False;
   --  Have we received that many messages?

   Send_Done : Boolean := False;
   --  Have we sent all the messages we wanted to send?

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
