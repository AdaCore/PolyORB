with Common; use Common;

package Server is

   pragma Remote_Call_Interface;
   --  This package can be called remotely

   procedure Post_Message
     (Sender  : in String;
      Message : in String);
   --  Add a message to the BBS service. Sender_Error will be raised if the
   --  sender's name is empty, Message_Error if the message is empty.

   function Number_Of_Messages return Natural;
   --  Return a number of messages that were posted to the BBS

   function Get_Sender (N : Positive) return String;
   --  Return the name of the sender of a particular message. No_Such_Message
   --  will be raised if there is no such message.

   function Get_Message (N : Positive) return String;
   --  Return the content of a particular message. No_Such_Message will be
   --  raised if there is no such message.

   type Penpal_Pointer is access all Penpal_Type'Class;
   --  A Penpal_Pointer can designated any descendent of the Penpal_Type type

   procedure Register (Penpal : in Penpal_Pointer);
   --  Register a penpal in the connected users database. Sender_Error will
   --  be raised if the penpal has not been correctly initialized. If a
   --  penpal with this name has been registered already, then it will be
   --  replaced with the new one (to cover the case where a penpal moves
   --  to another machine for example).

   function Get_Penpal (Name : String) return Penpal_Pointer;
   --  Return the object representing a penpal of a given type, or raise
   --  No_Such_Penpal if no penpal by this name has been registered.

   procedure Broadcast (Sender : in String; Message : in String);
   --  Broadcast a message to every registered penpal

end Server;
