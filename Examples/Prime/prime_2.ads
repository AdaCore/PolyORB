-- 
-- Copyright (c) Laurent Pautet (ENST, Paris) email : pautet@inf.enst.fr
--

with Types;
use Types;
package Prime_2 is
   pragma Remote_Call_Interface;

   procedure Begin_Session
     (Number  : Std_Number;
      Session : Std_Session);

   pragma Asynchronous (Begin_Session);

   procedure End_Session
     (Session : Std_Session;
      Node    : Std_Node;
      Result  : Boolean);

   pragma Asynchronous (End_Session);

   procedure Initiate
     (Number : Std_Number;
      Node   : out Std_Node; 
      Result : out Boolean);

end Prime_2;
