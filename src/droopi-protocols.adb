--  Support for object method invocation protocols.

--  $Id$

with Ada.Unchecked_Deallocation;

package body Droopi.Protocols is

   procedure Free is new Ada.Unchecked_Deallocation
     (Session'Class, Session_Access);

   procedure Destroy_Session (S : in out Session_Access) is
   begin
      Free (S);
   end Destroy_Session;

   procedure Signal_Data (SC : access Session_Channel) is
   begin
      Handle_Data (SC.Session);
   end Signal_Data;

   procedure Signal_Connection_Closed (SC : access Session_Channel) is
   begin
      Handle_Connection_Closed (SC.Session);
   end Signal_Connection_Closed;

end Droopi.Protocols;
