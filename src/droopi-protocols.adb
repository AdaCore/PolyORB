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

end Droopi.Protocols;
