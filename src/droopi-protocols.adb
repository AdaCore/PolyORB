--  Support for object method invocation protocols.

--  $Id$

with Ada.Unchecked_Deallocation;

package body Droopi.Protocols is

   use Droopi.Components;
   use Droopi.Filters.Data_Units;

   procedure Free is new Ada.Unchecked_Deallocation
     (Session'Class, Session_Access);

   procedure Destroy_Session (S : in out Session_Access) is
   begin
      Free (S);
   end Destroy_Session;

   function Handle_Message
     (Sess : access Session;
      S : Components.Message'Class)
     return Boolean is
   begin
      if S in Connect_Indication then
         Handle_Connect (Session_Access (Sess));
      elsif S in Disconnect_Indication then
         Handle_Disconnect (Session_Access (Sess));
      elsif S in Data_Indication then
         Handle_Data_Indication (Session_Access (Sess));
      else
         pragma Assert (False);
         null;
      end if;
      return True;
   end Handle_Message;

   procedure Expect_Data
     (S      : access Session;
      In_Buf : Buffers.Buffer_Access;
      Max    : Ada.Streams.Stream_Element_Count) is
   begin
      Emit (Signal => Lower (S),
            Msg    => Data_Expected'(In_Buf => In_Buf, Max => 1024));
   end Expect_Data;

end Droopi.Protocols;
