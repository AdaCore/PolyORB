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

   procedure Handle_SDU (Sess : access Session; S : SDU) is
   begin
      case S.Kind is
         when Connect_Indication =>
            Handle_Connect (Session_Access (Sess));
         when Disconnect_Indication =>
            Handle_Disconnect (Session_Access (Sess));
         when Data_Indication =>
            Handle_Data_Indication (Session_Access (Sess));

         when others =>
            pragma Assert (False);
            null;
      end case;
   end Handle_SDU;

   procedure Expect_Data
     (S      : access Session;
      In_Buf : Buffers.Buffer_Access;
      Max    : Ada.Streams.Stream_Element_Count) is
   begin
      Filters.Handle_SDU
        (Lower (S), SDU'(Kind => Data_Expected,
                         In_Buf => In_Buf, Max => 1024));
   end Expect_Data;


end Droopi.Protocols;
