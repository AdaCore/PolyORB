--  A communication filter (a transport Data_Unit handler/forwarder).

--  $Id$

with Ada.Tags;
pragma Warnings (Off, Ada.Tags);
--  Only used within pragma Debug.

with Droopi.Log;

package body Droopi.Components is

   use Ada.Tags;
   use Droopi.Log;
   use Component_Seqs;

   package L is new Droopi.Log.Facility_Log ("droopi.components");
   procedure O (Message : in String; Level : Log_Level := Debug)
     renames L.Output;

   procedure Connect
     (Port : out Component_Access;
      Target : Component_Access) is
   begin
      Port := Target;
   end Connect;

   function Emit
     (Port : Component_Access;
      Msg    : Message'Class)
     return Message'Class is
   begin
      pragma Debug (O ("Sending message " & External_Tag (Msg'Tag)
                       & " to target " & External_Tag (Port.all'Tag)));
      if Port /= null then
         return Handle_Message (Port, Msg);
      else
         raise Unhandled_Message;
      end if;
   end Emit;

   procedure Emit_No_Reply
     (Port : Component_Access;
      Msg    : Message'Class)
   is
      Reply : constant Message'Class
        := Emit (Port, Msg);
      pragma Warnings (Off, Reply);
      --  Reply must be a Null_Message, and is ignored.
   begin
      pragma Assert (Reply in Null_Message);
      null;
   end Emit_No_Reply;

   procedure Subscribe
     (G      : in out Group;
      Target : Component_Access) is
   begin
      pragma Assert (Target /= null);
      Append (G.Members, Target);
   end Subscribe;

   procedure Unsubscribe
     (G      : in out Group;
      Target : Component_Access)
   is
      Members : constant Element_Array
        := To_Element_Array (G.Members);
   begin
      for I in Members'Range loop
         if Members (I) = Target then
            Delete (Source  => G.Members,
                    From    => 1 + I - Members'First,
                    Through => I + I - Members'First);
            return;
         end if;
      end loop;
   end Unsubscribe;

   function Handle_Message
     (Grp : access Multicast_Group;
      Msg : Message'Class)
     return Message'Class
   is
      Members : constant Element_Array
        := To_Element_Array (Grp.Members);
      Handled : Boolean := False;
      Nothing : Null_Message;
   begin
      for I in Members'Range loop
         begin
            Emit_No_Reply (Members (I), Msg);
            Handled := True;
         exception
            when Unhandled_Message =>
               null;
            when others =>
               raise;
         end;
      end loop;

      if Handled then
         return Nothing;
      else
         raise Unhandled_Message;
      end if;
   end Handle_Message;

   function Handle_Message
     (Grp : access Anycast_Group;
      Msg : Message'Class)
     return Message'Class
   is
      Members : constant Element_Array
        := To_Element_Array (Grp.Members);
   begin
      for I in Members'Range loop
         begin
            declare
               Reply : constant Message'Class
                 := Handle_Message (Members (I), Msg);
            begin
               return Reply;
            end;
         exception
            when Unhandled_Message =>
               null;
            when others =>
               raise;
         end;
      end loop;
      raise Unhandled_Message;
   end Handle_Message;

end Droopi.Components;
