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

   procedure Emit
     (Port : Component_Access;
      Msg    : Message'Class)
   is
      Dummy : Boolean;
   begin
      pragma Debug (O ("Sending message " & External_Tag (Msg'Tag)
                       & " to target " & External_Tag (Port.all'Tag)));
      if Port /= null then
         Dummy := Handle_Message (Port, Msg);
      end if;
   end Emit;

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
     return Boolean
   is
      Members : constant Element_Array
        := To_Element_Array (Grp.Members);
      Handled : Boolean := False;
   begin
      for I in Members'Range loop
         Handled := Handled
           or else Handle_Message (Members (I), Msg);
      end loop;
      return Handled;
   end Handle_Message;

   function Handle_Message
     (Grp : access Anycast_Group;
      Msg : Message'Class)
     return Boolean
   is
      Members : constant Element_Array
        := To_Element_Array (Grp.Members);
   begin
      for I in Members'Range loop
         if Handle_Message (Members (I), Msg) then
            return True;
         end if;
      end loop;
      return False;
   end Handle_Message;

end Droopi.Components;
