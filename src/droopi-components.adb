--  A communication filter (a transport Data_Unit handler/forwarder).

--  $Id$

package body Droopi.Components is

   use Component_Seqs;

   procedure Connect
     (Signal : out Component_Access;
      Target : Component_Access) is
   begin
      Signal := Target;
   end Connect;

   procedure Emit
     (Signal : Component_Access;
      Msg    : Message'Class)
   is
      Dummy : Boolean;
   begin
      if Signal /= null then
         Dummy := Handle_Message (Signal.all, Msg);
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
     (Grp : Multicast_Group;
      Msg : Message'Class)
     return Boolean
   is
      Members : constant Element_Array
        := To_Element_Array (Grp.Members);
      Handled : Boolean := False;
   begin
      for I in Members'Range loop
         Handled := Handled
           or else Handle_Message (Members (I).all, Msg);
      end loop;
      return Handled;
   end Handle_Message;

   function Handle_Message
     (Grp : Anycast_Group;
      Msg : Message'Class)
     return Boolean
   is
      Members : constant Element_Array
        := To_Element_Array (Grp.Members);
   begin
      for I in Members'Range loop
         if Handle_Message (Members (I).all, Msg) then
            return True;
         end if;
      end loop;
      return False;
   end Handle_Message;

end Droopi.Components;
