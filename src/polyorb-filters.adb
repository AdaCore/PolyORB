--  A communication filter (a transport Data_Unit handler/forwarder).

--  $Id$

with Ada.Tags;

with PolyORB.Filters.Interface;
with PolyORB.Log;
pragma Elaborate_All (PolyORB.Log);

package body PolyORB.Filters is

   use PolyORB.Log;

   package L is new PolyORB.Log.Facility_Log ("polyorb.filters");
   procedure O (Message : in String; Level : Log_Level := Debug)
     renames L.Output;

   procedure Connect_Lower (F : access Filter; Lower : Component_Access) is
   begin
      Connect (F.Lower, Lower);
   end Connect_Lower;

   function Lower (F : access Filter) return Component_Access is
   begin
      return F.Lower;
   end Lower;

   function Upper (F : access Filter) return Component_Access is
   begin
      return F.Upper;
   end Upper;

   procedure Finalize (F : in out Filter) is
   begin
      if F.Upper /= null then
         pragma Debug
           (O ("Destroying upper of type "
               & Ada.Tags.External_Tag (F.Upper'Tag)));
         Destroy (F.Upper);
         --  XXX WHAT IF F.Upper has not been dynamically allocated?
      end if;
   end Finalize;

   function Handle_Message
     (F : access Factory;
      Msg : Message'Class)
     return Message'Class is
   begin
      if Msg in Interface.Create_Filter_Chain then
         return Interface.Created_Filter_Chain'
           (Filter_Chain => Create_Filter_Chain (F));
      else
         raise Unhandled_Message;
      end if;
   end Handle_Message;

   procedure Chain_Factories (Factories : Factory_Array) is
   begin
      for I in Factories'First .. Factories'Last - 1 loop
         pragma Debug
           (O ("Chaining "
               & Ada.Tags.External_Tag (Factories (I)'Tag)
               & " to "
               & Ada.Tags.External_Tag (Factories (I + 1)'Tag)));
         Connect
           (Factories (I).Upper,
            Component_Access (Factories (I + 1)));
      end loop;

      Factories (Factories'Last).Upper := null;
   end Chain_Factories;

   function Create_Filter_Chain (FChain : access Factory)
     return Filter_Access
   is
      F : Filter_Access;
   begin
      Create (Fact => Factory'Class (FChain.all)'Access, Filt => F);
      pragma Debug (O ("Created filter of type "
                       & Ada.Tags.External_Tag (F'Tag)));
      --  Create new filter.

      if FChain.Upper /= null then
         declare
            Reply : constant Message'Class
              := Emit
              (FChain.Upper,
               Interface.Create_Filter_Chain'(null record));
         begin
            if not (Reply in Interface.Created_Filter_Chain) then
               raise Unhandled_Message;
            end if;

            declare
               Upper : constant Filter_Access
                 := Interface.Created_Filter_Chain
                 (Reply).Filter_Chain;
            begin
               Connect (F.Upper, Component_Access (Upper));
               Connect_Lower (Upper, Component_Access (F));
            end;
         end;
      end if;
      return F;
   end Create_Filter_Chain;

end PolyORB.Filters;
