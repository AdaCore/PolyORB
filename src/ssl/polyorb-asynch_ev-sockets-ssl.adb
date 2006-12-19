------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--        P O L Y O R B . A S Y N C H _ E V . S O C K E T S . S S L         --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2005-2006, Free Software Foundation, Inc.          --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
-- not, write to the Free Software Foundation, 51 Franklin Street, Fifth    --
-- Floor, Boston, MA 02111-1301, USA.                                       --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

with PolyORB.Log;

package body PolyORB.Asynch_Ev.Sockets.SSL is

   use PolyORB.Log;
   use PolyORB.Sockets;
   use PolyORB.SSL;

   package L is new PolyORB.Log.Facility_Log ("polyorb.asynch_ev.sockets.ssl");
   procedure O (Message : String; Level : Log_Level := Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
     renames L.Enabled;
   pragma Unreferenced (C); --  For conditional pragma Debug

   type Socket_Event_Monitor_Access is access all Socket_Event_Monitor;

   function Create_SSL_Event_Monitor return Asynch_Ev_Monitor_Access;

   --------------------
   -- AEM_Factory_Of --
   --------------------

   function AEM_Factory_Of (AES : SSL_Event_Source) return AEM_Factory is
      pragma Unreferenced (AES);

   begin
      return Create_SSL_Event_Monitor'Access;
   end AEM_Factory_Of;

   -------------------------
   -- Create_Event_Source --
   -------------------------

   function Create_Event_Source
     (Socket : PolyORB.SSL.SSL_Socket_Type)
     return Asynch_Ev_Source_Access
   is
      Result : constant Asynch_Ev_Source_Access
        := new SSL_Event_Source;
   begin
      SSL_Event_Source (Result.all).SSL_Socket := Socket;
      SSL_Event_Source (Result.all).Socket     := Socket_Of (Socket);
      return Result;
   end Create_Event_Source;

   function Create_Event_Source
     (Socket : PolyORB.Sockets.Socket_Type)
     return Asynch_Ev_Source_Access
   is
      Result : constant Asynch_Ev_Source_Access
        := new SSL_Event_Source;
   begin
      SSL_Event_Source (Result.all).SSL_Socket := No_SSL_Socket;
      SSL_Event_Source (Result.all).Socket     := Socket;
      return Result;
   end Create_Event_Source;

   ------------------------------
   -- Create_SSL_Event_Monitor --
   ------------------------------

   function Create_SSL_Event_Monitor
     return Asynch_Ev_Monitor_Access is
   begin
      return new SSL_Event_Monitor;
   end Create_SSL_Event_Monitor;

   ---------------------
   -- Register_Source --
   ---------------------

   procedure Register_Source
     (AEM     : access SSL_Event_Monitor;
      AES     :        Asynch_Ev_Source_Access;
      Success :    out Boolean) is
   begin
      pragma Debug (O ("Register_Source: enter"));

      Success := False;
      if AES.all not in SSL_Event_Source then
         pragma Debug (O ("Register_Source: leave"));
         return;
      end if;

      Set (AEM.Monitored_Set, SSL_Event_Source (AES.all).Socket);
      Source_Lists.Append (AEM.Sources, AES);
      pragma Debug (O ("Register_Source: Sources'Length:="
                       & Integer'Image (Source_Lists.Length (AEM.Sources))));
      AES.Monitor := Asynch_Ev_Monitor_Access (AEM);

      Success := True;
      pragma Debug (O ("Register_Source: leave"));
   end Register_Source;

   -------------------
   -- Check_Sources --
   -------------------

   function Check_Sources
     (AEM     : access SSL_Event_Monitor;
      Timeout :        Duration)
     return AES_Array
   is
      use Source_Lists;

      Result : AES_Array (1 .. Length (AEM.Sources));
      Last   : Integer := 0;

   begin
      pragma Debug (O ("Check_Sources: enter"));

      --  SSL transport may cache data in the internal buffer, so if cached
      --  data available then adding event source to the result.

      declare
         Iter : Iterator := First (AEM.Sources);

      begin
         while not Source_Lists.Last (Iter) loop
            if SSL_Event_Source (Value (Iter).all.all).SSL_Socket
                 /= No_SSL_Socket
              and then Pending_Length
                (SSL_Event_Source (Value (Iter).all.all).SSL_Socket) /= 0
            then
               Last := Last + 1;
               Result (Last) := Value (Iter).all;

               Clear
                 (AEM.Monitored_Set,
                  SSL_Event_Source (Value (Iter).all.all).Socket);
               Remove (AEM.Sources, Iter);

            else
               Next (Iter);
            end if;

         end loop;
      end;

      --  If at least one event source has cached data, then
      --  immediately return (because checking of sockets may produce
      --  time delay), otherwise, call Check_Sources.

      if Last /= 0 then
         return Result (1 .. Last);

      else
         return Check_Sources (Socket_Event_Monitor_Access (AEM), Timeout);
      end if;
   end Check_Sources;

end PolyORB.Asynch_Ev.Sockets.SSL;
