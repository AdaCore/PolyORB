------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--            P O L Y O R B . A S Y N C H _ E V . S O C K E T S             --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                Copyright (C) 2001 Free Software Fundation                --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
-- not, write to the Free Software Foundation, 59 Temple Place - Suite 330, --
-- Boston, MA 02111-1307, USA.                                              --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
--              PolyORB is maintained by ENST Paris University.             --
--                                                                          --
------------------------------------------------------------------------------

--  An asynchrous event source that is a set of socket descriptors.

--  $Id$

with PolyORB.Constants;

with PolyORB.Log;

package body PolyORB.Asynch_Ev.Sockets is

   use PolyORB.Log;
   use PolyORB.Sockets;

   package L is new PolyORB.Log.Facility_Log
     ("polyorb.asynch_ev.sockets");
   procedure O (Message : in String; Level : Log_Level := Debug)
     renames L.Output;

   procedure Create (AEM : out Socket_Event_Monitor) is
   begin
      Create_Selector (AEM.Selector);
   end Create;

   procedure Destroy (AEM : in out Socket_Event_Monitor) is
   begin
      Close_Selector (AEM.Selector);
   end Destroy;

   procedure Register_Source
     (AEM     : access Socket_Event_Monitor;
      AES     : Asynch_Ev_Source_Access;
      Success : out Boolean) is
   begin
      Success := False;
      if AES.all not in Socket_Event_Source then
         return;
      end if;

      Source_Seqs.Append (AEM.Sources, AES);
      AES.Monitor := Asynch_Ev_Monitor_Access (AEM);
      Success := True;
   end Register_Source;

   procedure Unregister_Source
     (AEM : in out Socket_Event_Monitor;
      AES : Asynch_Ev_Source_Access)
   is
      use Source_Seqs;

      Sources : constant Element_Array
        := To_Element_Array (AEM.Sources);
   begin
      All_Sources :
      for I in Sources'Range loop
         if Sources (I) = AES then
            Delete (Source  => AEM.Sources,
                    From    => 1 + I - Sources'First,
                    Through => 1 + I - Sources'First);
            exit All_Sources;
         end if;
      end loop All_Sources;
   end Unregister_Source;

   function Check_Sources
     (AEM     : access Socket_Event_Monitor;
      Timeout : Duration)
     return AES_Array
   is
      Monitored_Set : constant Source_Seqs.Element_Array
        := Source_Seqs.To_Element_Array (AEM.Sources);
      Result : AES_Array (1 .. Monitored_Set'Length);
      Last   : Integer := 0;

      T : Duration := Timeout;

      S : Socket_Type;
      R_Set : Socket_Set_Type;
      W_Set : Socket_Set_Type;
      Status : Selector_Status;

   begin
      Empty (R_Set);
      Empty (W_Set);
      for I in Monitored_Set'Range loop
         S := Socket_Event_Source (Monitored_Set (I).all).Socket;
         Set (R_Set, S);
         pragma Debug (O ("Monitoring socket" & Image (S)));
      end loop;

      if T = Constants.Forever then
         --  Convert special value of Timeout.
         T := PolyORB.Sockets.Forever;
      end if;

      Check_Selector
        (Selector     => AEM.Selector,
         R_Socket_Set => R_Set,
         W_Socket_Set => W_Set,
         Status       => Status,
         Timeout      => T);

      pragma Debug (O ("Selector returned status "
                       & Status'Img));

      if Status = Completed then
         for I in Monitored_Set'Range loop
            if Is_Set (R_Set, Socket_Event_Source
                         (Monitored_Set (I).all).Socket) then
               pragma Debug
                 (O ("Got event on socket"
                       & Image (Socket_Event_Source
                                  (Monitored_Set (I).all).Socket)));

               Last := Last + 1;
                  Result (Last) := Monitored_Set (I);
            end if;
         end loop;
         pragma Assert (Last >= Result'First);
      end if;

      --  Free the storage space associated with our socket sets.

      Empty (R_Set);
      Empty (W_Set);

      return Result (1 .. Last);

   end Check_Sources;

   procedure Abort_Check_Sources (AEM : Socket_Event_Monitor) is
   begin
      --  XXX check that selector is currently blocking!
      --  (and do it in a thread-safe manner, if applicable!)
      Abort_Selector (AEM.Selector);
   end Abort_Check_Sources;

   function Create_Event_Source
     (Socket : PolyORB.Sockets.Socket_Type)
     return Asynch_Ev_Source_Access
   is
      Result : constant Asynch_Ev_Source_Access
        := new Socket_Event_Source;
   begin
      Socket_Event_Source (Result.all).Socket := Socket;
      return Result;
   end Create_Event_Source;

   function Create_Socket_Event_Monitor
     return Asynch_Ev_Monitor_Access;

   function Create_Socket_Event_Monitor
     return Asynch_Ev_Monitor_Access is
   begin
      return new Socket_Event_Monitor;
   end Create_Socket_Event_Monitor;

   function AEM_Factory_Of (AES : Socket_Event_Source)
     return AEM_Factory is
   begin
      pragma Warnings (Off);
      pragma Unreferenced (AES);
      pragma Warnings (On);
      --  Parameter used only for dispatch.
      return Create_Socket_Event_Monitor'Access;
   end AEM_Factory_Of;

end PolyORB.Asynch_Ev.Sockets;
