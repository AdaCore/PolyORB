------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--                 S Y S T E M . G A R L I C . R E P L A Y                  --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision$                             --
--                                                                          --
--         Copyright (C) 1996,1997 Free Software Foundation, Inc.           --
--                                                                          --
-- GARLIC is free software;  you can redistribute it and/or modify it under --
-- terms of the  GNU General Public License  as published by the Free Soft- --
-- ware Foundation;  either version 2,  or (at your option)  any later ver- --
-- sion.  GARLIC is distributed  in the hope that  it will be  useful,  but --
-- WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHANTABI- --
-- LITY or  FITNESS FOR A PARTICULAR PURPOSE.  See the  GNU General Public  --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License  distributed with GARLIC;  see file COPYING.  If  --
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
--               GLADE  is maintained by ACT Europe.                        --
--               (email: glade-report@act-europe.fr)                        --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Streams;
with System.Garlic.Heart;
with System.Garlic.Physical_Location; use System.Garlic.Physical_Location;
with System.Garlic.Protocols; use System.Garlic.Protocols;
with System.Garlic.Debug; use System.Garlic.Debug;
with System.Garlic.Trace; use System.Garlic.Trace;

package body System.Garlic.Replay is

   use type System.RPC.Partition_ID;

   Private_Debug_Key : constant Debug_Key :=
     Debug_Initialize ("GARREP", "(s-garrep): ");
   procedure D
     (Level   : in Debug_Level;
      Message : in String;
      Key     : in Debug_Key := Private_Debug_Key)
     renames Print_Debug_Info;

   ------------
   -- Create --
   ------------

   function Create return System.Garlic.Protocols.Protocol_Access is
      Self : Protocol_Access := new Replay_Protocol;
   begin
      Register_Protocol (Self);
      return Self;
   end Create;

   --------------
   -- Get_Info --
   --------------

   function Get_Info (P : access Replay_Protocol) return String is
   begin
      return "";
   end Get_Info;

   --------------
   -- Get_Name --
   --------------

   function Get_Name (P : access Replay_Protocol) return String is
   begin
      return "replay";
   end Get_Name;

   ----------
   -- Send --
   ----------

   procedure Send
     (Protocol  : access Replay_Protocol;
      Partition : in System.RPC.Partition_ID;
      Data      : access Ada.Streams.Stream_Element_Array) is
   begin
      pragma Debug
         (D (D_Debug, "Send (but do nothing)" & Data'Length'Img & " bytes"));

      --  Send is a no-op since every partition gets its messages from
      --  the trace file.
      null;
   end Send;

   -------------------
   -- Set_Boot_Data --
   -------------------

   procedure Set_Boot_Data
     (Protocol         : access Replay_Protocol;
      Is_Boot_Protocol : in Boolean := False;
      Boot_Data        : in String  := "";
      Is_Master        : in Boolean := False)
   is
   begin
      pragma Assert (Boot_Data = "");
      null;
   end Set_Boot_Data;

   --------------
   -- Shutdown --
   --------------

   procedure Shutdown (Protocol : access Replay_Protocol) is
   begin
      null;
   end Shutdown;

   task body Recorded_Data_Reader is
   begin

      select
         accept Start;
      or
         terminate;
      end select;

      pragma Debug (D (D_Debug, "Task Recorded_Data_Reader started"));

      select
         Heart.Shutdown_Keeper.Wait;
         pragma Debug
           (D (D_Debug, "Recorded_Data_Reader exiting because of " &
               "Shutdown_Keeper"));
         --  Is this an error?
      then abort
         declare
            Is_Last : Boolean;
         begin
            Reader : loop
               --  Read next trace from trace file, sleep
               --  until it's time to deliver and then call
               --  Heart.Has_Arrived with the data read.
               Deliver_Next_Trace (Is_Last);
               exit Reader when Is_Last;
            end loop Reader;
         end;
      end select;

      pragma Debug (D (D_Debug, "End of task Recorded_Data_Reader"));

   end Recorded_Data_Reader;

end System.Garlic.Replay;
