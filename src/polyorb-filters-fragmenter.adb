------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--           P O L Y O R B . F I L T E R S . F R A G M E N T E R            --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2001-2007, Free Software Foundation, Inc.          --
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

--  Fragmenter filter
--  Fragment data which comes from endpoint whithout read length control
--  For example UDP sockets

with PolyORB.Filters.Iface;
with PolyORB.Log;
with PolyORB.Types;
with PolyORB.Representations.CDR.Common;

package body PolyORB.Filters.Fragmenter is

   use Ada.Streams;

   use PolyORB.Buffers;
   use PolyORB.Components;
   use PolyORB.Filters.Iface;
   use PolyORB.Log;

   package L is new PolyORB.Log.Facility_Log
     ("polyorb.filters.fragmenter");
   procedure O (Message : String; Level : Log_Level := Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
     renames L.Enabled;
   pragma Unreferenced (C); --  For conditional pragma Debug

   ------------
   -- Create --
   ------------

   procedure Create
     (Fact   : access Fragmenter_Factory;
      Fragmenter : out Filter_Access)
   is
      pragma Unreferenced (Fact);

      Res : constant Filter_Access := new Fragmenter_Filter;
   begin
      Fragmenter_Filter (Res.all).Data_Expected := 0;

      --  Create buffer for lower filter

      Fragmenter_Filter (Res.all).Socket_Buf := new Buffer_Type;

      Fragmenter := Res;
   end Create;

   procedure Copy
     (From : access Buffer_Type;
      To   : access Buffer_Type;
      Len  :        Ada.Streams.Stream_Element_Count);
   --  Copy data from From to To, leaving CDR position of To unchanged

   ----------
   -- Copy --
   ----------

   procedure Copy
     (From : access Buffer_Type;
      To   : access Buffer_Type;
      Len  :        Ada.Streams.Stream_Element_Count)
   is
      use PolyORB.Representations.CDR.Common;
      K : constant Stream_Element_Offset := CDR_Position (To);
      Temp : Types.Octet;
   begin
      for J in 1 .. Integer (Len) loop
         Temp := Unmarshall (From);
         Marshall (To, Temp);
      end loop;
      Set_CDR_Position (To, K);
   end Copy;

   ------------------
   -- Process_Data --
   ------------------

   --  this function process data
   --  ask for other data if there is not enough data
   --  or send data to upper fiter

   function Process_Data
     (F : access Fragmenter_Filter)
     return Components.Message'Class;

   function Process_Data
     (F : access Fragmenter_Filter)
     return Components.Message'Class
   is
      --  Data ready in buffer
      Ready : constant Stream_Element_Count
        := Remaining (F.Socket_Buf);
   begin
      if Ready /= 0 then
         --  There is some data ready
         if Ready <= F.Data_Expected then
            --  Copy data to destination
            Copy (F.Socket_Buf, F.In_Buf, Ready);

            if Ready = F.Data_Expected then
               --  Just enough data
               Release_Contents (F.Socket_Buf.all);
               F.Data_Expected := 0;
               F.In_Buf := null;
               pragma Debug (O ("Sending"
                                & F.Initial_Data_Expected'Img
                                & ", buffer empty"));
               --  Send data to upper filter
               return Emit
                 (F.Upper,
                  Data_Indication'
                  (Data_Amount => F.Initial_Data_Expected));
            else
               --  Not enough data
               --  This case must not happend
               raise Constraint_Error;
            end if;
         else
            --  Too much data, fragmenting

            --  Copy data asked by upper layer
            Copy (F.Socket_Buf, F.In_Buf, F.Data_Expected);
            F.Data_Expected := 0;
            F.In_Buf := null;
            pragma Debug (O ("Sending"
                             & F.Initial_Data_Expected'Img
                             & ","
                             & Remaining (F.Socket_Buf)'Img
                             & " bytes remaining in buffer"));

            --  Send data to upper layer
            return Emit
              (F.Upper,
               Data_Indication'
               (Data_Amount => F.Initial_Data_Expected));
         end if;

      else
         --  No data are present, ask for them to lower layer
         pragma Debug (O ("Asking for "
                          & F.Data_Expected'Img
                          & " bytes"));
         return Emit
           (F.Lower,
            Data_Expected'
            (Max => F.Data_Expected, In_Buf => F.Socket_Buf));
      end if;
   end Process_Data;

   --------------------
   -- Handle_Message --
   --------------------

   function Handle_Message
     (F : access Fragmenter_Filter;
      S : Components.Message'Class) return Components.Message'Class
   is
   begin
      if S in Data_Expected'Class then
         declare
            DEM : Data_Expected renames Data_Expected (S);
         begin
            --  Upper layer ask for data
            pragma Debug (O ("Upper filter expects"
                             & DEM.Max'Img
                             & " bytes"));

            if DEM.Max = 0 then
               --  No data asked, return to upper layer
               return Emit
                 (F.Upper,
                  Data_Indication'
                  (Data_Amount => DEM.Max));
            end if;

            pragma Assert (True
                             and then F.Data_Expected = 0
                             and then F.In_Buf = null
                             and then DEM.In_Buf /= null);

            F.Data_Expected := DEM.Max;
            F.Initial_Data_Expected := DEM.Max;
            F.In_Buf := DEM.In_Buf;

            --  Try to satisfy demand
            return Process_Data (F);
         end;

      elsif S in Data_Indication then
         declare
            Data_Received : constant Stream_Element_Count
              := Stream_Element_Count (Data_Indication (S).Data_Amount);

         begin
            --  Some data received
            pragma Debug (O ("Received" & Data_Received'Img & " bytes"));

            --  Try to satisfy demand
            return Process_Data (F);
         end;

      else
         return Filters.Handle_Message (Filters.Filter (F.all)'Access, S);
      end if;
   end Handle_Message;

end PolyORB.Filters.Fragmenter;
