------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--              P O L Y O R B . F I L T E R S . S L I C E R S               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2001-2010, Free Software Foundation, Inc.          --
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

--  A filter that slices a stream into a set of known-length messages.

with PolyORB.Filters.Iface;
with PolyORB.Log;

package body PolyORB.Filters.Slicers is

   use Ada.Streams;

   use PolyORB.Buffers;
   use PolyORB.Components;
   use PolyORB.Filters.Iface;
   use PolyORB.Log;

   package L is new PolyORB.Log.Facility_Log ("polyorb.filters.slicers");
   procedure O (Message : String; Level : Log_Level := Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
     renames L.Enabled;

   ------------
   -- Create --
   ------------

   procedure Create
     (Fact   : access Slicer_Factory;
      Slicer :    out Filter_Access)
   is
      pragma Warnings (Off);
      pragma Unreferenced (Fact);
      pragma Warnings (On);

      Res : constant Filter_Access := new Slicer_Filter;
   begin
      Slicer_Filter (Res.all).Data_Expected := 0;
      Slicer := Res;
   end Create;

   --------------------
   -- Handle_Message --
   --------------------

   function Handle_Message
     (F : access Slicer_Filter;
      S :        Components.Message'Class)
     return Components.Message'Class
   is
      Res : Components.Null_Message;
   begin
      if S in Data_Expected'Class then
         declare
            DEM : Data_Expected renames Data_Expected (S);
         begin
            pragma Debug (C, O ("Expecting" & DEM.Max'Img
                             & " bytes."));

            pragma Assert (True
              and then F.Data_Expected = 0
              and then F.In_Buf = null
              and then DEM.Max /= 0
              and then DEM.In_Buf /= null);

            F.Data_Expected := DEM.Max;
            F.Initial_Data_Expected := DEM.Max;
            F.In_Buf := DEM.In_Buf;
            F.Buffer_Length := Length (F.In_Buf.all);

            return Emit
              (F.Lower,
               Data_Expected'
               (Max => F.Data_Expected, In_Buf => F.In_Buf));
         end;

      elsif S in Data_Indication then
         declare
            Data_Received : constant Stream_Element_Count
              := Data_Indication (S).Data_Amount;

         begin
            pragma Debug (C, O ("Expected" & F.Data_Expected'Img
                             & " bytes, received"
                             & Data_Received'Img));
            if F.In_Buf = null
              or else Data_Received > F.Data_Expected
            then
               raise Program_Error;
               --  This exception will be propagated to the ORB.
            end if;

            pragma Assert
              (Data_Received = Length (F.In_Buf.all) - F.Buffer_Length);
            --  Integrity check: Receive_Buffer must have increased
            --  Length (F.In_Buf) by exactly the amount of data received.

            F.Data_Expected := F.Data_Expected - Data_Received;
            F.Buffer_Length := Length (F.In_Buf.all);

            if F.Data_Expected = 0 then
               declare
                  Total_Data_Amount : Stream_Element_Count;
               begin
                  if F.Initial_Data_Expected = 0 then
                     Total_Data_Amount := Data_Received;
                  else
                     Total_Data_Amount := F.Initial_Data_Expected;
                     F.Initial_Data_Expected := 0;
                  end if;

                  F.In_Buf := null;
                  return Emit
                    (F.Upper,
                     Data_Indication'
                     (Data_Amount => Total_Data_Amount));
               end;
            else
               pragma Debug (C, O ("Expecting" & F.Data_Expected'Img
                                & " further bytes."));
               Emit_No_Reply
                 (F.Lower,
                  Data_Expected'
                  (Max => F.Data_Expected, In_Buf => F.In_Buf));
            end if;
         end;

      else
         return Filters.Handle_Message (Filters.Filter (F.all)'Access, S);
      end if;

      return Res;
   end Handle_Message;

end PolyORB.Filters.Slicers;
