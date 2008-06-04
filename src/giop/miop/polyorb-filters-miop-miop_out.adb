------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--        P O L Y O R B . F I L T E R S . M I O P . M I O P _ O U T         --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2003-2007, Free Software Foundation, Inc.          --
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

--  MIOP filter for data which arrive from a GIOP Session

with PolyORB.Filters.Iface;
with PolyORB.Log;
with PolyORB.Parameters;
with PolyORB.Representations.CDR.Common;
with PolyORB.Types;

package body PolyORB.Filters.MIOP.MIOP_Out is

   use PolyORB.Buffers;
   use PolyORB.Components;
   use PolyORB.Filters.Iface;
   use PolyORB.Log;

   package L is new PolyORB.Log.Facility_Log ("polyorb.filters.miop.miop_out");
   procedure O (Message : String; Level : Log_Level := Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
     renames L.Enabled;
   pragma Unreferenced (C); --  For conditional pragma Debug

   ------------
   -- Create --
   ------------

   procedure Create
     (Fact     : access MIOP_Out_Factory;
      MIOP_Out :    out Filter_Access)
   is
      pragma Warnings (Off);
      pragma Unreferenced (Fact);
      pragma Warnings (On);

      use PolyORB.Parameters;

      Res : constant Filter_Access := new MIOP_Out_Filter;
   begin
      MIOP_Out_Filter (Res.all).MIOP_Buff := new Buffer_Type;

      --  read the max MIOP packet size in conf file
      MIOP_Out_Filter (Res.all).Max_Size := Types.Unsigned_Short
        (Get_Conf
         ("miop", "polyorb.miop.max_message_size",
          Default_Max_MIOP_Message_Size));
      MIOP_Out := Res;
   end Create;

   ----------
   -- Copy --
   ----------

   --  Copy data between two buffer

   procedure Copy
     (From : access Buffer_Type;
      To   : access Buffer_Type;
      Len  :        Types.Unsigned_Short);
   pragma Inline (Copy);

   procedure Copy
     (From : access Buffer_Type;
      To   : access Buffer_Type;
      Len  :        Types.Unsigned_Short)
   is
      use PolyORB.Representations.CDR.Common;
      Temp : Types.Octet;
   begin
      for J in 1 .. Integer (Len) loop
         Temp := Unmarshall (From);
         Marshall (To, Temp);
      end loop;
   end Copy;

   --------------------
   -- Handle_Message --
   --------------------

   function Handle_Message
     (F : access MIOP_Out_Filter;
      S : Components.Message'Class) return Components.Message'Class
   is
      use PolyORB.Types;

      Res : Components.Null_Message;
   begin
      if S in Data_Out then
         --  the GIOP layer MUST send only one Data_Out for each GIOP packet
         declare
            D : Data_Out renames Data_Out (S);
            --  Length of GIOP packet
            L : constant Types.Unsigned_Short
              := Types.Unsigned_Short (Length (D.Out_Buf));
            H : MIOP_Header;
         begin
            pragma Debug (O ("Encapsulate GIOP data in a MIOP Packet,"
                             & L'Img
                             & " bytes"));
            --  Create the request Unique Id
            H.Unique_Id := Generate_Unique_Id;
            pragma Debug (O ("Unique Id :"
                             & To_Standard_String (H.Unique_Id)));

            --  Calculate the packet size
            H.Packet_Size := Types.Unsigned_Short (MIOP_Header_Size)
              + Types.Unsigned_Short (Length (H.Unique_Id) + 1);
            while (H.Packet_Size mod 8) /= 0 loop
               H.Packet_Size := H.Packet_Size + 1;
            end loop;

            --  Rewind GIOP Buffer before copying data
            Rewind (D.Out_Buf);

            --  Test if packet need to be fragmented
            if H.Packet_Size + L <= F.Max_Size then
               --  Normal packet,  no fragmentation
               H.Packet_Size := H.Packet_Size + L;
               H.Collect_Mode := False;
               H.Packet_Number := 0;
               H.Packet_Total := 1;

               --  Create MIOP packet
               Marshall_MIOP_Header (F.MIOP_Buff, H);
               Copy (D.Out_Buf, F.MIOP_Buff, L);

               --  Size check
               pragma Assert (Types.Unsigned_Short (CDR_Position (F.MIOP_Buff))
                             = H.Packet_Size);
               pragma Debug (O ("Send MIOP Message, size : "
                                & H.Packet_Size'Img));

               --  Send packet
               Emit_No_Reply (F.Lower, Data_Out'(Out_Buf => F.MIOP_Buff));
               Release_Contents (F.MIOP_Buff.all);
            else
               --  Fragmenting data
               declare
                  --  Header size for all packets
                  Header_Size  : constant Types.Unsigned_Short
                    := H.Packet_Size;
                  --  Number of fragment
                  Packet_Total : Types.Unsigned_Short
                    := L / (F.Max_Size - Header_Size);
               begin
                  if L mod (F.Max_Size - Header_Size) /= 0 then
                     Packet_Total := Packet_Total + 1;
                  end if;

                  pragma Debug (O ("Fragmenting MIOP packet, size : "
                                   & Types.Unsigned_Short
                                   (Header_Size + L)'Img
                                   & ", need "
                                   & Packet_Total'Img
                                   & " packets, max "
                                   & Types.Unsigned_Short
                                   (F.Max_Size - Header_Size)'Img
                                   & " bytes per packet"));

                  --  Prepare Header Data
                  H.Collect_Mode := True;
                  H.Packet_Total := Types.Unsigned_Long (Packet_Total);
                  for J in 0 .. Packet_Total - 1 loop
                     H.Packet_Number := Types.Unsigned_Long (J);
                     if J /= (Packet_Total - 1) then
                        H.Packet_Size := F.Max_Size;
                     else
                        H.Packet_Size :=
                          Types.Unsigned_Short (Remaining (D.Out_Buf))
                          + Header_Size;
                     end if;

                     --  Prepare fragment
                     Marshall_MIOP_Header (F.MIOP_Buff, H);
                     Copy (D.Out_Buf,
                           F.MIOP_Buff,
                           H.Packet_Size - Header_Size);

                     pragma Debug (O ("Send MIOP Fragment, number :"
                                      & H.Packet_Number'Img
                                      & ", size :"
                                      & H.Packet_Size'Img
                                      & ", payload :"
                                      & Stream_Element_Offset
                                      (H.Packet_Size - Header_Size)'Img));
                     --  Size check
                     pragma Assert (Types.Unsigned_Short
                                      (CDR_Position (F.MIOP_Buff))
                                    = H.Packet_Size);
                     --  Send fragment
                     Emit_No_Reply (F.Lower,
                                    Data_Out'(Out_Buf => F.MIOP_Buff));
                     Release_Contents (F.MIOP_Buff.all);
                  end loop;
               end;
            end if;
         end;

      else
         return Filters.Handle_Message (Filters.Filter (F.all)'Access, S);
      end if;

      return Res;
   end Handle_Message;

end PolyORB.Filters.MIOP.MIOP_Out;
