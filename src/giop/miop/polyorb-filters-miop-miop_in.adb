------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--           P O L Y O R B . F I L T E R S . M I O P . M I O P _ I N        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--             Copyright (C) 2003 Free Software Foundation, Inc.            --
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
--                PolyORB is maintained by ACT Europe.                      --
--                    (email: sales@act-europe.fr)                          --
--                                                                          --
------------------------------------------------------------------------------

--  MIOP filter for data which arrive from network to ORB
--  this filter MUST be under a GIOP Session

with PolyORB.Filters.Interface;
with PolyORB.Log;
with PolyORB.Protocols.GIOP;

package body PolyORB.Filters.MIOP.MIOP_In is

   use PolyORB.Buffers;
   use PolyORB.Components;
   use PolyORB.Filters.Interface;
   use PolyORB.Log;
   use PolyORB.Protocols.GIOP;

   package L is new PolyORB.Log.Facility_Log ("polyorb.filters.miop.miop_in");
   procedure O (Message : in String; Level : Log_Level := Debug)
     renames L.Output;

   ------------
   -- Create --
   ------------

   procedure Create
     (Fact     : access MIOP_In_Factory;
      MIOP_In :    out Filter_Access)
   is
      pragma Warnings (Off);
      pragma Unreferenced (Fact);
      pragma Warnings (On);

      use PolyORB.Components;

      Res : constant Filter_Access := new MIOP_In_Filter;
   begin
      Set_Allocation_Class (Res.all, Dynamic);
      MIOP_In_Filter (Res.all).In_Buf := null;
      MIOP_In_Filter (Res.all).MIOP_Buf := new Buffer_Type;
      MIOP_In := Res;
   end Create;

   --------------------
   -- Handle_Message --
   --------------------

   function Handle_Message
     (F : access MIOP_In_Filter;
      S : Components.Message'Class)
     return Components.Message'Class
   is
      use PolyORB.Types;

   begin
      if S in Data_Indication then
         case F.State is
            when Wait_MIOP_Header =>
               --  MIOP header received

               --  read MIOP header and unique id length
               Unmarshall_MIOP_Header (F.MIOP_Buf, F.Header);
               F.State := Wait_Unique_Id;
               pragma Debug (O ("Wait for Unique Id, size : "
                                & F.Header.Unique_Id_Size'Img));

               --  Calculate length to read, unique id length + 8 bytes padding
               declare
                  N : Stream_Element_Count
                    := Stream_Element_Count (F.Header.Unique_Id_Size +
                        Types.Unsigned_Long (MIOP_Header_Size));
               begin
                  while N mod 8 /= 0 loop
                     N := N + 1;
                  end loop;

                  --  Calculate data length containing in packet
                  F.Payload := F.Header.Packet_Size - Types.Unsigned_Short (N);
                  pragma Debug (O ("Packet payload : " & F.Payload'Img));

                  --  Calculate data length of unique id + padding
                  N := N - MIOP_Header_Size;
                  return Emit
                    (F.Lower,
                     Data_Expected'(In_Buf => F.MIOP_Buf, Max => N));
               end;

            when Wait_Unique_Id =>
               --  Unique id received
               Unmarshall_Unique_Id (F.MIOP_Buf,
                                     F.Header.Unique_Id_Size,
                                     F.Header.Unique_Id);
               pragma Debug (O ("Unique Id : "
                                & To_Standard_String (F.Header.Unique_Id)));

               Release_Contents (F.MIOP_Buf.all);
               F.State := Wait_GIOP_Data;

               if F.Fragment then
                  --  Test if the received packet is the good packet
                  if not F.Header.Collect_Mode
                    or F.Header.Unique_Id /= F.Old_Header.Unique_Id
                    or F.Header.Packet_Total /= F.Old_Header.Packet_Total
                    or F.Header.Packet_Number /= F.Old_Header.Packet_Number + 1
                  then
                     --  XXX error if a packet is missing
                     raise MIOP_Packet_Error;
                  end if;

                  --  Check size if last fragment
                  pragma Assert
                    (F.Header.Packet_Number + 1 /= F.Header.Packet_Total
                       or else Stream_Element_Offset (F.Payload) = F.Data_Exp);

                  --  Ask for next fragment
                  return Emit
                    (F.Lower,
                     Data_Expected'
                     (In_Buf => F.In_Buf,
                      Max => Stream_Element_Offset (F.Payload)));
               else
                  --  No fragment, ask for data
                  return Emit
                    (F.Lower,
                     Data_Expected'
                     (In_Buf => F.In_Buf,
                      Max => F.Data_Exp));
               end if;

            when Wait_GIOP_Data =>
               --  GIOP data received
               if
                 not F.Fragment and
                 F.Initial_Data_Exp = Remaining (F.In_Buf) - F.Initial_Remain
               then
                  --  All data are here
                  pragma Debug (O ("Send asked data to upper filter"));
                  F.Payload := F.Payload - Types.Unsigned_Short (F.Data_Exp);
                  F.State := Wait_GIOP_Ask;
                  return Emit (F.Upper, S);

               elsif
                 F.Fragment and
                 F.Header.Packet_Number + 1 = F.Header.Packet_Total
               then
                  --  all fragments are here
                  pragma Debug (O ("Fragment received, number"
                                   & F.Header.Packet_Number'Img
                                   & " /"
                                   & Types.Unsigned_Long
                                   (F.Header.Packet_Total - 1)'Img
                                   & ", size :"
                                   & F.Header.Packet_Size'Img
                                   & ", payload :"
                                   & F.Payload'Img));
                  F.Fragment := False;
                  pragma Debug (O ("Send asked data to upper filter"));
                  F.State := Wait_GIOP_Ask;
                  return Emit (F.Upper, S);

               else
                  if not F.Header.Collect_Mode then
                     --  XXX error if a packet is missing
                     raise MIOP_Packet_Error;
                  end if;

                  --  Some fragments left, ask for next
                  pragma Assert (F.Initial_Data_Exp
                                   >= Remaining (F.In_Buf) - F.Initial_Remain);
                  begin
                     F.Data_Exp :=
                       F.Data_Exp - Stream_Element_Offset (F.Payload);
                  exception

                     when Constraint_Error =>
                        raise MIOP_Packet_Error;
                  end;

                  pragma Debug (O ("Fragment received, number"
                                   & F.Header.Packet_Number'Img
                                   & " /"
                                   & Types.Unsigned_Long
                                   (F.Header.Packet_Total - 1)'Img
                                   & ", size :"
                                   & F.Header.Packet_Size'Img
                                   & ", payload :"
                                   & F.Payload'Img));
                  pragma Debug (O ("Bytes left to receive:"
                                   & F.Data_Exp'Img));
                  F.Fragment := True;
                  F.Old_Header := F.Header;
                  F.State := Wait_MIOP_Header;
                  pragma Debug (O ("Wait for MIOP Header"));
                  return Emit (F.Lower,
                               Data_Expected'
                               (In_Buf => F.MIOP_Buf,
                                Max => MIOP_Header_Size));
               end if;

            when others =>
               raise MIOP_Packet_Error;
         end case;

      elsif S in GIOP_Data_Expected then
         declare
            D : GIOP_Data_Expected renames GIOP_Data_Expected (S);
         begin
            F.In_Buf := D.In_Buf;
            F.Data_Exp := D.Max;
            F.Initial_Data_Exp := D.Max;
            F.Initial_Remain := Remaining (F.In_Buf);

            case F.State is
               when Wait_For_GIOP_Layer =>
                  --  GIOP layer ask for next packet
                  pragma Assert (D.State = Expect_Header);
                  F.State := Wait_MIOP_Header;
                  pragma Debug (O ("Wait for MIOP Header"));
                  return Emit (F.Lower,
                               Data_Expected'
                               (In_Buf => F.MIOP_Buf,
                                Max => MIOP_Header_Size));

               when Wait_GIOP_Ask =>

                  if D.State = Expect_Header then
                     --  GIOP layer ask for next packet
                     F.State := Wait_MIOP_Header;
                     pragma Debug (O ("Wait for MIOP Header"));
                     return Emit (F.Lower,
                                  Data_Expected'
                                  (In_Buf => F.MIOP_Buf,
                                   Max => MIOP_Header_Size));

                  else
                     --  GIOP layer ask for data
                     pragma Debug (O ("Upper requests"
                                      & F.Data_Exp'Img
                                      & " bytes"));
                     F.State := Wait_GIOP_Data;

                     --  Test if requested data are in the current packet
                     if F.Data_Exp > Stream_Element_Offset (F.Payload) then
                        --  Not all data are here, fragment mode needed
                        if not F.Header.Collect_Mode then
                           raise MIOP_Packet_Error;
                        end if;
                        return Emit (F.Lower,
                                     Data_Expected'
                                     (In_Buf => F.In_Buf,
                                      Max =>
                                        Stream_Element_Offset (F.Payload)));
                     else
                        --  No fragment, data are in packet
                        return Emit (F.Lower,
                                     Data_Expected'
                                     (In_Buf => F.In_Buf,
                                      Max => F.Data_Exp));
                     end if;
                  end if;

               when others =>
                  raise MIOP_Packet_Error;
            end case;
         end;


      elsif False
        or else S in Connect_Indication
        or else S in Connect_Confirmation
        or else S in Disconnect_Indication
        or else S in Set_Server
      then
         return Emit (F.Upper, S);

      elsif False
        or else S in Data_Out
        or else S in Disconnect_Request
      then
         return Emit (F.Lower, S);

      else
         raise Unhandled_Message;
      end if;
   end Handle_Message;

end PolyORB.Filters.MIOP.MIOP_In;
