------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                 P O L Y O R B . F I L T E R S . H T T P                  --
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

--  $Id$

with PolyORB.Filters.Interface;
with PolyORB.Log;
pragma Elaborate_All (PolyORB.Log);
with PolyORB.Opaque;

package body PolyORB.Filters.HTTP is

   use Ada.Streams;

   use PolyORB.Buffers;
   use PolyORB.Filters.Interface;
   use PolyORB.Log;

   package L is new PolyORB.Log.Facility_Log ("polyorb.filters.http");
   procedure O (Message : in String; Level : Log_Level := Debug)
     renames L.Output;

   procedure Create
     (Fact   : access HTTP_Filter_Factory;
      Filt   : out Filter_Access)
   is
      Res : constant Filter_Access := new HTTP_Filter;
   begin
      Filt  := Res;
   end Create;

   function Handle_Message
     (F : access HTTP_Filter;
      S : Components.Message'Class)
     return Components.Message'Class
   is
      Res : Components.Null_Message;
   begin
      if S in Data_Expected then
         declare
            DEM : Data_Expected renames Data_Expected (S);
         begin
            --  pragma Debug (O ("Expecting" & DEM.Max'Img

            if DEM.Max = 0 then
               F.Expected_Size_Fixed := False;
               F.Data_Expected := 1;
            else
               F.Expected_Size_Fixed := True;
               F.Data_Expected := DEM.Max;
            end if;

            F.In_Buf := DEM.In_Buf;
            F.Buffer_Length := Length (F.In_Buf);

            return Emit
              (F.Lower,
               Data_Expected'
               (Max => F.Data_Expected, In_Buf => F.In_Buf));
         end;

      elsif S in Data_Indication then
         if F.Expected_Size_Fixed = True then
            declare
               Data_Received : constant Stream_Element_Count
                 := Length (F.In_Buf) - F.Buffer_Length;
            begin
               pragma Debug (O ("Expected" & F.Data_Expected'Img
                             & " bytes, received"
                             & Data_Received'Img));

               if F.In_Buf = null
                  or else Data_Received > F.Data_Expected
               then
                  raise Unexpected_Data;
                  --  This exception will be propagated to the ORB.
               end if;

               F.Data_Expected := F.Data_Expected - Data_Received;
               F.Buffer_Length := Length (F.In_Buf);

               if F.Data_Expected = 0 then
                  F.In_Buf := null;
                  return Emit
                    (F.Upper,
                      Data_Indication'(Root_Data_Unit with null record));
               else
                  pragma Debug (O ("Expecting" & F.Data_Expected'Img
                                & " further bytes."));
                  Emit_No_Reply
                    (F.Lower,
                     Data_Expected'
                     (Max => F.Data_Expected, In_Buf => F.In_Buf));
               end if;
            end;
         else
            if F.In_Buf = null then
               raise Unexpected_Data;
            else
               pragma Debug
                 (O ("Data received, checking for end of headers."));

               if Length (F.In_Buf) >= 4 then
                  declare
                     Last_Four_Bytes : PolyORB.Opaque.Opaque_Pointer;
                     CRLF_CRLF : constant Stream_Element_Array
                       := (Stream_Element (Character'Pos (ASCII.CR)),
                           Stream_Element (Character'Pos (ASCII.LF)),
                           Stream_Element (Character'Pos (ASCII.CR)),
                           Stream_Element (Character'Pos (ASCII.LF)));
                     LFB_Position : constant Stream_Element_Offset
                       := CDR_Position (F.In_Buf) + Length (F.In_Buf) - 4;
                  begin
                     pragma Debug
                       (O ("Extracting last 4 bytes of buffer at position"
                           & LFB_Position'Img));

                     Extract_Data
                       (F.In_Buf, Last_Four_Bytes, 4,
                        Use_Current => False,
                        At_Position => LFB_Position);
                     pragma Debug (O ("done."));
                     if Last_Four_Bytes.Zone
                       (Last_Four_Bytes.Offset .. Last_Four_Bytes.Offset + 3)
                       = CRLF_CRLF
                     then
                        F.In_Buf := null;
                        pragma Debug (O ("Found end of headers!"));
                        return Emit
                          (F.Upper,
                           Data_Indication'(Root_Data_Unit with null record));
                     end if;
                  end;
               end if;

               pragma Debug (O ("End of headers not seen yet."));
               Emit_No_Reply
                 (F.Lower,
                  Data_Expected'
                  (Max => F.Data_Expected, In_Buf => F.In_Buf));

            end if;
         end if;

      elsif False
        or else S in Connect_Indication
        or else S in Disconnect_Indication
        or else S in Set_Server
      then
         return Emit (F.Upper, S);
      elsif S in Data_Out then
         return Emit (F.Lower, S);
      else
         raise Unhandled_Message;
      end if;

      return Res;
   end Handle_Message;

end PolyORB.Filters.HTTP;
