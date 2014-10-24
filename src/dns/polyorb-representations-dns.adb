------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--          P O L Y O R B . R E P R E S E N T A T I O N S . D N S           --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2002-2013, Free Software Foundation, Inc.          --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.                                               --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Streams;

with PolyORB.DNS.Helper;
with PolyORB.Initialization;
with PolyORB.Log;
with PolyORB.Utils;
with PolyORB.Utils.Buffers;
with PolyORB.Utils.Strings;

pragma Elaborate_All (PolyORB.Utils.Buffers);

package body PolyORB.Representations.DNS is

   use Ada.Streams;

   use PolyORB.Any.TypeCode;
   use PolyORB.DNS.Helper;
   use PolyORB.Errors;
   use PolyORB.Log;
   use PolyORB.Utils;
   use PolyORB.Utils.Buffers;

   package L is new PolyORB.Log.Facility_Log ("polyorb.representations.dns");
   procedure O (Message : String; Level : Log_Level := Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
     renames L.Enabled;

   -----------------------
   -- Marshall_From_Any --
   -----------------------

   procedure Marshall_From_Any
     (Buffer : Buffer_Access; Argument : Any.Any;
      Is_Reply : Types.Boolean) is
      current_Seq : rrSequence;
      current_rr : RR;
   begin
      pragma Debug (C, O ("Marshall_From_Any: enter"));
      current_Seq := From_Any (Argument);
      for J in 1 .. Length (current_Seq) loop
         current_rr := Get_Element (current_Seq, J);
         Marshall_DNS_String (Buffer, To_Standard_String (current_rr.rr_name));

         case current_rr.rr_type is
            when A =>
               Marshall (Buffer, A_Code);
               Marshall (Buffer, Default_Class_Code);
               if Is_Reply then
                  Marshall (Buffer, current_rr.TTL);
                  Marshall (Buffer, current_rr.data_length);
                  Marshall (Buffer, Get_Element
                         (current_rr.rr_data.a_address, 1));
                  Marshall (Buffer, Get_Element
                         (current_rr.rr_data.a_address, 2));
                  Marshall (Buffer, Get_Element
                         (current_rr.rr_data.a_address, 3));
                  Marshall (Buffer, Get_Element
                         (current_rr.rr_data.a_address, 4));
               end if;

            when PTR =>
               Marshall (Buffer, PTR_Code);
               Marshall (Buffer, Default_Class_Code);
               if Is_Reply then
                  Marshall (Buffer, current_rr.TTL);
                  Marshall (Buffer, current_rr.data_length);
                  Marshall_DNS_String (Buffer,
                   To_Standard_String (current_rr.rr_data.rr_answer));
               end if;

            when TXT =>
               Marshall (Buffer, TXT_Code);
               Marshall (Buffer, Default_Class_Code);
               if Is_Reply then
                  Marshall (Buffer, current_rr.TTL);
                  Marshall (Buffer, current_rr.data_length);
                  Marshall_TXT_String (Buffer,
                   To_Standard_String (current_rr.rr_data.rr_answer));
               end if;

            when SRV =>
               Marshall (Buffer, SRV_Code);
               Marshall (Buffer, Default_Class_Code);
               if Is_Reply then
                  Marshall (Buffer, current_rr.TTL);
                  Marshall (Buffer, current_rr.data_length);
                  Marshall (Buffer, current_rr.rr_data.srv_data.priority);
                  Marshall (Buffer, current_rr.rr_data.srv_data.weight);
                  Marshall (Buffer, current_rr.rr_data.srv_data.port);
                  Marshall_DNS_String (Buffer,
                   To_Standard_String (current_rr.rr_data.srv_data.target));
               end if;

            when others =>
               null;
         end case;
      end loop;
      pragma Debug (C, O ("Marshall_From_Any: leave"));
   end Marshall_From_Any;

   -----------------------
   -- Unmarshall_To_Any --
   -----------------------

   procedure Unmarshall_To_Any
     (Buffer   : Buffer_Access;
      Arg      : Any.Any;
      Length   : Integer;
      Is_Reply : Types.Boolean)
   is
      Request_Class     : Types.Unsigned_Short;
      pragma Unreferenced (Request_Class);

      Request_Type_Code : Types.Unsigned_Short;
      current_rr        : RR;
      current_Seq       : rrSequence := To_Sequence (Length);
   begin
      for J in 1 .. Length loop
         current_rr.rr_name := Unmarshall_DNS_String (Buffer);
         Request_Type_Code := Unmarshall (Buffer);
         case Request_Type_Code is
            when A_Code =>
               current_rr.rr_type := A;

            when PTR_Code =>
               current_rr.rr_type := PTR;

            when TXT_Code =>
               current_rr.rr_type := TXT;

            when SRV_Code =>
               current_rr.rr_type := SRV;

            when others =>
               --  Should not happen for now
               raise DNS_Error;
         end case;

         Request_Class := Unmarshall (Buffer);
         if not Is_Reply then
            return;
         end if;
         current_rr.TTL := Unmarshall (Buffer);
         current_rr.data_length := Unmarshall (Buffer);

         --  Part specific to each RR type

         declare
            rr_d : RR_Data (current_rr.rr_type);
         begin
            case current_rr.rr_type is
               when SRV =>
                  rr_d.srv_data.priority := Unmarshall (Buffer);
                  rr_d.srv_data.weight   := Unmarshall (Buffer);
                  rr_d.srv_data.port     := Unmarshall (Buffer);
                  rr_d.srv_data.target   := Unmarshall_DNS_String (Buffer);

               when A =>
                  rr_d.a_address :=
                    IDL_AT_Sequence_4_octet
                      (IDL_SEQUENCE_4_octet.To_Sequence
                           (IDL_SEQUENCE_4_octet.Element_Array'(
                            Unmarshall (Buffer),
                            Unmarshall (Buffer),
                            Unmarshall (Buffer),
                            Unmarshall (Buffer))));
               when TXT =>
                  pragma Debug (C, O ("Message is a TXT record"));
                  rr_d.rr_answer := Unmarshall_TXT_String
                    (Buffer, current_rr.data_length);
                  pragma Debug (C, O (To_Standard_String (rr_d.rr_answer)));
               when others =>
                  rr_d.rr_answer := Unmarshall_DNS_String (Buffer);
            end case;
            current_rr.rr_data := rr_d;
            Replace_Element (current_Seq, Integer (J), current_rr);
         end;
      end loop;

      Copy_Any_Value (Arg, To_Any (current_Seq));
      pragma Debug (C, O ("After Copy_Any_Value"));
   end Unmarshall_To_Any;

   --------------
   -- Marshall --
   --------------

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : PolyORB.Types.Boolean)
   is
   begin
      pragma Debug (C, O ("Marshall (Boolean) : enter"));
      Marshall
        (Buffer, PolyORB.Types.Octet'(PolyORB.Types.Boolean'Pos (Data)));
      pragma Debug (C, O ("Marshall (Boolean) : end"));
   end Marshall;

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : PolyORB.Types.Octet)
   is
   begin
      pragma Debug (C, O ("Marshall (Octet) : enter"));
      Align_Marshall_Copy (Buffer, (1 => Stream_Element
                           (PolyORB.Types.Octet'(Data))), Align_1);
      pragma Debug (C, O ("Marshall (Octet) : end"));
   end Marshall;

   ---------------------------
   -- Marshall_Latin_1_Char --
   ---------------------------

   procedure Marshall_Latin_1_Char
     (Buffer : access Buffer_Type;
      Data   : PolyORB.Types.Char)
   is
   begin
      pragma Debug (C, O ("Marshall (Char) : enter"));
      Marshall (Buffer, PolyORB.Types.Octet'(PolyORB.Types.Char'Pos (Data)));
      pragma Debug (C, O ("Marshall (Char) : end"));
   end Marshall_Latin_1_Char;

   -----------------------------
   -- Unmarshall_Latin_1_Char --
   -----------------------------

   function Unmarshall_Latin_1_Char
     (Buffer : access Buffer_Type) return PolyORB.Types.Char
   is
   begin
      pragma Debug (C, O ("Unmarshall (Char) : enter & end"));
      return PolyORB.Types.Char'Val
        (PolyORB.Types.Octet'(Unmarshall (Buffer)));
   end Unmarshall_Latin_1_Char;

   -----------------------------
   -- Marshall_Latin_1_String --
   -----------------------------

   procedure Marshall_Latin_1_String
     (Buffer : access Buffer_Type;
      Data   : Standard.String)
   is
      Str : Stream_Element_Array (1 .. Data'Length);
      for Str'Address use Data'Address;
      pragma Import (Ada, Str);

   begin
      pragma Debug (C, O ("Marshall (String) : enter:" & Data'Length'Img));
      if Data'Length = 0 then
         return;
      end if;

      Marshall (Buffer, PolyORB.Types.Octet'(Data'Length));
      Align_Marshall_Copy (Buffer, Str);
      pragma Debug (C, O ("Marshall (String) : end"));
   end Marshall_Latin_1_String;

   procedure Marshall_Latin_1_String
     (Buffer : access Buffer_Type;
      Data   : PolyORB.Types.String)
   is
   begin
      pragma Debug (C, O ("Marshall (PolyORB.Types.String) : enter"));
      Marshall_Latin_1_String
        (Buffer, PolyORB.Types.To_Standard_String (Data));
      pragma Debug (C, O ("Marshall (PolyORB.Types.String) : end"));
   end Marshall_Latin_1_String;

   -------------------------
   -- Marshall_DNS_String --
   -------------------------

   procedure Marshall_DNS_String
     (Buffer : access Buffer_Type;
      Data   : Standard.String)
   is
      S       : String renames Data;
      Index   : Integer;
      Index2  : Integer;
      Label : PolyORB.Types.String;
   begin
      pragma Debug (C, O ("Marshall_DNS_String: enter"));
      Index := S'First;
      Index2 := Find (S, Index, '.');
      while Index2 > Index loop
         Label := To_PolyORB_String (S (Index .. Index2 - 1));
         Marshall_Latin_1_String (Buffer, Label);
         pragma Debug (C, O ("Marshall DNS string :label " &
           To_Standard_String (Label)));
         Index := Index2 + 1;
         Index2 := Find (S, Index, '.');
      end loop;
      Marshall (Buffer, Types.Octet (0));
      pragma Debug (C, O ("Marshall_DNS_String: end"));
   end Marshall_DNS_String;

   -------------------------
   -- Marshall_TXT_String --
   -------------------------

   procedure Marshall_TXT_String
     (Buffer : access Buffer_Type;
      Data   : Standard.String)
   is
      S       : String renames Data;
      Index   : Integer;
      Index2  : Integer;
      Label : PolyORB.Types.String;

   begin
      pragma Debug (C, O ("Marshall_TXT_String: enter"));
      Index := S'First;
      Index2 := Find (S, Index, '\') + 1;
      while Index2 > Index and then Index /= S'Last + 3   loop
         Label := To_PolyORB_String (S (Index .. Index2 - 2));
         Marshall_Latin_1_String (Buffer, Label);
         pragma Debug (C, O ("Marshall TXT string :label " &
           To_Standard_String (Label)));
         Index := Index2 + 1;
         Index2 := Find (S, Index, '\') + 1;
         pragma Debug (C, O ("Index2=" & Index2'Img & " > Index="
           & Index'Img));
      end loop;
      pragma Debug (C, O ("Marshall_TXT_String: end"));
   end Marshall_TXT_String;

   ---------------------------
   -- Unmarshall_TXT_String --
   ---------------------------

   function Unmarshall_TXT_String
     (Buffer      : access Buffer_Type;
      Data_Length : PolyORB.Types.Unsigned_Short) return PolyORB.Types.String
   is
      Length         : PolyORB.Types.Octet := Unmarshall (Buffer);
      Current_Length : PolyORB.Types.Unsigned_Short := 0;
      Label          : Types.String := To_PolyORB_String ("");
   begin
      pragma Debug
        (C, O ("Unmarshall_TXT_String: enter, Length =" & Length'Img));

      if Length = 0 then
         pragma Debug (C, O ("Unmarshall TXT: returning empty"));
         return To_PolyORB_String ("");
      end if;

      while Data_Length > Current_Length loop
         declare
            Equiv  : String (1 .. Natural (Length));
         begin
            for J in Equiv'Range loop
               Equiv (J) := Character'Val
                 (PolyORB.Types.Char'Pos (Unmarshall_Latin_1_Char (Buffer)));
            end loop;
            Label := Label & To_PolyORB_String (Equiv);
            pragma Debug (C, O ("Unmarshall DNS (String): -> " &
              To_Standard_String (Label)));
            Current_Length := Current_Length +
              To_Standard_String (Label)'Length;
         end;
         if Data_Length > Current_Length then
            Length := Unmarshall (Buffer);
            Label := Label & "\.";
            Current_Length := Current_Length + 2;
         end if;
      end loop;
      pragma Debug (C, O ("Unmarshall (String): -> " &
        To_Standard_String (Label)));
      return Label;
   end Unmarshall_TXT_String;

   ---------------------------
   -- Unmarshall_DNS_String --
   ---------------------------

   function Unmarshall_DNS_String
     (Buffer : access Buffer_Type) return PolyORB.Types.String
   is
      Length : PolyORB.Types.Octet
        := Unmarshall (Buffer);
      Label : Types.String := To_PolyORB_String ("");
   begin
      pragma Debug (C, O ("Unmarshall (String): enter"));
      pragma Debug (C, O ("Unmarshall (String): length is " &
                    PolyORB.Types.Octet'Image (Length)));
      if Length = 0 then
         pragma Debug (C, O ("Unmarshall (String): returning empty"));
         return To_PolyORB_String ("");
      end if;
      while Length /= Types.Octet (0) loop
         declare
            Equiv  : String (1 .. Natural (Length));
         begin
            for J in Equiv'Range loop
               Equiv (J) := Character'Val
                 (PolyORB.Types.Char'Pos (Unmarshall_Latin_1_Char (Buffer)));
            end loop;
            Label := Label & To_PolyORB_String (Equiv);
            pragma Debug (C, O ("Unmarshall DNS (String): -> " &
              To_Standard_String (Label)));
         end;
         Length := Unmarshall (Buffer);
         pragma Debug (C, O ("Unmarshall (String): length is " &
             PolyORB.Types.Octet'Image (Length)));
         if Length /= Types.Octet (0) then
            Label := Label & ".";
         end if;
      end loop;
      pragma Debug (C, O ("Unmarshall (String): -> " &
        To_Standard_String (Label)));
      return Label;
   end Unmarshall_DNS_String;

   -------------------------------
   -- Unmarshall_Latin_1_String --
   -------------------------------

   function Unmarshall_Latin_1_String
     (Buffer : access Buffer_Type)
     return Standard.String
   is
      Length : constant PolyORB.Types.Unsigned_Short
        := Unmarshall (Buffer);
      Equiv  : String (1 .. Natural (Length) - 1);

   begin
      pragma Debug (C, O ("Unmarshall (String): enter"));
      pragma Debug (C, O ("Unmarshall (String): length is " &
                    PolyORB.Types.Unsigned_Short'Image (Length)));

      if Length = 0 then
         return "";
      end if;

      for J in Equiv'Range loop
         Equiv (J) := Character'Val
           (PolyORB.Types.Char'Pos (Unmarshall_Latin_1_Char (Buffer)));
      end loop;

      if Character'Val
           (PolyORB.Types.Char'Pos (Unmarshall_Latin_1_Char (Buffer)))
        /= ASCII.NUL
      then
         raise Constraint_Error;
      end if;

      pragma Debug (C, O ("Unmarshall (String): -> " & Equiv));

      return Equiv;
   end Unmarshall_Latin_1_String;

   function Unmarshall_Latin_1_String
     (Buffer : access Buffer_Type)
     return PolyORB.Types.String
   is
   begin
      return
        PolyORB.Types.To_PolyORB_String (Unmarshall_Latin_1_String (Buffer));
   end Unmarshall_Latin_1_String;

   ------------------------------------
   -- Unmarshall-by-copy subprograms --
   ------------------------------------

   function Unmarshall
     (Buffer : access Buffer_Type) return PolyORB.Types.Boolean
   is
   begin
      pragma Debug (C, O ("Unmarshall (Boolean) : enter & end"));
      return PolyORB.Types.Boolean'Val
        (PolyORB.Types.Octet'(Unmarshall (Buffer)));
   end Unmarshall;

   package DNS_Octet is
     new Align_Transfer_Elementary (T => PolyORB.Types.Octet);
   function Unmarshall
     (Buffer : access Buffer_Type) return PolyORB.Types.Octet
      renames DNS_Octet.Unmarshall;

   package DNS_Unsigned_Long is
     new Align_Transfer_Elementary (T => PolyORB.Types.Unsigned_Long,
                                    With_Alignment => False);
   function Unmarshall
     (Buffer : access Buffer_Type) return PolyORB.Types.Unsigned_Long
      renames DNS_Unsigned_Long.Unmarshall;

   procedure Marshall
     (Buffer : access Buffer_Type; Data : PolyORB.Types.Unsigned_Long)
      renames DNS_Unsigned_Long.Marshall;

   package DNS_Unsigned_Long_Long is
     new Align_Transfer_Elementary (T => PolyORB.Types.Unsigned_Long_Long);
   function Unmarshall
     (Buffer : access Buffer_Type) return PolyORB.Types.Unsigned_Long_Long
      renames DNS_Unsigned_Long_Long.Unmarshall;

   procedure Marshall
     (Buffer : access Buffer_Type; Data : PolyORB.Types.Unsigned_Long_Long)
      renames DNS_Unsigned_Long_Long.Marshall;

   package DNS_Unsigned_Short is
     new Align_Transfer_Elementary (T => PolyORB.Types.Unsigned_Short,
                                    With_Alignment => False);
   procedure Marshall
     (Buffer : access Buffer_Type; Data : PolyORB.Types.Unsigned_Short)
      renames DNS_Unsigned_Short.Marshall;

   function Unmarshall
     (Buffer : access Buffer_Type) return PolyORB.Types.Unsigned_Short
      renames DNS_Unsigned_Short.Unmarshall;

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : PolyORB.Types.Identifier)
   is
   begin
      pragma Debug (C, O ("Marshall (Identifier) : enter"));
      Marshall_Latin_1_String (Buffer, PolyORB.Types.String (Data));
      pragma Debug (C, O ("Marshall (Identifier) : end"));
   end Marshall;

   function Unmarshall
     (Buffer : access Buffer_Type)
     return PolyORB.Types.Identifier
   is
   begin
      pragma Debug (C, O ("Unmarshall (Identifier) : enter & end"));
      return PolyORB.Types.Identifier
        (Types.String'(Unmarshall_Latin_1_String (Buffer)));
   end Unmarshall;

   procedure Initialize;
   procedure Initialize is
   begin
      null;
   end Initialize;

   use PolyORB.Initialization;
   use PolyORB.Initialization.String_Lists;
   use PolyORB.Utils.Strings;

begin
   Register_Module
     (Module_Info'
      (Name      => +"representations.dns",
       Conflicts => Empty,
       Depends   => Empty,
       Provides  => Empty,
       Implicit  => False,
       Init      => Initialize'Access,
       Shutdown  => null));
end PolyORB.Representations.DNS;
