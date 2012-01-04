------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                              T E S T 0 0 3                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2004-2012, Free Software Foundation, Inc.          --
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

with CORBA.IDL_SEQUENCES;
with CORBA.ORB;
with IOP.Codec;
with IOP.CodecFactory.Helper;

with PolyORB.Buffers;
--  For Host_Order

with PolyORB.Utils.Report;

with PolyORB.Setup.No_Tasking_Server;
pragma Warnings (Off, PolyORB.Setup.No_Tasking_Server);

procedure Test003 is
   use CORBA;
   use CORBA.IDL_SEQUENCES;
   use IOP;
   use PolyORB.Utils.Report;

   Argv      : CORBA.ORB.Arg_List := CORBA.ORB.Command_Line_Arguments;
   Factory   : IOP.CodecFactory.Local_Ref;
   Codec     : IOP.Codec.Local_Ref;
   BE_Stream : OctetSeq;
   LE_Stream : OctetSeq;

begin
   CORBA.ORB.Init (CORBA.ORB.To_CORBA_String ("ORB"), Argv);

   New_Test ("IOP::CodecFactory and IOP::Codec operations");

   begin
      Factory :=
        IOP.CodecFactory.Helper.To_Local_Ref
          (CORBA.ORB.Resolve_Initial_References
           (CORBA.ORB.To_CORBA_String ("CodecFactory")));
      Output ("Getting CodecFactory from Resolve_Initial_References", True);
   exception
      when others =>
         Output
           ("Getting CodecFactory from Resolve_Initial_References", False);
   end;

   begin
      Codec := IOP.CodecFactory.Create_Codec (Factory, (255, 0, 0));
      Output ("Rasing UnknownEncoding for invalid encoding method", False);
   exception
      when IOP.CodecFactory.UnknownEncoding =>
         Output ("Rasing UnknownEncoding for invalid encoding method", True);
      when others =>
         Output ("Rasing UnknownEncoding for invalid encoding method", False);
   end;

   begin
      Codec :=
        IOP.CodecFactory.Create_Codec (Factory, (Encoding_CDR_Encaps, 0, 0));
      Output ("Rasing UnknownEncoding for invalid encoding version", False);
   exception
      when IOP.CodecFactory.UnknownEncoding =>
         Output ("Rasing UnknownEncoding for invalid encoding version", True);
      when others =>
         Output ("Rasing UnknownEncoding for invalid encoding version", False);
   end;

   begin
      Codec :=
        IOP.CodecFactory.Create_Codec (Factory, (Encoding_CDR_Encaps, 1, 2));
         Output ("Create codec for known encoding and version", True);
   exception
      when others =>
         Output ("Create codec for known encoding and version", False);
   end;

   --  This is unsigned long (1), big endian
   --  Bytes marked '16#AA#' are padding

   Append (BE_Stream, 16#00#);
   Append (BE_Stream, 16#AA#);
   Append (BE_Stream, 16#AA#);
   Append (BE_Stream, 16#AA#);
   Append (BE_Stream, 16#00#);
   Append (BE_Stream, 16#00#);
   Append (BE_Stream, 16#00#);
   Append (BE_Stream, 16#01#);

   --  This is unsigned long (1), little endian
   --  Bytes marked '16#AA#' are padding

   Append (LE_Stream, 16#01#);
   Append (LE_Stream, 16#AA#);
   Append (LE_Stream, 16#AA#);
   Append (LE_Stream, 16#AA#);
   Append (LE_Stream, 16#01#);
   Append (LE_Stream, 16#00#);
   Append (LE_Stream, 16#00#);
   Append (LE_Stream, 16#00#);

   declare
      Data : Any;

   begin
      Data :=
        IOP.Codec.Decode_Value (Codec, BE_Stream, CORBA.TC_Unsigned_Long);
      Output ("IOP::Codec::Decode_Value (big endian)",
        Unsigned_Long'(From_Any (Data)) = 1);
   exception
      when others =>
         Output ("IOP::Codec::Decode_Value (big endian)", False);
   end;

   declare
      Data : Any;

   begin
      Data :=
        IOP.Codec.Decode_Value (Codec, LE_Stream, CORBA.TC_Unsigned_Long);
      Output ("IOP::Codec::Decode_Value (little endian)",
        Unsigned_Long'(From_Any (Data)) = 1);
   exception
      when others =>
         Output ("IOP::Codec::Decode_Value (little endian)", False);
   end;

   declare
      Data       : constant Any := To_Any (Unsigned_Long'(1));
      Stream     : OctetSeq;
      Exp_Stream : OctetSeq;

      use PolyORB.Buffers;

   begin
      Stream := IOP.Codec.Encode_Value (Codec, Data);
      case Host_Order is
         when Little_Endian =>
            Exp_Stream := LE_Stream;
         when Big_Endian =>
            Exp_Stream := BE_Stream;
      end case;

      --  Compare Seq with Exp_Stream, ignoring padding bytes (marked as 16#AA#
      --  in Exp_Stream).

      declare
         use CORBA.IDL_SEQUENCES.IDL_SEQUENCE_Octet;
         Bytes     : constant Element_Array := To_Element_Array (Stream);
         Exp_Bytes : constant Element_Array := To_Element_Array (Exp_Stream);
         Ok        : Boolean;
      begin
         if Bytes'First = Exp_Bytes'First
           and then Bytes'Last = Exp_Bytes'Last
         then
            Ok := True;
            for J in Bytes'Range loop
               if Exp_Bytes (J) /= 16#AA#
                 and then Bytes (J) /= Exp_Bytes (J)
               then
                  Ok := False;
               end if;
            end loop;
         else
            Ok := False;
         end if;
         Output ("IOP::Codec::Encode_Value", Ok);
      end;
   exception
      when others =>
         Output ("IOP::Codec::Encode_Value", False);
   end;

   declare
      Data   : constant Any := To_Any (Unsigned_Long'(1));
      Stream : OctetSeq;

   begin
      Stream := IOP.Codec.Encode (Codec, Data);
      Output ("IOP::Codec::Encode and IOP::Codec::Decode",
        IOP.Codec.Decode (Codec, Stream) = Data);
   exception
      when others =>
         Output ("IOP::Codec::Encode and IOP::Codec::Decode", False);
   end;

   End_Report;
end Test003;
