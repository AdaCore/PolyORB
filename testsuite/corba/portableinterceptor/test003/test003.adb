------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                              T E S T 0 0 3                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2004-2005 Free Software Foundation, Inc.           --
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

with CORBA.IDL_SEQUENCES;
with CORBA.ORB;
with IOP.Codec;
with IOP.CodecFactory.Helper;

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

   Append (BE_Stream, 16#00#);
   Append (BE_Stream, 16#AA#);
   Append (BE_Stream, 16#AA#);
   Append (BE_Stream, 16#AA#);
   Append (BE_Stream, 16#00#);
   Append (BE_Stream, 16#00#);
   Append (BE_Stream, 16#00#);
   Append (BE_Stream, 16#01#);

   --  This is unsigned long (1), little endian

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
      if Unsigned_Long'(From_Any (Data)) /= 1 then
         Output ("IOP::Codec::Decode_Value (big endian)", False);
      else
         Output ("IOP::Codec::Decode_Value (big endian)", True);
      end if;
   exception
      when others =>
         Output ("IOP::Codec::Decode_Value (big endian)", False);
   end;

   declare
      Data : Any;

   begin
      Data :=
        IOP.Codec.Decode_Value (Codec, LE_Stream, CORBA.TC_Unsigned_Long);
      if Unsigned_Long'(From_Any (Data)) /= 1 then
         Output ("IOP::Codec::Decode_Value (little endian)", False);
      else
         Output ("IOP::Codec::Decode_Value (little endian)", True);
      end if;
   exception
      when others =>
         Output ("IOP::Codec::Decode_Value (little endian)", False);
   end;

   declare
      Data   : Any := To_Any (Unsigned_Long'(1));
      Stream : OctetSeq;

   begin
      Stream := IOP.Codec.Encode_Value (Codec, Data);
      if Stream = BE_Stream
        or else Stream = LE_Stream
      then
         Output ("IOP::Codec::Encode_Value", True);
      else
         Output ("IOP::Codec::Encode_Value", False);
      end if;
   exception
      when others =>
         Output ("IOP::Codec::Encode_Value", False);
   end;

   declare
      Data   : Any := To_Any (Unsigned_Long'(1));
      Stream : OctetSeq;

   begin
      Stream := IOP.Codec.Encode (Codec, Data);
      if IOP.Codec.Decode (Codec, Stream) /= Data then
         Output ("IOP::Codec::Encode and IOP::Codec::Decode", False);
      else
         Output ("IOP::Codec::Encode and IOP::Codec::Decode", True);
      end if;
   exception
      when others =>
         Output ("IOP::Codec::Encode and IOP::Codec::Decode", False);
   end;

   End_Report;
end Test003;
