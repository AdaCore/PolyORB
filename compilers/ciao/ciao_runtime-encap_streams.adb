----------------------------------------
--                                    --
--       ----  ---     --  ----       --
--       -      -     - -  -  -       --
--       -      -    ----  -  -       --
--       ----  ---  -   -  ----       --
--                                    --
----------------------------------------
--  CORBA                             --
--  Interface for                     --
--  Ada'95 distributed systems annex  --
--  Objects                           --
----------------------------------------
--  Copyright (c) 1999                --
--  École nationale supérieure des    --
--  télécommunications                --
----------------------------------------

--  A DSA stream based on a CORBA Encapsulation
--  (sequence<octet>).
--  $Id: //depot/ciao/main/ciao_runtime-encap_streams.adb#4 $

package body CIAO_Runtime.Encap_Streams is

   use IDL_SEQUENCE_Octet;

   procedure Set_Seq (St : in out Stream; Ar : Octet_Array) is
   begin
      St.Seq := IDL_Sequence_Octet.To_Sequence (Ar);
      St.Pos := 1;
   end Set_Seq;

   function Get_Seq (St : Stream) return Octet_Array is
   begin
      return To_Element_Array (St.Seq);
   end Get_Seq;

   procedure Read
     (St : in out Stream;
      Item : out Stream_Element_Array;
      Last : out Stream_Element_Offset)
   is
      Read_Length : Integer;
   begin
      if Item'Length > Length (St.Seq) - St.Pos + 1 then
         Read_Length := Length (St.Seq) - St.Pos;
      else
         Read_Length := Item'Length;
      end if;

      if Read_Length <= 0 then
         Last := Item'First - 1;
         return;
      end if;

      declare
         Data : Octet_Array
           := Slice (St.Seq, St.Pos, St.Pos + Read_Length - 1);
      begin
         for I in Data'Range loop
            Item (Stream_Element_Offset (I - Data'First) + Item'First)
              := Stream_Element (Data (I));
         end loop;
      end;

      Last := Item'First + Stream_Element_Offset (Read_Length - 1);
      St.Pos := St.Pos + Read_Length;
   end Read;

   procedure Write
     (St   : in out Stream;
      Item : in Stream_Element_Array) is
   begin
      declare
         Data : Octet_Array (Integer (Item'First) .. Integer (Item'Last));
      begin
         for I in Item'Range loop
            Data (Integer (I)) := CORBA.Octet (Item (I));
         end loop;

         Append (St.Seq, Data);
         St.Pos := St.Pos + Item'Size;
      end;
   end Write;

end CIAO_Runtime.Encap_Streams;
