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
--  $Id: //depot/ciao/main/ciao_runtime-encap_streams.ads#2 $

with Ada.Streams; use Ada.Streams;
with CORBA.Sequences.Unbounded;

package CIAO_Runtime.Encap_Streams is

   package IDL_SEQUENCE_Octet is new CORBA.Sequences.Unbounded (CORBA.Octet);
   subtype Sequence is IDL_SEQUENCE_Octet.Sequence;
   subtype Octet_Array is IDL_SEQUENCE_Octet.Element_Array;

   type Stream is new Ada.Streams.Root_Stream_Type with private;

   procedure Set_Seq (St : in out Stream; Ar : Octet_Array);
   function Get_Seq (St : Stream) return Octet_Array;

private

   type Stream is new Ada.Streams.Root_Stream_Type with record
      Seq : Sequence;
      Pos : Natural := 0;
   end record;

   procedure Read (St : in out Stream;
                   Item : out Stream_Element_Array;
                   Last : out Stream_Element_Offset);
   procedure Write (St : in out Stream;
                    Item : in Stream_Element_Array);


end CIAO_Runtime.Encap_Streams;
