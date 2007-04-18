------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                       I O P . C O D E C . I M P L                        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2004-2007, Free Software Foundation, Inc.          --
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

with Ada.Streams;
with Ada.Unchecked_Deallocation;

with PolyORB.Any;
with PolyORB.Buffers;
with PolyORB.Errors;
with PolyORB.Representations.CDR.Common;

package body IOP.Codec.Impl is

   use Ada.Streams;
   use PolyORB.Buffers;
   use PolyORB.Errors;
   use PolyORB.Representations.CDR;
   use PolyORB.Representations.CDR.Common;

   procedure Free is
      new Ada.Unchecked_Deallocation
      (CDR_Representation'Class, CDR_Representation_Access);

   function To_Sequence
     (Item : Encapsulation)
     return CORBA.IDL_SEQUENCES.OctetSeq;

   function To_Encapsulation
     (Item : CORBA.IDL_SEQUENCES.OctetSeq)
     return Encapsulation;

   ------------
   -- Decode --
   ------------

   function Decode
     (Self : access Object;
      Data : CORBA.IDL_SEQUENCES.OctetSeq) return CORBA.Any
   is
      Data_Enc : aliased Encapsulation := To_Encapsulation (Data);
      Buffer   : Buffer_Access := new Buffer_Type;
      Result   : PolyORB.Any.Any;

   begin
      Decapsulate (Data_Enc'Access, Buffer);
      Result := Unmarshall (Buffer, Self.Representation);
      Release (Buffer);

      return CORBA.Any (Result);
   end Decode;

   ------------------
   -- Decode_Value --
   ------------------

   function Decode_Value
     (Self : access Object;
      Data : CORBA.IDL_SEQUENCES.OctetSeq;
      TC   : CORBA.TypeCode.Object) return CORBA.Any
   is
      Data_Enc : aliased Encapsulation := To_Encapsulation (Data);
      Buffer   : Buffer_Access := new Buffer_Type;
      Error    : Error_Container;
      Result   : PolyORB.Any.Any :=
                   PolyORB.Any.Get_Empty_Any
                     (CORBA.TypeCode.Internals.To_PolyORB_Object (TC));

      use PolyORB.Any;
   begin
      Decapsulate (Data_Enc'Access, Buffer);
      Unmarshall_To_Any
        (Self.Representation,
         Buffer,
         Get_Container (Result).all,
         Error);
      Release (Buffer);

      if Found (Error) then
         Catch (Error);
         raise Program_Error;
         --  XXX Handling of errors must be investigated
      end if;

      return CORBA.Any (Result);
   end Decode_Value;

   ------------
   -- Encode --
   ------------

   function Encode
     (Self : access Object;
      Data : CORBA.Any) return CORBA.IDL_SEQUENCES.OctetSeq
   is
      Buffer : Buffer_Access := new Buffer_Type;
      Result : CORBA.IDL_SEQUENCES.OctetSeq;

   begin
      Start_Encapsulation (Buffer);
      Marshall
        (Buffer,
         Self.Representation,
         PolyORB.Any.Any (Data));
      Result := To_Sequence (Encapsulate (Buffer));
      Release (Buffer);

      return Result;
   end Encode;

   ------------------
   -- Encode_Value --
   ------------------

   function Encode_Value
     (Self : access Object;
      Data : CORBA.Any)
     return CORBA.IDL_SEQUENCES.OctetSeq
   is
      Buffer : Buffer_Access := new Buffer_Type;
      Error  : Error_Container;
      Result : CORBA.IDL_SEQUENCES.OctetSeq;

      use PolyORB.Any;

   begin
      Start_Encapsulation (Buffer);
      Marshall_From_Any
        (Self.Representation,
         Buffer,
         CORBA.Get_Container (Data).all,
         Error);

      if Found (Error) then
         Release (Buffer);
         Catch (Error);
         raise Program_Error;
         --  XXX Handling of errors must be investigated
      end if;

      Result := To_Sequence (Encapsulate (Buffer));

      Release (Buffer);

      return Result;
   end Encode_Value;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (Self : in out Object) is
   begin
      Release (Self.Representation.all);
      Free (Self.Representation);
   end Finalize;

   ----------
   -- Init --
   ----------

   procedure Init
     (Self           : access Object;
      Representation : CDR_Representation_Access)
   is
   begin
      Self.Representation := Representation;
   end Init;

   ----------
   -- Is_A --
   ----------

   function Is_A
     (Self            : access Object;
      Logical_Type_Id : Standard.String)
     return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return CORBA.Is_Equivalent
        (Logical_Type_Id,
         IOP.Codec.Repository_Id)
        or else CORBA.Is_Equivalent
          (Logical_Type_Id,
           "IDL:omg.org/CORBA/Object:1.0");
   end Is_A;

   ----------------------
   -- To_Encapsulation --
   ----------------------

   function To_Encapsulation
     (Item : CORBA.IDL_SEQUENCES.OctetSeq)
      return Encapsulation
   is
      Result : Encapsulation
        (1 .. Stream_Element_Offset (CORBA.IDL_SEQUENCES.Length (Item)));

   begin
      for J in Result'Range loop
         Result (J) :=
           Stream_Element
             (CORBA.IDL_SEQUENCES.Get_Element (Item, Integer (J)));
      end loop;

      return Result;
   end To_Encapsulation;

   -----------------
   -- To_Sequence --
   -----------------

   function To_Sequence
     (Item : Encapsulation)
     return CORBA.IDL_SEQUENCES.OctetSeq
   is
      Result : CORBA.IDL_SEQUENCES.OctetSeq;

   begin
      for J in Item'Range loop
         CORBA.IDL_SEQUENCES.Append (Result, CORBA.Octet (Item (J)));
      end loop;

      return Result;
   end To_Sequence;

end IOP.Codec.Impl;
