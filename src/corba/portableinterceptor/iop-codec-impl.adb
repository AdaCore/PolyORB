------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                       I O P . C O D E C . I M P L                        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2004 Free Software Foundation, Inc.             --
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

--  $Id$

with Ada.Streams;
with Ada.Unchecked_Deallocation;

with PolyORB.Any;
with PolyORB.Buffers;
with PolyORB.Exceptions;
with PolyORB.Representations.CDR.Common;

package body IOP.Codec.Impl is

   use Ada.Streams;
   use PolyORB.Buffers;
   use PolyORB.Exceptions;
   use PolyORB.Representations.CDR;
   use PolyORB.Representations.CDR.Common;

   procedure Free is
      new Ada.Unchecked_Deallocation
      (CDR_Representation'Class, CDR_Representation_Access);

   function To_Sequence
     (Item : in Encapsulation)
     return IDL_Sequence_Octet.Sequence;

   function To_Encapsulation
     (Item : in IDL_Sequence_Octet.Sequence)
     return Encapsulation;

   ------------
   -- Decode --
   ------------

   function Decode
     (Self : access Object;
      Data : in     IDL_Sequence_Octet.Sequence)
     return CORBA.Any
   is
      Data_Enc : aliased Encapsulation := To_Encapsulation (Data);
      Buffer   : Buffer_Access := new Buffer_Type;
      Result   : PolyORB.Any.Any;

   begin
      Decapsulate (Data_Enc'Access, Buffer);
      Result := Unmarshall (Buffer, Self.Representation.all);
      Release (Buffer);

      return CORBA.Internals.To_CORBA_Any (Result);
   end Decode;

   ------------------
   -- Decode_Value --
   ------------------

   function Decode_Value
     (Self : access Object;
      Data : in     IDL_Sequence_Octet.Sequence;
      TC   : in     CORBA.TypeCode.Object)
     return CORBA.Any
   is
      Data_Enc : aliased Encapsulation := To_Encapsulation (Data);
      Buffer   : Buffer_Access := new Buffer_Type;
      Error    : Error_Container;
      Result   : PolyORB.Any.Any
        := PolyORB.Any.Get_Empty_Any
        (CORBA.TypeCode.Internals.To_PolyORB_Object (TC));

   begin
      Decapsulate (Data_Enc'Access, Buffer);
      Unmarshall_To_Any (Self.Representation.all, Buffer, Result, Error);
      Release (Buffer);

      if Found (Error) then
         Catch (Error);
         raise PolyORB.Not_Implemented;
         --  XXX Handling of errors must be ivestigated.
      end if;

      return CORBA.Internals.To_CORBA_Any (Result);
   end Decode_Value;

   ------------
   -- Encode --
   ------------

   function Encode
     (Self : access Object;
      Data : in     CORBA.Any)
     return IDL_Sequence_Octet.Sequence
   is
      Buffer : Buffer_Access := new Buffer_Type;
      Result : IDL_Sequence_Octet.Sequence;

   begin
      Start_Encapsulation (Buffer);
      Marshall
        (Buffer,
         Self.Representation.all,
         CORBA.Internals.To_PolyORB_Any (Data));
      Result := To_Sequence (Encapsulate (Buffer));
      Release (Buffer);

      return Result;
   end Encode;

   ------------------
   -- Encode_Value --
   ------------------

   function Encode_Value
     (Self : access Object;
      Data : in     CORBA.Any)
     return IDL_Sequence_Octet.Sequence
   is
      Buffer : Buffer_Access := new Buffer_Type;
      Error  : Error_Container;
      Result : IDL_Sequence_Octet.Sequence;

   begin
      Start_Encapsulation (Buffer);
      Marshall_From_Any
        (Self.Representation.all,
         Buffer,
         CORBA.Internals.To_PolyORB_Any (Data),
         Error);

      if Found (Error) then
         Release (Buffer);
         Catch (Error);
         raise PolyORB.Not_Implemented;
         --  XXX Handling of errors must be ivestigated.
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
      Representation : in     CDR_Representation_Access)
   is
   begin
      Self.Representation := Representation;
   end Init;

   ----------
   -- Is_A --
   ----------

   function Is_A
     (Self            : access Object;
      Logical_Type_Id : in     Standard.String)
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
     (Item : in IDL_Sequence_Octet.Sequence)
      return Encapsulation
   is
      Result : Encapsulation
        (1 .. Stream_Element_Offset (IDL_Sequence_Octet.Length (Item)));

   begin
      for J in Result'Range loop
         Result (J) :=
           Stream_Element (IDL_Sequence_Octet.Element_Of (Item, Integer (J)));
      end loop;

      return Result;
   end To_Encapsulation;

   -----------------
   -- To_Sequence --
   -----------------

   function To_Sequence
     (Item : in Encapsulation)
     return IDL_Sequence_Octet.Sequence
   is
      Result : IDL_Sequence_Octet.Sequence;

   begin
      for J in Item'Range loop
         IDL_Sequence_Octet.Append (Result, CORBA.Octet (Item (J)));
      end loop;

      return Result;
   end To_Sequence;

end IOP.Codec.Impl;
