------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                            I O P . C O D E C                             --
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

with PolyORB.Exceptions;

with IOP.Codec.Impl;

package body IOP.Codec is

   ------------
   -- Decode --
   ------------

   function Decode
     (Self : in Local_Ref;
      Data : in IDL_Sequence_Octet.Sequence)
     return CORBA.Any
   is
      Self_Ref : CORBA.Object.Ref := CORBA.Object.Ref (Self);

   begin
      if CORBA.Object.Is_Nil (Self_Ref) then
         CORBA.Raise_Inv_Objref (CORBA.Default_Sys_Member);
      end if;

      return Impl.Decode (Impl.Object_Ptr (Entity_Of (Self)), Data);
   end Decode;

   ------------------
   -- Decode_Value --
   ------------------

   function Decode_Value
     (Self : in Local_Ref;
      Data : in IDL_Sequence_Octet.Sequence;
      TC   : in CORBA.TypeCode.Object)
     return CORBA.Any
   is
      Self_Ref : CORBA.Object.Ref := CORBA.Object.Ref (Self);

   begin
      if CORBA.Object.Is_Nil (Self_Ref) then
         CORBA.Raise_Inv_Objref (CORBA.Default_Sys_Member);
      end if;

      return Impl.Decode_Value (Impl.Object_Ptr (Entity_Of (Self)), Data, TC);
   end Decode_Value;

   ------------
   -- Encode --
   ------------

   function Encode
     (Self : in Local_Ref;
      Data : in CORBA.Any)
      return IDL_Sequence_Octet.Sequence
   is
      Self_Ref : CORBA.Object.Ref := CORBA.Object.Ref (Self);

   begin
      if CORBA.Object.Is_Nil (Self_Ref) then
         CORBA.Raise_Inv_Objref (CORBA.Default_Sys_Member);
      end if;

      return Impl.Encode (Impl.Object_Ptr (Entity_Of (Self)), Data);
   end Encode;

   ------------------
   -- Encode_Value --
   ------------------

   function Encode_Value
     (Self : in Local_Ref;
      Data : in CORBA.Any)
      return IDL_Sequence_Octet.Sequence
   is
      Self_Ref : CORBA.Object.Ref := CORBA.Object.Ref (Self);

   begin
      if CORBA.Object.Is_Nil (Self_Ref) then
         CORBA.Raise_Inv_Objref (CORBA.Default_Sys_Member);
      end if;

      return Impl.Encode_Value (Impl.Object_Ptr (Entity_Of (Self)), Data);
   end Encode_Value;

   -----------------
   -- Get_Members --
   -----------------

   procedure Get_Members
     (From : in     Ada.Exceptions.Exception_Occurrence;
      To   :    out FormatMismatch_Members)
   is
   begin
      PolyORB.Exceptions.User_Get_Members (From, To);
   end Get_Members;

   procedure Get_Members
     (From : in     Ada.Exceptions.Exception_Occurrence;
      To   :    out InvalidTypeForEncoding_Members)
   is
   begin
      PolyORB.Exceptions.User_Get_Members (From, To);
   end Get_Members;

   procedure Get_Members
     (From : in     Ada.Exceptions.Exception_Occurrence;
      To   :    out TypeMismatch_Members)
   is
   begin
      PolyORB.Exceptions.User_Get_Members (From, To);
   end Get_Members;

end IOP.Codec;
