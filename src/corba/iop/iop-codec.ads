------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                            I O P . C O D E C                             --
--                                                                          --
--                                 S p e c                                  --
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

with Ada.Exceptions;

with CORBA.Object;

package IOP.Codec is

   type Local_Ref is new CORBA.Object.Ref with null record;

   --  InvalidTypeForEncoding exception

   InvalidTypeForEncoding : exception;

   type InvalidTypeForEncoding_Members is
     new CORBA.IDL_Exception_Members with null record;

   procedure Get_Members
     (From : in     Ada.Exceptions.Exception_Occurrence;
      To   :    out InvalidTypeForEncoding_Members);

   --  FormatMismatch exception

   FormatMismatch : exception;

   type FormatMismatch_Members is
     new CORBA.IDL_Exception_Members with null record;

   procedure Get_Members
     (From : in     Ada.Exceptions.Exception_Occurrence;
      To   :    out FormatMismatch_Members);

   --  TypeMismatch exception

   TypeMismatch : exception;

   type TypeMismatch_Members is
     new CORBA.IDL_Exception_Members with null record;

   procedure Get_Members
     (From : in     Ada.Exceptions.Exception_Occurrence;
      To   :    out TypeMismatch_Members);

   --  Codec API

   function Encode
     (Self : in Local_Ref;
      Data : in CORBA.Any)
     return CORBA.IDL_SEQUENCES.OctetSeq;

   function Decode
     (Self : in Local_Ref;
      Data : in CORBA.IDL_SEQUENCES.OctetSeq)
     return CORBA.Any;

   function Encode_Value
     (Self : in Local_Ref;
      Data : in CORBA.Any)
     return CORBA.IDL_SEQUENCES.OctetSeq;

   function Decode_Value
     (Self : in Local_Ref;
      Data : in CORBA.IDL_SEQUENCES.OctetSeq;
      TC   : in CORBA.TypeCode.Object)
     return CORBA.Any;

   --  Repository Ids

   Repository_Id                        : constant Standard.String
     := "IDL:omg.org/IOP/Codec:1.0";

   Decode_Repository_Id                 : constant Standard.String
     := "IDL:omg.org/IOP/Codec/decode:1.0";

   Decode_Value_Repository_Id           : constant Standard.String
     := "IDL:omg.org/IOP/Codec/decode_value:1.0";

   Encode_Repository_Id                 : constant Standard.String
     := "IDL:omg.org/IOP/Codec/encode:1.0";

   Encode_Value_Repository_Id           : constant Standard.String
     := "IDL:omg.org/IOP/Codec/encode_value:1.0";

   FormatMismatch_Repository_Id         : constant Standard.String
     := "IDL:omg.org/IOP/Codec/FormatMismatch:1.0";

   InvalidTypeForEncoding_Repository_Id : constant Standard.String
     := "IDL:omg.org/IOP/Codec/InvalidTypeForEncoding:1.0";

   TypeMismatch_Repository_Id           : constant Standard.String
     := "IDL:omg.org/IOP/Codec/TypeMismatch:1.0";

end IOP.Codec;
