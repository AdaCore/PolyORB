------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                           D Y N A M I C A N Y                            --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2005-2006, Free Software Foundation, Inc.          --
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

with CORBA.Forward;
with CORBA.Sequences.Unbounded;

package DynamicAny is

   --  Interface forward declaratios

   package DynAny_Forward is new CORBA.Forward;

   --  FieldName type

   type FieldName is new CORBA.String;

   --  NameValuePair structure

   type NameValuePair is record
      Id    : FieldName;
      Value : CORBA.Any;
   end record;

   --  NameValuePairSeq sequence

   package IDL_SEQUENCE_DynamicAny_NameValuePair is
     new CORBA.Sequences.Unbounded (NameValuePair);

   type NameValuePairSeq is new IDL_SEQUENCE_DynamicAny_NameValuePair.Sequence;

   --  NameDynAnyPair structure

   type NameDynAnyPair is record
      Id    : FieldName;
      Value : DynAny_Forward.Ref;
   end record;

   --  NameDynAnyPairSeq sequence

   package IDL_SEQUENCE_DynamicAny_NameDynAnyPair is
     new CORBA.Sequences.Unbounded (NameDynAnyPair);

   type NameDynAnyPairSeq is
     new IDL_SEQUENCE_DynamicAny_NameDynAnyPair.Sequence;

   --  AnySeq sequence

   package IDL_SEQUENCE_Any is new CORBA.Sequences.Unbounded (CORBA.Any);

   type AnySeq is new IDL_SEQUENCE_Any.Sequence;

   --  DynAnySeq sequence

   package IDL_SEQUENCE_DynamicAny_DynAny_Forward is
     new CORBA.Sequences.Unbounded (DynAny_Forward.Ref);

   type DynAnySeq is new IDL_SEQUENCE_DynamicAny_DynAny_Forward.Sequence;

   --  MustTruncate exception

   MustTruncate : exception;

   type MustTruncate_Members is
     new CORBA.IDL_Exception_Members with null record;

   procedure Get_Members
     (From : Ada.Exceptions.Exception_Occurrence;
      To   :    out MustTruncate_Members);

   --  Repository Ids

   Repository_Id                   : constant Standard.String
     := "IDL:omg.org/DynamicAny:1.0";

   AnySeq_Repository_Id            : constant Standard.String
     := "IDL:omg.org/DynamicAny/AnySeq:1.0";

   DynAnySeq_Repository_Id         : constant Standard.String
     := "IDL:omg.org/DynamicAny/DynAnySeq:1.0";

   FieldName_Repository_Id         : constant Standard.String
     := "IDL:omg.org/DynamicAny/FieldName:1.0";

   MustTruncate_Repository_Id      : constant Standard.String
     := "IDL:omg.org/DynamicAny/MustTruncate:1.0";

   NameDynAnyPair_Repository_Id    : constant Standard.String
     := "IDL:omg.org/DynamicAny/NameDynAnyPair:1.0";

   NameDynAnyPairSeq_Repository_Id : constant Standard.String
     := "IDL:omg.org/DynamicAny/NameDynAnyPairSeq:1.0";

   NameValuePair_Repository_Id     : constant Standard.String
     := "IDL:omg.org/DynamicAny/NameValuePair:1.0";

   NameValuePairSeq_Repository_Id  : constant Standard.String
     := "IDL:omg.org/DynamicAny/NameValuePairSeq:1.0";

end DynamicAny;
