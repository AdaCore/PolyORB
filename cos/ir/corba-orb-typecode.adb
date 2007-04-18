------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                   C O R B A . O R B . T Y P E C O D E                    --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 1999-2007, Free Software Foundation, Inc.          --
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

package body CORBA.ORB.Typecode is

   function Create_Struct_Tc
     (Id      : in CORBA.RepositoryId;
      Name    : in CORBA.Identifier;
      Members : in CORBA.Repository_Root.StructMemberSeq)
     return CORBA.TypeCode.Object
   is
      Result : CORBA.TypeCode.Object;
      package SMS renames
        CORBA.Repository_Root.IDL_SEQUENCE_CORBA_StructMember;
   begin
      Result := CORBA.TypeCode.Internals.To_CORBA_Object
                  (PolyORB.Any.TypeCode.TC_Struct);
      CORBA.Internals.Add_Parameter
        (Result, To_Any (CORBA.String (Name)));
      CORBA.Internals.Add_Parameter
        (Result, To_Any (CORBA.String (Id)));
      declare
         Memb_Array : SMS.Element_Array
           := SMS.To_Element_Array (SMS.Sequence (Members));
      begin
         for I in Memb_Array'Range loop
            CORBA.Internals.Add_Parameter
              (Result, To_Any (Memb_Array (I).IDL_Type));
            CORBA.Internals.Add_Parameter
              (Result, To_Any (CORBA.String (Memb_Array (I).Name)));
         end loop;
      end;
      return Result;
   end Create_Struct_Tc;

   function Create_Union_Tc
     (Id                 : in CORBA.RepositoryId;
      Name               : in CORBA.Identifier;
      Discriminator_Type : in CORBA.TypeCode.Object;
      Members            : in CORBA.Repository_Root.UnionMemberSeq)
     return CORBA.TypeCode.Object
   is
      Result : CORBA.TypeCode.Object;
      package UMS renames
        CORBA.Repository_Root.IDL_SEQUENCE_CORBA_UnionMember;
   begin
      Result := CORBA.TypeCode.Internals.To_CORBA_Object
                  (PolyORB.Any.TypeCode.TC_Union);
      CORBA.Internals.Add_Parameter
        (Result, To_Any (CORBA.String (Name)));
      CORBA.Internals.Add_Parameter
        (Result, To_Any (CORBA.String (Id)));
      CORBA.Internals.Add_Parameter
        (Result, To_Any (Discriminator_Type));
      declare
         Memb_Array : UMS.Element_Array
           := UMS.To_Element_Array (UMS.Sequence (Members));
      begin
         for I in Memb_Array'Range loop
            CORBA.Internals.Add_Parameter
              (Result, To_Any (Memb_Array (I).IDL_Type));
            CORBA.Internals.Add_Parameter
              (Result, To_Any (Memb_Array (I).Label));
            CORBA.Internals.Add_Parameter
              (Result, To_Any (CORBA.String (Memb_Array (I).Name)));
         end loop;
      end;
      return Result;
   end Create_Union_Tc;

   function Create_Enum_Tc
     (Id      : in CORBA.RepositoryId;
      Name    : in CORBA.Identifier;
      Members : in CORBA.Repository_Root.EnumMemberSeq)
     return CORBA.TypeCode.Object
   is
      Result : CORBA.TypeCode.Object;
      package EMS renames
        CORBA.Repository_Root.IDL_SEQUENCE_CORBA_Identifier;
      Memb_Array : EMS.Element_Array
        := EMS.To_Element_Array (EMS.Sequence (Members));
   begin
      Result := CORBA.TypeCode.Internals.To_CORBA_Object
                  (PolyORB.Any.TypeCode.TC_Enum);
      CORBA.Internals.Add_Parameter
        (Result, To_Any (CORBA.String (Name)));
      CORBA.Internals.Add_Parameter
        (Result, To_Any (CORBA.String (Id)));
      for I in Memb_Array'Range loop
         CORBA.Internals.Add_Parameter
           (Result, To_Any (CORBA.String (Memb_Array (I))));
      end loop;
      return Result;
   end Create_Enum_Tc;

   function Create_Alias_Tc
     (Id            : in CORBA.RepositoryId;
      Name          : in CORBA.Identifier;
      Original_Type : in CORBA.TypeCode.Object) return CORBA.TypeCode.Object is
   begin
      return CORBA.TypeCode.Internals.Build_Alias_TC
        (Name => CORBA.String (Name), Id => CORBA.String (Id),
         Parent => Original_Type);
   end Create_Alias_Tc;

   function Create_Exception_Tc
     (Id      : in CORBA.RepositoryId;
      Name    : in CORBA.Identifier;
      Members : in CORBA.Repository_Root.StructMemberSeq)
     return CORBA.TypeCode.Object
   is
      Result : CORBA.TypeCode.Object;
      package SMS renames
        CORBA.Repository_Root.IDL_SEQUENCE_CORBA_StructMember;
   begin
      Result := CORBA.TypeCode.Internals.To_CORBA_Object
                  (PolyORB.Any.TypeCode.TC_Except);
      CORBA.Internals.Add_Parameter
        (Result, To_Any (CORBA.String (Name)));
      CORBA.Internals.Add_Parameter
        (Result, To_Any (CORBA.String (Id)));
      declare
         Memb_Array : SMS.Element_Array
           := SMS.To_Element_Array (SMS.Sequence (Members));
      begin
         for I in Memb_Array'Range loop
            CORBA.Internals.Add_Parameter
              (Result, To_Any (Memb_Array (I).IDL_Type));
            CORBA.Internals.Add_Parameter
              (Result, To_Any (CORBA.String (Memb_Array (I).Name)));
         end loop;
      end;
      return Result;
   end Create_Exception_Tc;

   function Create_Interface_Tc
     (Id   : in CORBA.RepositoryId;
      Name : in CORBA.Identifier)
     return CORBA.TypeCode.Object
   is
      Result : CORBA.TypeCode.Object;
   begin
      Result := CORBA.TypeCode.Internals.To_CORBA_Object
                  (PolyORB.Any.TypeCode.TC_Object);
      CORBA.Internals.Add_Parameter
        (Result, To_Any (CORBA.String (Name)));
      CORBA.Internals.Add_Parameter
        (Result, To_Any (CORBA.String (Id)));
      return Result;
   end Create_Interface_Tc;

   function Create_String_Tc
     (Bound : in CORBA.Unsigned_Long)
     return CORBA.TypeCode.Object
   is
      Result : CORBA.TypeCode.Object;
   begin
      Result := CORBA.TypeCode.Internals.To_CORBA_Object
                  (PolyORB.Any.TypeCode.TC_String);
      CORBA.Internals.Add_Parameter (Result, To_Any (Bound));
      return Result;
   end Create_String_Tc;

   function Create_Wstring_Tc
     (Bound : in CORBA.Unsigned_Long)
     return CORBA.TypeCode.Object
   is
      Result : CORBA.TypeCode.Object;
   begin
      Result := TC_Wide_String;
      CORBA.Internals.Add_Parameter (Result, To_Any (Bound));
      return Result;
   end Create_Wstring_Tc;

   function Create_Fixed_Tc
     (IDL_Digits : in CORBA.Unsigned_Short;
      Scale  : in CORBA.Short)
     return CORBA.TypeCode.Object
   is
      Result : CORBA.TypeCode.Object;
   begin
      Result := CORBA.TypeCode.Internals.To_CORBA_Object
                  (PolyORB.Any.TypeCode.TC_Fixed);
      CORBA.Internals.Add_Parameter (Result, To_Any (IDL_Digits));
      CORBA.Internals.Add_Parameter (Result, To_Any (Scale));
      return Result;
   end Create_Fixed_Tc;

   function Create_Sequence_Tc
     (Bound       : in CORBA.Unsigned_Long;
      Elementtype : in CORBA.TypeCode.Object)
     return CORBA.TypeCode.Object
   is
      Result : CORBA.TypeCode.Object;
   begin
      Result := CORBA.TypeCode.Internals.To_CORBA_Object
                  (PolyORB.Any.TypeCode.TC_Sequence);
      CORBA.Internals.Add_Parameter (Result, To_Any (Bound));
      CORBA.Internals.Add_Parameter (Result, To_Any (Elementtype));
      return Result;
   end Create_Sequence_Tc;

   function Create_Recursive_Sequence_Tc
     (Bound  : in CORBA.Unsigned_Long;
      Offset : in CORBA.Unsigned_Long)
     return CORBA.TypeCode.Object
   is
      pragma Warnings (Off);
      pragma Unreferenced (Bound, Offset);
      pragma Warnings (On);
   begin
      return TC_Null;
   end Create_Recursive_Sequence_Tc;

   function Create_Array_Tc
     (Length       : in CORBA.Unsigned_Long;
      Element_Type : in CORBA.TypeCode.Object)
     return CORBA.TypeCode.Object
   is
      Result : CORBA.TypeCode.Object;
   begin
      Result := CORBA.TypeCode.Internals.To_CORBA_Object
                  (PolyORB.Any.TypeCode.TC_Array);
      CORBA.Internals.Add_Parameter (Result, To_Any (Length));
      CORBA.Internals.Add_Parameter (Result, To_Any (Element_Type));
      return Result;
   end Create_Array_Tc;

   function Create_Value_Tc
     (Id            : in CORBA.RepositoryId;
      Name          : in CORBA.Identifier;
      Type_Modifier : in CORBA.ValueModifier;
      Concrete_Base : in CORBA.TypeCode.Object;
      Members       : in CORBA.Repository_Root.ValueMemberSeq)
     return CORBA.TypeCode.Object
   is
      Result : CORBA.TypeCode.Object;

      package VMS renames CORBA.Repository_Root.IDL_SEQUENCE_CORBA_ValueMember;

   begin
      Result := CORBA.TypeCode.Internals.To_CORBA_Object
                  (PolyORB.Any.TypeCode.TC_Value);
      CORBA.Internals.Add_Parameter
        (Result, To_Any (CORBA.String (Name)));
      CORBA.Internals.Add_Parameter
        (Result, To_Any (CORBA.String (Id)));
      CORBA.Internals.Add_Parameter
        (Result, To_Any (CORBA.Short (Type_Modifier)));
      CORBA.Internals.Add_Parameter (Result, To_Any (Concrete_Base));

      declare
         Memb_Array : constant VMS.Element_Array :=
                        VMS.To_Element_Array (VMS.Sequence (Members));
      begin
         for I in Memb_Array'Range loop
            CORBA.Internals.Add_Parameter
              (Result, To_Any (CORBA.Short (Memb_Array (I).IDL_Access)));
            CORBA.Internals.Add_Parameter
              (Result, To_Any (Memb_Array (I).IDL_Type));
            CORBA.Internals.Add_Parameter
              (Result, To_Any (CORBA.String (Memb_Array (I).Name)));
         end loop;
      end;
      return Result;
   end Create_Value_Tc;

   function Create_Value_Box_Tc
     (Id         : in CORBA.RepositoryId;
      Name       : in CORBA.Identifier;
      Boxed_Type : in CORBA.TypeCode.Object)
     return CORBA.TypeCode.Object
   is
      Result : CORBA.TypeCode.Object;
   begin
      Result := CORBA.TypeCode.Internals.To_CORBA_Object
                  (PolyORB.Any.TypeCode.TC_Valuebox);
      CORBA.Internals.Add_Parameter
        (Result, To_Any (CORBA.String (Name)));
      CORBA.Internals.Add_Parameter
        (Result, To_Any (CORBA.String (Id)));
      CORBA.Internals.Add_Parameter
        (Result, To_Any (Boxed_Type));
      return Result;
   end Create_Value_Box_Tc;

   function Create_Native_Tc
     (Id   : in CORBA.RepositoryId;
      Name : in CORBA.Identifier)
     return CORBA.TypeCode.Object
   is
      Result : CORBA.TypeCode.Object;
   begin
      Result := CORBA.TypeCode.Internals.To_CORBA_Object
                  (PolyORB.Any.TypeCode.TC_Native);
      CORBA.Internals.Add_Parameter
        (Result, To_Any (CORBA.String (Name)));
      CORBA.Internals.Add_Parameter
        (Result, To_Any (CORBA.String (Id)));
      return Result;
   end Create_Native_Tc;

   function Create_Recursive_Tc
     (Id   : in CORBA.RepositoryId)
     return CORBA.TypeCode.Object
   is
      pragma Warnings (Off);
      pragma Unreferenced (Id);
      pragma Warnings (On);
   begin
      return TC_Null;
   end Create_Recursive_Tc;

   function Create_Abstract_Interface_Tc
     (Id   : in CORBA.RepositoryId;
      Name : in CORBA.Identifier) return CORBA.TypeCode.Object
   is
   begin
      return CORBA.TypeCode.Internals.To_CORBA_Object
        (PolyORB.Any.TypeCode.Build_Complex_TC (Tk_Abstract_Interface,
           (0 => PolyORB.Any.To_Any (PolyORB.Types.String (Name)),
            1 => PolyORB.Any.To_Any (PolyORB.Types.String (Id)))));
   end Create_Abstract_Interface_Tc;

end CORBA.ORB.Typecode;
