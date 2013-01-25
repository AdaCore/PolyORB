------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                   C O R B A . O R B . T Y P E C O D E                    --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 1999-2012, Free Software Foundation, Inc.          --
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

package body CORBA.ORB.Typecode is

   function Create_Struct_Tc
     (Id      : CORBA.RepositoryId;
      Name    : CORBA.Identifier;
      Members : CORBA.Repository_Root.StructMemberSeq)
     return CORBA.TypeCode.Object
   is
      Result : CORBA.TypeCode.Object;
      package SMS renames
        CORBA.Repository_Root.IDL_SEQUENCE_CORBA_StructMember;
   begin
      Result := CORBA.TypeCode.Internals.To_CORBA_Object
                  (PolyORB.Any.TypeCode.TCF_Struct);
      CORBA.Internals.Add_Parameter
        (Result, To_Any (CORBA.String (Name)));
      CORBA.Internals.Add_Parameter
        (Result, To_Any (CORBA.String (Id)));
      declare
         Memb_Array : constant SMS.Element_Array :=
                        SMS.To_Element_Array (SMS.Sequence (Members));
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
     (Id                 : CORBA.RepositoryId;
      Name               : CORBA.Identifier;
      Discriminator_Type : CORBA.TypeCode.Object;
      Members            : CORBA.Repository_Root.UnionMemberSeq)
     return CORBA.TypeCode.Object
   is
      Result : CORBA.TypeCode.Object;
      package UMS renames
        CORBA.Repository_Root.IDL_SEQUENCE_CORBA_UnionMember;
   begin
      Result := CORBA.TypeCode.Internals.To_CORBA_Object
                  (PolyORB.Any.TypeCode.TCF_Union);
      CORBA.Internals.Add_Parameter
        (Result, To_Any (CORBA.String (Name)));
      CORBA.Internals.Add_Parameter
        (Result, To_Any (CORBA.String (Id)));
      CORBA.Internals.Add_Parameter
        (Result, To_Any (Discriminator_Type));
      declare
         Memb_Array : constant UMS.Element_Array :=
                        UMS.To_Element_Array (UMS.Sequence (Members));
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
     (Id      : CORBA.RepositoryId;
      Name    : CORBA.Identifier;
      Members : CORBA.Repository_Root.EnumMemberSeq)
     return CORBA.TypeCode.Object
   is
      Result : CORBA.TypeCode.Object;
      package EMS renames
        CORBA.Repository_Root.IDL_SEQUENCE_CORBA_Identifier;
      Memb_Array : constant EMS.Element_Array :=
                     EMS.To_Element_Array (EMS.Sequence (Members));
   begin
      Result := CORBA.TypeCode.Internals.To_CORBA_Object
                  (PolyORB.Any.TypeCode.TCF_Enum);
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
     (Id            : CORBA.RepositoryId;
      Name          : CORBA.Identifier;
      Original_Type : CORBA.TypeCode.Object) return CORBA.TypeCode.Object is
   begin
      return CORBA.TypeCode.Internals.Build_Alias_TC
        (Name => CORBA.String (Name), Id => CORBA.String (Id),
         Parent => Original_Type);
   end Create_Alias_Tc;

   function Create_Exception_Tc
     (Id      : CORBA.RepositoryId;
      Name    : CORBA.Identifier;
      Members : CORBA.Repository_Root.StructMemberSeq)
     return CORBA.TypeCode.Object
   is
      Result : CORBA.TypeCode.Object;
      package SMS renames
        CORBA.Repository_Root.IDL_SEQUENCE_CORBA_StructMember;
   begin
      Result := CORBA.TypeCode.Internals.To_CORBA_Object
                  (PolyORB.Any.TypeCode.TCF_Except);
      CORBA.Internals.Add_Parameter
        (Result, To_Any (CORBA.String (Name)));
      CORBA.Internals.Add_Parameter
        (Result, To_Any (CORBA.String (Id)));
      declare
         Memb_Array : constant SMS.Element_Array :=
                        SMS.To_Element_Array (SMS.Sequence (Members));
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
     (Id   : CORBA.RepositoryId;
      Name : CORBA.Identifier)
     return CORBA.TypeCode.Object
   is
      Result : CORBA.TypeCode.Object;
   begin
      Result := CORBA.TypeCode.Internals.To_CORBA_Object
                  (PolyORB.Any.TypeCode.TCF_Object);
      CORBA.Internals.Add_Parameter
        (Result, To_Any (CORBA.String (Name)));
      CORBA.Internals.Add_Parameter
        (Result, To_Any (CORBA.String (Id)));
      return Result;
   end Create_Interface_Tc;

   function Create_String_Tc
     (Bound : CORBA.Unsigned_Long) return CORBA.TypeCode.Object
   is
   begin
      return CORBA.TypeCode.Internals.To_CORBA_Object
               (PolyORB.Any.TypeCode.Build_String_TC
                  (PolyORB.Types.Unsigned_Long (Bound)));
   end Create_String_Tc;

   function Create_Wstring_Tc
     (Bound : CORBA.Unsigned_Long) return CORBA.TypeCode.Object
   is
   begin
      return CORBA.TypeCode.Internals.To_CORBA_Object
               (PolyORB.Any.TypeCode.Build_Wstring_TC
                  (PolyORB.Types.Unsigned_Long (Bound)));
   end Create_Wstring_Tc;

   function Create_Fixed_Tc
     (IDL_Digits : CORBA.Unsigned_Short;
      Scale  : CORBA.Short)
     return CORBA.TypeCode.Object
   is
      Result : CORBA.TypeCode.Object;
   begin
      Result := CORBA.TypeCode.Internals.To_CORBA_Object
                  (PolyORB.Any.TypeCode.TCF_Fixed);
      CORBA.Internals.Add_Parameter (Result, To_Any (IDL_Digits));
      CORBA.Internals.Add_Parameter (Result, To_Any (Scale));
      return Result;
   end Create_Fixed_Tc;

   function Create_Sequence_Tc
     (Bound       : CORBA.Unsigned_Long;
      Elementtype : CORBA.TypeCode.Object)
     return CORBA.TypeCode.Object
   is
      Result : CORBA.TypeCode.Object;
   begin
      Result := CORBA.TypeCode.Internals.To_CORBA_Object
                  (PolyORB.Any.TypeCode.TCF_Sequence);
      CORBA.Internals.Add_Parameter (Result, To_Any (Bound));
      CORBA.Internals.Add_Parameter (Result, To_Any (Elementtype));
      return Result;
   end Create_Sequence_Tc;

   function Create_Recursive_Sequence_Tc
     (Bound  : CORBA.Unsigned_Long;
      Offset : CORBA.Unsigned_Long)
     return CORBA.TypeCode.Object
   is
      pragma Warnings (Off);
      pragma Unreferenced (Bound, Offset);
      pragma Warnings (On);
   begin
      return TC_Null;
   end Create_Recursive_Sequence_Tc;

   function Create_Array_Tc
     (Length       : CORBA.Unsigned_Long;
      Element_Type : CORBA.TypeCode.Object)
     return CORBA.TypeCode.Object
   is
      Result : CORBA.TypeCode.Object;
   begin
      Result := CORBA.TypeCode.Internals.To_CORBA_Object
                  (PolyORB.Any.TypeCode.TCF_Array);
      CORBA.Internals.Add_Parameter (Result, To_Any (Length));
      CORBA.Internals.Add_Parameter (Result, To_Any (Element_Type));
      return Result;
   end Create_Array_Tc;

   function Create_Value_Tc
     (Id            : CORBA.RepositoryId;
      Name          : CORBA.Identifier;
      Type_Modifier : CORBA.ValueModifier;
      Concrete_Base : CORBA.TypeCode.Object;
      Members       : CORBA.Repository_Root.ValueMemberSeq)
     return CORBA.TypeCode.Object
   is
      Result : CORBA.TypeCode.Object;

      package VMS renames CORBA.Repository_Root.IDL_SEQUENCE_CORBA_ValueMember;

   begin
      Result := CORBA.TypeCode.Internals.To_CORBA_Object
                  (PolyORB.Any.TypeCode.TCF_Value);
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
     (Id         : CORBA.RepositoryId;
      Name       : CORBA.Identifier;
      Boxed_Type : CORBA.TypeCode.Object)
     return CORBA.TypeCode.Object
   is
      Result : CORBA.TypeCode.Object;
   begin
      Result := CORBA.TypeCode.Internals.To_CORBA_Object
                  (PolyORB.Any.TypeCode.TCF_Valuebox);
      CORBA.Internals.Add_Parameter
        (Result, To_Any (CORBA.String (Name)));
      CORBA.Internals.Add_Parameter
        (Result, To_Any (CORBA.String (Id)));
      CORBA.Internals.Add_Parameter
        (Result, To_Any (Boxed_Type));
      return Result;
   end Create_Value_Box_Tc;

   function Create_Native_Tc
     (Id   : CORBA.RepositoryId;
      Name : CORBA.Identifier)
     return CORBA.TypeCode.Object
   is
      Result : CORBA.TypeCode.Object;
   begin
      Result := CORBA.TypeCode.Internals.To_CORBA_Object
                  (PolyORB.Any.TypeCode.TCF_Native);
      CORBA.Internals.Add_Parameter
        (Result, To_Any (CORBA.String (Name)));
      CORBA.Internals.Add_Parameter
        (Result, To_Any (CORBA.String (Id)));
      return Result;
   end Create_Native_Tc;

   function Create_Recursive_Tc
     (Id   : CORBA.RepositoryId)
     return CORBA.TypeCode.Object
   is
      pragma Warnings (Off);
      pragma Unreferenced (Id);
      pragma Warnings (On);
   begin
      return TC_Null;
   end Create_Recursive_Tc;

   function Create_Abstract_Interface_Tc
     (Id   : CORBA.RepositoryId;
      Name : CORBA.Identifier) return CORBA.TypeCode.Object
   is
   begin
      return CORBA.TypeCode.Internals.To_CORBA_Object
        (PolyORB.Any.TypeCode.Build_Complex_TC (Tk_Abstract_Interface,
           (0 => PolyORB.Any.To_Any (PolyORB.Types.String (Name)),
            1 => PolyORB.Any.To_Any (PolyORB.Types.String (Id)))));
   end Create_Abstract_Interface_Tc;

end CORBA.ORB.Typecode;
