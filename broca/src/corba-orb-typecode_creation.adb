------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--          C O R B A . O R B . T Y P E C O D E _ C R E A T I O N           --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1999-2000 ENST Paris University, France.          --
--                                                                          --
-- AdaBroker is free software; you  can  redistribute  it and/or modify it  --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. AdaBroker  is distributed  in the hope that it will be  useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with AdaBroker; see file COPYING. If  --
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
--             AdaBroker is maintained by ENST Paris University.            --
--                     (email: broker@inf.enst.fr)                          --
--                                                                          --
------------------------------------------------------------------------------

package body CORBA.ORB.Typecode_Creation is

   function Create_Struct_Tc
     (Self    : in Ref;
      Id      : in CORBA.RepositoryId;
      Name    : in CORBA.Identifier;
      Members : in CORBA.StructMemberSeq)
      return CORBA.TypeCode.Object is
      Result : TypeCode.Object;
   begin
      Result := TypeCode.TC_Struct;
      TypeCode.Add_Parameter (Result, To_Any (CORBA.String (Name)));
      TypeCode.Add_Parameter (Result, To_Any (CORBA.String (Id)));
      declare
         Member : StructMember;
      begin
         for I in 0 .. Length (Members) - 1 loop
            Member := Element_Of (Members, I);
            TypeCode.Add_Parameter (Result, To_Any (Member.IDL_Type));
            TypeCode.Add_Parameter
              (Result, To_Any (CORBA.String (Member.Name)));
         end loop;
      end;
      return Result;
   end Create_Struct_Tc;

   function Create_Union_Tc
     (Self               : in Ref;
      Id                 : in CORBA.RepositoryId;
      Name               : in CORBA.Identifier;
      Discriminator_Type : in CORBA.TypeCode.Object;
      Members            : in CORBA.UnionMemberSeq)
      return CORBA.TypeCode.Object is
      Result : TypeCode.Object;
   begin
      Result := TypeCode.TC_Union;
      TypeCode.Add_Parameter (Result, To_Any (CORBA.String (Name)));
      TypeCode.Add_Parameter (Result, To_Any (CORBA.String (Id)));
      TypeCode.Add_Parameter (Result, To_Any (Discriminator_Type));
      declare
         Member : UnionMember;
      begin
         for I in 0 .. Length (Members) - 1 loop
            Member := Element_Of (Members, I);
            TypeCode.Add_Parameter (Result, To_Any (Member.IDL_Type));
--  FIXME            TypeCode.Add_Parameter (Result, To_Any (Member.Label));
            TypeCode.Add_Parameter
              (Result, To_Any (CORBA.String (Member.Name)));
         end loop;
      end;
      return Result;
   end Create_Union_Tc;

   function Create_Enum_Tc
     (Self    : in Ref;
      Id      : in CORBA.RepositoryId;
      Name    : in CORBA.Identifier;
      Members : in CORBA.EnumMemberSeq)
      return CORBA.TypeCode.Object is
      Result : TypeCode.Object;
   begin
      Result := TypeCode.TC_Enum;
      TypeCode.Add_Parameter (Result, To_Any (CORBA.String (Name)));
      TypeCode.Add_Parameter (Result, To_Any (CORBA.String (Id)));
      for I in 0 .. Length (Members) - 1 loop
         TypeCode.Add_Parameter
           (Result, To_Any (CORBA.String (Element_Of (Members, I))));
      end loop;
      return Result;
   end Create_Enum_Tc;

   function Create_Alias_Tc
     (Self          : in Ref;
      Id            : in CORBA.RepositoryId;
      Name          : in CORBA.Identifier;
      Original_Type : in CORBA.TypeCode.Object)
      return CORBA.TypeCode.Object is
      Result : TypeCode.Object;
   begin
      Result := TypeCode.TC_Alias;
      TypeCode.Add_Parameter (Result, To_Any (CORBA.String (Name)));
      TypeCode.Add_Parameter (Result, To_Any (CORBA.String (Id)));
      TypeCode.Add_Parameter (Result, To_Any (Original_Type));
      return Result;
   end Create_Alias_Tc;

   function Create_Exception_Tc
     (Self    : in Ref;
      Id      : in CORBA.RepositoryId;
      Name    : in CORBA.Identifier;
      Members : in CORBA.StructMemberSeq)
      return CORBA.TypeCode.Object is
      Result : TypeCode.Object;
   begin
      Result := TypeCode.TC_Except;
      TypeCode.Add_Parameter (Result, To_Any (CORBA.String (Name)));
      TypeCode.Add_Parameter (Result, To_Any (CORBA.String (Id)));
      declare
         Member : StructMember;
      begin
         for I in 0 .. Length (Members) - 1 loop
            Member := Element_Of (Members, I);
            TypeCode.Add_Parameter (Result, To_Any (Member.IDL_Type));
            TypeCode.Add_Parameter
              (Result, To_Any (CORBA.String (Member.Name)));
         end loop;
      end;
      return Result;
   end Create_Exception_Tc;

   function Create_Interface_Tc
     (Self : in Ref;
      Id   : in CORBA.RepositoryId;
      Name : in CORBA.Identifier)
      return CORBA.TypeCode.Object is
      Result : TypeCode.Object;
   begin
      Result := TypeCode.TC_Objref;
      TypeCode.Add_Parameter (Result, To_Any (CORBA.String (Name)));
      TypeCode.Add_Parameter (Result, To_Any (CORBA.String (Id)));
      return Result;
   end Create_Interface_Tc;

   function Create_String_Tc
     (Self  : in Ref;
      Bound : in CORBA.Unsigned_Long)
      return CORBA.TypeCode.Object is
      Result : TypeCode.Object;
   begin
      Result := TypeCode.TC_String;
      TypeCode.Add_Parameter (Result, To_Any (Bound));
      return Result;
   end Create_String_Tc;

   function Create_Wstring_Tc
     (Self  : in Ref;
      Bound : in CORBA.Unsigned_Long)
      return CORBA.TypeCode.Object is
      Result : TypeCode.Object;
   begin
      Result := TypeCode.TC_Wide_String;
      TypeCode.Add_Parameter (Result, To_Any (Bound));
      return Result;
   end Create_Wstring_Tc;

   function Create_Fixed_Tc
     (Self   : in Ref;
      IDL_Digits : in CORBA.Unsigned_Short;
      Scale  : in CORBA.Short)
      return CORBA.TypeCode.Object is
      Result : TypeCode.Object;
   begin
      Result := TypeCode.TC_Fixed;
      TypeCode.Add_Parameter (Result, To_Any (IDL_Digits));
      TypeCode.Add_Parameter (Result, To_Any (Scale));
      return Result;
   end Create_Fixed_Tc;

   function Create_Sequence_Tc
     (Self        : in Ref;
      Bound       : in CORBA.Unsigned_Long;
      Elementtype : in CORBA.TypeCode.Object)
      return CORBA.TypeCode.Object is
      Result : TypeCode.Object;
   begin
      Result := TypeCode.TC_Sequence;
      TypeCode.Add_Parameter (Result, To_Any (Bound));
      TypeCode.Add_Parameter (Result, To_Any (Elementtype));
      return Result;
   end Create_Sequence_Tc;

   function Create_Recursive_Sequence_Tc
     (Self   : in Ref;
      Bound  : in CORBA.Unsigned_Long;
      Offset : in CORBA.Unsigned_Long)
      return CORBA.TypeCode.Object is
   begin
      return TC_Null;
   end Create_Recursive_Sequence_Tc;

   function Create_Array_Tc
     (Self         : in Ref;
      Length       : in CORBA.Unsigned_Long;
      Element_Type : in CORBA.TypeCode.Object)
      return CORBA.TypeCode.Object is
      Result : TypeCode.Object;
   begin
      Result := TypeCode.TC_Array;
      TypeCode.Add_Parameter (Result, To_Any (Length));
      TypeCode.Add_Parameter (Result, To_Any (Element_Type));
      return Result;
   end Create_Array_Tc;

   function Create_Value_Tc
     (Self          : in Ref;
      Id            : in CORBA.RepositoryId;
      Name          : in CORBA.Identifier;
      Type_Modifier : in CORBA.ValueModifier;
      Concrete_Base : in CORBA.TypeCode.Object;
      Members       : in CORBA.ValueMemberSeq)
      return CORBA.TypeCode.Object is
      Result : TypeCode.Object;
   begin
      Result := TypeCode.TC_Value;
      TypeCode.Add_Parameter (Result, To_Any (CORBA.String (Name)));
      TypeCode.Add_Parameter (Result, To_Any (CORBA.String (Id)));
      TypeCode.Add_Parameter (Result, To_Any (CORBA.Short (Type_Modifier)));
      TypeCode.Add_Parameter (Result, To_Any (Concrete_Base));
      declare
         Member : ValueMember;
      begin
         for I in 0 .. Length (Members) - 1 loop
            Member := Element_Of (Members, I);
            TypeCode.Add_Parameter
              (Result, To_Any (CORBA.Short (Member.IDL_Access)));
            TypeCode.Add_Parameter (Result, To_Any (Member.IDL_Type));
            TypeCode.Add_Parameter
              (Result, To_Any (CORBA.String (Member.Name)));
         end loop;
      end;
      return Result;
   end Create_Value_Tc;

   function Create_Value_Box_Tc
     (Self       : in Ref;
      Id         : in CORBA.RepositoryId;
      Name       : in CORBA.Identifier;
      Boxed_Type : in CORBA.TypeCode.Object)
      return CORBA.TypeCode.Object is
      Result : TypeCode.Object;
   begin
      Result := TypeCode.TC_Valuebox;
      TypeCode.Add_Parameter (Result, To_Any (CORBA.String (Name)));
      TypeCode.Add_Parameter (Result, To_Any (CORBA.String (Id)));
      TypeCode.Add_Parameter (Result, To_Any (Boxed_Type));
      return Result;
   end Create_Value_Box_Tc;

   function Create_Native_Tc
     (Self : in Ref;
      Id   : in CORBA.RepositoryId;
      Name : in CORBA.Identifier)
      return CORBA.TypeCode.Object is
      Result : TypeCode.Object;
   begin
      Result := TypeCode.TC_Native;
      TypeCode.Add_Parameter (Result, To_Any (CORBA.String (Name)));
      TypeCode.Add_Parameter (Result, To_Any (CORBA.String (Id)));
      return Result;
   end Create_Native_Tc;

   function Create_Recursive_Tc
     (Self : in Ref;
      Id   : in CORBA.RepositoryId)
      return CORBA.TypeCode.Object is
   begin
      return TC_Null;
   end Create_Recursive_Tc;

   function Create_Abstract_Interface_Tc
     (Self : in Ref;
      Id   : in CORBA.RepositoryId;
      Name : in CORBA.Identifier)
      return CORBA.TypeCode.Object is
      Result : TypeCode.Object;
   begin
      Result := TypeCode.TC_Abstract_Interface;
      TypeCode.Add_Parameter (Result, To_Any (CORBA.String (Name)));
      TypeCode.Add_Parameter (Result, To_Any (CORBA.String (Id)));
      return Result;
   end Create_Abstract_Interface_Tc;

end CORBA.ORB.Typecode_Creation;
