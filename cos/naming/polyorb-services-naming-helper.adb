------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--        P O L Y O R B . S E R V I C E S . N A M I N G . H E L P E R       --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--             Copyright (C) 1999-2002 Free Software Fundation              --
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
--              PolyORB is maintained by ENST Paris University.             --
--                                                                          --
------------------------------------------------------------------------------

--  $Id$


with PolyORB.Initialization;
pragma Elaborate_All (PolyORB.Initialization);

with PolyORB.Any.ObjRef;
--  with PolyORB.Exceptions;
with PolyORB.Log;
with PolyORB.Types;
with PolyORB.Utils.Strings;

package body  PolyORB.Services.Naming.Helper is

   use PolyORB.Any;
   use PolyORB.Log;
   use PolyORB.Types;

   package L is new PolyORB.Log.Facility_Log
     ("polyorb.services.naming.helper");
   procedure O (Message : in Standard.String; Level : Log_Level := Debug)
     renames L.Output;

   --------------
   -- From_Any --
   --------------

   function From_Any (Item : in PolyORB.Any.Any)
      return Istring
   is
      Result : PolyORB.Types.String := From_Any (Item);
   begin
      pragma Debug (O ("From Any : (Istring)"));
      return Istring (Result);
   end From_Any;

   function From_Any (Item : in PolyORB.Any.Any)
      return NameComponent is
      Index : PolyORB.Any.Any;
      Result_id : Istring;
      Result_kind : Istring;
   begin
      pragma Debug (O ("From Any : (NameComponent)"));
      Index := Get_Aggregate_Element (Item,
                                      TC_Istring,
                                      PolyORB.Types.Unsigned_Long (0));
      Result_id := Helper.From_Any (Index);

      Index := Get_Aggregate_Element (Item,
                                      TC_Istring,
                                      PolyORB.Types.Unsigned_Long (1));
      Result_kind := Helper.From_Any (Index);
      return
         (id => Result_id,
          kind => Result_kind);
   end From_Any;

   function From_Any (Item : in PolyORB.Any.Any)
      return Name
   is
      Result : SEQUENCE_NameComponent.Sequence := Helper.From_Any (Item);
   begin
      pragma Debug (O ("From Any : (Name)"));
      return Name (Result);
   end From_Any;

   function From_Any (Item : in PolyORB.Any.Any)
      return BindingType
   is
      pragma Debug (O ("From Any : (BindingType)"));
      Index : PolyORB.Any.Any :=
         Get_Aggregate_Element (Item,
                                TypeCode.TC_Unsigned_Long,
                                PolyORB.Types.Unsigned_Long (0));
      Position : constant PolyORB.Types.Unsigned_Long
        := From_Any (Index);
   begin
      return BindingType'Val (Position);
   end From_Any;

   function From_Any (Item : in PolyORB.Any.Any)
      return Binding
   is
      Index : PolyORB.Any.Any;
      Result_binding_name : Name;
      Result_binding_type : BindingType;
   begin
      pragma Debug (O ("From Any : (Binding)"));
      Index := Get_Aggregate_Element (Item,
                                      Helper.TC_Name,
                                      PolyORB.Types.Unsigned_Long (0));
      Result_binding_name := Helper.From_Any (Index);
      Index := Get_Aggregate_Element (Item,
                                      Helper.TC_BindingType,
                                      PolyORB.Types.Unsigned_Long (1));
      Result_binding_type := Helper.From_Any (Index);
      return
         (binding_name => Result_binding_name,
          binding_type => Result_binding_type);
   end From_Any;

   function From_Any (Item : in PolyORB.Any.Any)
      return SEQUENCE_Binding.Sequence
   is
      use SEQUENCE_Binding;
      Nb_Any : PolyORB.Any.Any :=
         Get_Aggregate_Element (Item,
                                TypeCode.TC_Unsigned_Long,
                                PolyORB.Types.Unsigned_Long (0));
      Nb_Long : constant PolyORB.Types.Unsigned_Long
        := From_Any (Nb_Any);
      Nb : constant Integer := Integer (Nb_Long);
      Index : PolyORB.Any.Any;
      Result : Element_Array (1 .. Nb);
   begin
      pragma Debug (O ("From Any : (SequenceBinding)"));
      for I in 1 .. Nb loop
         Index :=
            Get_Aggregate_Element (Item,
                                   Helper.TC_Binding,
                                   PolyORB.Types.Unsigned_Long (I));
         Result (I) := Helper.From_Any (Index);
      end loop;
      return To_Sequence (Result);
   end From_Any;

   function From_Any (Item : in PolyORB.Any.Any)
      return SEQUENCE_NameComponent.Sequence
   is
      use SEQUENCE_NameComponent;
      Nb_Any : PolyORB.Any.Any :=
         Get_Aggregate_Element (Item,
                                TypeCode.TC_Unsigned_Long,
                                PolyORB.Types.Unsigned_Long (0));
      Nb_Long : constant PolyORB.Types.Unsigned_Long
        := From_Any (Nb_Any);
      Nb : constant Integer := Integer (Nb_Long);
      Index : PolyORB.Any.Any;
      Result : Element_Array (1 .. Nb);
   begin
      pragma Debug (O ("From Any : (Sequence namecomponent)"));
      for I in 1 .. Nb loop
         Index :=
            Get_Aggregate_Element (Item,
                                   TC_NameComponent,
                                   PolyORB.Types.Unsigned_Long (I));
         Result (I) := Helper.From_Any (Index);
      end loop;
      return To_Sequence (Result);
   end From_Any;

   function From_Any (Item : in PolyORB.Any.Any)
      return BindingList is
      Result : SEQUENCE_Binding.Sequence := Helper.From_Any (Item);
   begin
      return BindingList (Result);
   end From_Any;

   ------------
   -- To_Any --
   ------------

   function To_Any (Item : in Istring)
     return PolyORB.Any.Any
   is
      Result : PolyORB.Any.Any := To_Any (PolyORB.Types.String (Item));
   begin
      pragma Debug (O ("To Any : (Istring)"));
      PolyORB.Any.Set_Type (Result, TC_Istring);
      return Result;
   end To_Any;

   function To_Any
     (Item : in NameComponent)
     return PolyORB.Any.Any is
      Result : PolyORB.Any.Any := Get_Empty_Any_Aggregate (TC_NameComponent);
   begin
      pragma Debug (O ("To Any : (NameComponent)"));
      Add_Aggregate_Element (Result, To_Any (Item.id));
      Add_Aggregate_Element (Result, To_Any (Item.kind));

      return Result;
   end To_Any;

   function To_Any
     (Item : in SEQUENCE_NameComponent.Sequence)
     return PolyORB.Any.Any
   is
      use SEQUENCE_NameComponent;
      Array_Item : Element_Array := To_Element_Array (Item);
      Result : PolyORB.Any.Any := Get_Empty_Any_Aggregate
        (TC_SEQUENCE_NameComponent);
   begin
      pragma Debug (O ("To Any : (Sequence NameComponent)"));
      Add_Aggregate_Element
        (Result, To_Any (PolyORB.Types.Unsigned_Long (Length (Item))));

      for I in Array_Item'Range loop
         Add_Aggregate_Element
           (Result,
            Helper.To_Any (Array_Item (I)));
      end loop;
      return Result;
   end To_Any;

   function To_Any (Item : in Name)
     return PolyORB.Any.Any
   is
      Result : PolyORB.Any.Any :=
        To_Any (SEQUENCE_NameComponent.Sequence (Item));
   begin
      pragma Debug (O ("To Any : (Name)"));
      Set_Type (Result, TC_Name);
      return Result;
   end To_Any;

   function To_Any
     (Item : in BindingType)
      return PolyORB.Any.Any
   is
      Result : PolyORB.Any.Any :=
         Get_Empty_Any_Aggregate (TC_BindingType);
   begin
      pragma Debug (O ("To Any : (BindingType)"));
      Add_Aggregate_Element
         (Result,
          To_Any (PolyORB.Types.Unsigned_Long (BindingType'Pos (Item))));
      return Result;
   end To_Any;

   function To_Any
     (Item : in Binding)
     return PolyORB.Any.Any
   is
      Result : PolyORB.Any.Any :=
         Get_Empty_Any_Aggregate (TC_Binding);
   begin
      pragma Debug (O ("To Any : (Binding)"));
      Add_Aggregate_Element
         (Result, Helper.To_Any (Item.binding_name));
      Add_Aggregate_Element
         (Result, Helper.To_Any (Item.binding_type));
      return Result;
   end To_Any;

   function To_Any
     (Item : in SEQUENCE_Binding.Sequence)
     return PolyORB.Any.Any
   is
      use SEQUENCE_Binding;
      Array_Item : Element_Array := To_Element_Array (Item);
      Result : PolyORB.Any.Any := Get_Empty_Any_Aggregate
        (TC_SEQUENCE_Binding);
   begin
      pragma Debug (O ("To Any : (SequenceBinding)"));
      Add_Aggregate_Element
        (Result,
          To_Any (PolyORB.Types.Unsigned_Long (Length (Item))));
      for I in Array_Item'Range loop
         Add_Aggregate_Element
           (Result,
            Helper.To_Any (Array_Item (I)));
      end loop;
      return Result;
   end To_Any;

   function To_Any
     (Item : in BindingList)
     return PolyORB.Any.Any is
      Result : PolyORB.Any.Any := To_Any (SEQUENCE_Binding.Sequence (Item));
   begin
      pragma Debug (O ("To Any : (BindingList)"));
      Set_Type (Result, TC_BindingList);
      return Result;
   end To_Any;

   function To_Any (Item : in PolyORB.References.Ref)
                    return PolyORB.Any.Any
   is
      A : PolyORB.Any.Any := PolyORB.Any.ObjRef.To_Any (Item);
   begin
      Set_Type (A, PolyORB.Services.Naming.Helper.TC_Object);
      return A;
   end To_Any;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize;

   procedure Initialize is
   begin
      declare
         Name : PolyORB.Types.String :=
           To_PolyORB_String ("Istring");
         Id : PolyORB.Types.String :=
           To_PolyORB_String ("IDL:omg.org/CosNaming/Istring:1.0");
      begin
         TypeCode.Add_Parameter (TC_Istring, To_Any (Name));
         TypeCode.Add_Parameter (TC_Istring, To_Any (Id));
         TypeCode.Add_Parameter (TC_Istring, To_Any (TC_String));
      end;

      declare
         Name : PolyORB.Types.String :=
           To_PolyORB_String ("NameComponent");
         Id : PolyORB.Types.String :=
           To_PolyORB_String ("IDL:omg.org/CosNaming/NameComponent:1.0");
         Arg_Name_id : PolyORB.Types.String :=
           To_PolyORB_String ("id");
         Arg_Name_kind : PolyORB.Types.String :=
           To_PolyORB_String ("kind");
      begin
         TypeCode.Add_Parameter (TC_NameComponent, To_Any (Name));
         TypeCode.Add_Parameter (TC_NameComponent, To_Any (Id));
         TypeCode.Add_Parameter (TC_NameComponent, To_Any (TC_Istring));
         TypeCode.Add_Parameter (TC_NameComponent, To_Any (Arg_Name_id));
         TypeCode.Add_Parameter (TC_NameComponent, To_Any (TC_Istring));
         TypeCode.Add_Parameter (TC_NameComponent, To_Any (Arg_Name_kind));
      end;

      TypeCode.Add_Parameter (TC_SEQUENCE_NameComponent,
                              To_Any (PolyORB.Types.Unsigned_Long (0)));
      TypeCode.Add_Parameter (TC_SEQUENCE_NameComponent,
                              To_Any (TC_NameComponent));

      declare
         Name : PolyORB.Types.String := To_PolyORB_String ("Name");
         Id : PolyORB.Types.String :=
           To_PolyORB_String ("IDL:omg.org/CosNaming/Name:1.0");
      begin
         TypeCode.Add_Parameter (TC_Name, To_Any (Name));
         TypeCode.Add_Parameter (TC_Name, To_Any (Id));
         TypeCode.Add_Parameter (TC_Name, To_Any (TC_SEQUENCE_NameComponent));
      end;

      declare
         Name : PolyORB.Types.String :=
           To_PolyORB_String ("BindingType");
         Id : PolyORB.Types.String :=
           To_PolyORB_String ("IDL:omg.org/CosNaming/BindingType:1.0");
         nobject_Name : PolyORB.Types.String :=
           To_PolyORB_String ("nobject");
         ncontext_Name : PolyORB.Types.String :=
           To_PolyORB_String ("ncontext");
      begin
         TypeCode.Add_Parameter (TC_BindingType, To_Any (Name));
         TypeCode.Add_Parameter (TC_BindingType, To_Any (Id));
         TypeCode.Add_Parameter (TC_BindingType, To_Any (nobject_Name));
         TypeCode.Add_Parameter (TC_BindingType, To_Any (ncontext_Name));
      end;

      declare
         Name : PolyORB.Types.String :=
           To_PolyORB_String ("Binding");
         Id : PolyORB.Types.String :=
           To_PolyORB_String ("IDL:omg.org/CosNaming/Binding:1.0");
         Arg_Name_binding_name : PolyORB.Types.String :=
           To_PolyORB_String ("binding_name");
         Arg_Name_binding_type : PolyORB.Types.String :=
           To_PolyORB_String ("binding_type");
      begin
         TypeCode.Add_Parameter (TC_Binding, To_Any (Name));
         TypeCode.Add_Parameter (TC_Binding, To_Any (Id));
         TypeCode.Add_Parameter (TC_Binding, To_Any (Helper.TC_Name));
         TypeCode.Add_Parameter (TC_Binding, To_Any (Arg_Name_binding_name));
         TypeCode.Add_Parameter (TC_Binding, To_Any (Helper.TC_BindingType));
         TypeCode.Add_Parameter (TC_Binding, To_Any (Arg_Name_binding_type));
      end;

      TypeCode.Add_Parameter (TC_SEQUENCE_Binding,
                              To_Any (PolyORB.Types.Unsigned_Long (0)));
      TypeCode.Add_Parameter (TC_SEQUENCE_Binding,
                              To_Any (TC_Binding));

      declare
         Name : PolyORB.Types.String := To_PolyORB_String
           ("BindingList");
         Id : PolyORB.Types.String := To_PolyORB_String
           ("IDL:omg.org/CosNaming/BindingList:1.0");
      begin
         TypeCode.Add_Parameter (TC_BindingList, To_Any (Name));
         TypeCode.Add_Parameter (TC_BindingList, To_Any (Id));
         TypeCode.Add_Parameter (TC_BindingList,
                                 To_Any (TC_SEQUENCE_Binding));
      end;

      --  XXX to be declared in minimal servant ???
      declare
         Name : PolyORB.Types.String := To_PolyORB_String
           ("Object");
         Id : PolyORB.Types.String := To_PolyORB_String
           ("IDL:CORBA/Object:1.0");
      begin
         TypeCode.Add_Parameter (TC_Object, To_Any (Name));
         TypeCode.Add_Parameter (TC_Object, To_Any (Id));
      end;

   end Initialize;

begin
   declare
      use PolyORB.Initialization;
      use PolyORB.Initialization.String_Lists;
      use PolyORB.Utils.Strings;
   begin
      Register_Module
        (Module_Info'
         (Name      => +"Naming.Helper",
          Conflicts => Empty,
          Depends   =>
            +"soft_links"
          ,
          Provides  => Empty,
          Init      => Initialize'Access));
   end;

end PolyORB.Services.Naming.Helper;
