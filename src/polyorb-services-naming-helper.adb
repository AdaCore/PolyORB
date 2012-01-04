------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--       P O L Y O R B . S E R V I C E S . N A M I N G . H E L P E R        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2002-2012, Free Software Foundation, Inc.          --
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
   procedure O (Message : Standard.String; Level : Log_Level := Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
     renames L.Enabled;

   --------------
   -- From_Any --
   --------------

   function From_Any (Item : PolyORB.Any.Any) return Istring
   is
      Result : constant PolyORB.Types.String := From_Any (Item);
   begin
      pragma Debug (C, O ("From Any : (Istring)"));
      return Istring (Result);
   end From_Any;

   function From_Any (Item : PolyORB.Any.Any)
      return NameComponent is
      Index : PolyORB.Any.Any;
      Result_id : Istring;
      Result_kind : Istring;
   begin
      pragma Debug (C, O ("From Any : (NameComponent)"));
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

   function From_Any (Item : PolyORB.Any.Any) return Name is
      Result : constant SEQUENCE_NameComponent.Sequence :=
                 Helper.From_Any (Item);
   begin
      pragma Debug (C, O ("From Any : (Name)"));
      return Name (Result);
   end From_Any;

   function From_Any (Item : PolyORB.Any.Any)
      return BindingType
   is
      pragma Debug (C, O ("From Any : (BindingType)"));
      Index : constant PolyORB.Any.Any :=
                Get_Aggregate_Element (Item,
                                       TypeCode.TC_Unsigned_Long,
                                       PolyORB.Types.Unsigned_Long (0));
      Position : constant PolyORB.Types.Unsigned_Long := From_Any (Index);
   begin
      return BindingType'Val (Position);
   end From_Any;

   function From_Any (Item : PolyORB.Any.Any)
      return Binding
   is
      Index : PolyORB.Any.Any;
      Result_binding_name : Name;
      Result_binding_type : BindingType;
   begin
      pragma Debug (C, O ("From Any : (Binding)"));
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

   function From_Any (Item : PolyORB.Any.Any)
      return SEQUENCE_Binding.Sequence
   is
      use SEQUENCE_Binding;
      Count  : constant PolyORB.Types.Unsigned_Long :=
                 From_Any
                   (Get_Aggregate_Element (Item,
                                           TypeCode.TC_Unsigned_Long,
                                           PolyORB.Types.Unsigned_Long (0)));
      Result : Element_Array (1 .. Integer (Count));
   begin
      pragma Debug (C, O ("From Any : (SequenceBinding)"));
      for J in Result'Range loop
         Result (J) := Helper.From_Any
                         (Get_Aggregate_Element
                          (Item, Helper.TC_Binding,
                           PolyORB.Types.Unsigned_Long (J)));
      end loop;
      return To_Sequence (Result);
   end From_Any;

   function From_Any (Item : PolyORB.Any.Any)
      return SEQUENCE_NameComponent.Sequence
   is
      use SEQUENCE_NameComponent;
      Count  : constant PolyORB.Types.Unsigned_Long :=
                 From_Any
                   (Get_Aggregate_Element (Item,
                                           TypeCode.TC_Unsigned_Long,
                                           PolyORB.Types.Unsigned_Long (0)));
      Result : Element_Array (1 .. Integer (Count));
   begin
      pragma Debug (C, O ("From Any : (Sequence namecomponent)"));
      for J in Result'Range loop
         Result (J) := Helper.From_Any
                         (Get_Aggregate_Element
                          (Item, Helper.TC_Binding,
                           PolyORB.Types.Unsigned_Long (J)));
      end loop;
      return To_Sequence (Result);
   end From_Any;

   function From_Any (Item : PolyORB.Any.Any) return BindingList is
      Result : constant SEQUENCE_Binding.Sequence := Helper.From_Any (Item);
   begin
      return BindingList (Result);
   end From_Any;

   ------------
   -- To_Any --
   ------------

   function To_Any (Item : Istring)
     return PolyORB.Any.Any
   is
      Result : PolyORB.Any.Any := To_Any (PolyORB.Types.String (Item));
   begin
      pragma Debug (C, O ("To Any : (Istring)"));
      PolyORB.Any.Set_Type (Result, TC_Istring);
      return Result;
   end To_Any;

   function To_Any
     (Item : NameComponent)
     return PolyORB.Any.Any is
      Result : PolyORB.Any.Any := Get_Empty_Any_Aggregate (TC_NameComponent);
   begin
      pragma Debug (C, O ("To Any : (NameComponent)"));
      Add_Aggregate_Element (Result, To_Any (Item.id));
      Add_Aggregate_Element (Result, To_Any (Item.kind));

      return Result;
   end To_Any;

   function To_Any
     (Item : SEQUENCE_NameComponent.Sequence)
     return PolyORB.Any.Any
   is
      use SEQUENCE_NameComponent;
      Array_Item : constant Element_Array := To_Element_Array (Item);
      Result : PolyORB.Any.Any :=
                 Get_Empty_Any_Aggregate (TC_SEQUENCE_NameComponent);
   begin
      pragma Debug (C, O ("To Any : (Sequence NameComponent)"));
      Add_Aggregate_Element
        (Result, To_Any (PolyORB.Types.Unsigned_Long (Length (Item))));

      for I in Array_Item'Range loop
         Add_Aggregate_Element
           (Result,
            Helper.To_Any (Array_Item (I)));
      end loop;
      return Result;
   end To_Any;

   function To_Any (Item : Name)
     return PolyORB.Any.Any
   is
      Result : PolyORB.Any.Any :=
        To_Any (SEQUENCE_NameComponent.Sequence (Item));
   begin
      pragma Debug (C, O ("To Any : (Name)"));
      Set_Type (Result, TC_Name);
      return Result;
   end To_Any;

   function To_Any
     (Item : BindingType)
      return PolyORB.Any.Any
   is
      Result : PolyORB.Any.Any :=
         Get_Empty_Any_Aggregate (TC_BindingType);
   begin
      pragma Debug (C, O ("To Any : (BindingType)"));
      Add_Aggregate_Element
         (Result,
          To_Any (PolyORB.Types.Unsigned_Long (BindingType'Pos (Item))));
      return Result;
   end To_Any;

   function To_Any
     (Item : Binding)
     return PolyORB.Any.Any
   is
      Result : PolyORB.Any.Any :=
         Get_Empty_Any_Aggregate (TC_Binding);
   begin
      pragma Debug (C, O ("To Any : (Binding)"));
      Add_Aggregate_Element
         (Result, Helper.To_Any (Item.binding_name));
      Add_Aggregate_Element
         (Result, Helper.To_Any (Item.binding_type));
      return Result;
   end To_Any;

   function To_Any
     (Item : SEQUENCE_Binding.Sequence)
     return PolyORB.Any.Any
   is
      use SEQUENCE_Binding;
      Array_Item : constant Element_Array := To_Element_Array (Item);
      Result : PolyORB.Any.Any :=
                 Get_Empty_Any_Aggregate (TC_SEQUENCE_Binding);
   begin
      pragma Debug (C, O ("To Any : (SequenceBinding)"));
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
     (Item : BindingList)
     return PolyORB.Any.Any is
      Result : PolyORB.Any.Any := To_Any (SEQUENCE_Binding.Sequence (Item));
   begin
      pragma Debug (C, O ("To Any : (BindingList)"));
      Set_Type (Result, TC_BindingList);
      return Result;
   end To_Any;

   function To_Any (Item : PolyORB.References.Ref)
                    return PolyORB.Any.Any
   is
      A : Any.Any := Any.ObjRef.To_Any (Item);
   begin
      Set_Type (A, Services.Naming.Helper.TC_Object);
      return A;
   end To_Any;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize;

   procedure Initialize is
      use PolyORB.Any.TypeCode;
   begin
      declare
         Name : constant PolyORB.Types.String := To_PolyORB_String ("Istring");
         Id   : constant PolyORB.Types.String :=
                  To_PolyORB_String ("IDL:omg.org/CosNaming/Istring:1.0");
      begin
         TC_Istring := TC_Alias;
         Add_Parameter (TC_Istring, To_Any (Name));
         Add_Parameter (TC_Istring, To_Any (Id));
         Add_Parameter (TC_Istring, To_Any (TypeCode.TC_String));
         Disable_Reference_Counting (Object_Of (TC_Istring).all);
      end;

      declare
         Name : constant PolyORB.Types.String :=
                  To_PolyORB_String ("NameComponent");
         Id : constant PolyORB.Types.String :=
                To_PolyORB_String ("IDL:omg.org/CosNaming/NameComponent:1.0");

         Arg_Name_id   : constant PolyORB.Types.String :=
                           To_PolyORB_String ("id");
         Arg_Name_kind : constant PolyORB.Types.String :=
                           To_PolyORB_String ("kind");
      begin
         TC_NameComponent := TC_Struct;
         Add_Parameter (TC_NameComponent, To_Any (Name));
         Add_Parameter (TC_NameComponent, To_Any (Id));
         Add_Parameter (TC_NameComponent, To_Any (TC_Istring));
         Add_Parameter (TC_NameComponent, To_Any (Arg_Name_id));
         Add_Parameter (TC_NameComponent, To_Any (TC_Istring));
         Add_Parameter (TC_NameComponent, To_Any (Arg_Name_kind));
         Disable_Reference_Counting (Object_Of (TC_NameComponent).all);
      end;

      TC_SEQUENCE_NameComponent := TC_Sequence;
      Add_Parameter (TC_SEQUENCE_NameComponent,
                     To_Any (Types.Unsigned_Long'(0)));
      Add_Parameter (TC_SEQUENCE_NameComponent, To_Any (TC_NameComponent));
      Disable_Reference_Counting (Object_Of (TC_SEQUENCE_NameComponent).all);

      declare
         Name : constant PolyORB.Types.String := To_PolyORB_String ("Name");
         Id   : constant PolyORB.Types.String :=
                  To_PolyORB_String ("IDL:omg.org/CosNaming/Name:1.0");
      begin
         TC_Name := TC_Alias;
         Add_Parameter (TC_Name, To_Any (Name));
         Add_Parameter (TC_Name, To_Any (Id));
         Add_Parameter (TC_Name, To_Any (TC_SEQUENCE_NameComponent));
         Disable_Reference_Counting (Object_Of (TC_Name).all);
      end;

      declare
         Name : constant PolyORB.Types.String :=
                  To_PolyORB_String ("BindingType");
         Id   : constant PolyORB.Types.String :=
                  To_PolyORB_String ("IDL:omg.org/CosNaming/BindingType:1.0");

         nobject_Name : constant PolyORB.Types.String :=
                          To_PolyORB_String ("nobject");
         ncontext_Name : constant PolyORB.Types.String :=
                           To_PolyORB_String ("ncontext");
      begin
         TC_BindingType := TC_Enum;
         Add_Parameter (TC_BindingType, To_Any (Name));
         Add_Parameter (TC_BindingType, To_Any (Id));
         Add_Parameter (TC_BindingType, To_Any (nobject_Name));
         Add_Parameter (TC_BindingType, To_Any (ncontext_Name));
         Disable_Reference_Counting (Object_Of (TC_BindingType).all);
      end;

      declare
         Name : constant PolyORB.Types.String :=
                  To_PolyORB_String ("Binding");
         Id   : constant PolyORB.Types.String :=
                  To_PolyORB_String ("IDL:omg.org/CosNaming/Binding:1.0");

         Arg_Name_binding_name : constant PolyORB.Types.String :=
                                   To_PolyORB_String ("binding_name");
         Arg_Name_binding_type : constant PolyORB.Types.String :=
                                   To_PolyORB_String ("binding_type");
      begin
         TC_Binding := TC_Struct;
         Add_Parameter (TC_Binding, To_Any (Name));
         Add_Parameter (TC_Binding, To_Any (Id));
         Add_Parameter (TC_Binding, To_Any (Helper.TC_Name));
         Add_Parameter (TC_Binding, To_Any (Arg_Name_binding_name));
         Add_Parameter (TC_Binding, To_Any (Helper.TC_BindingType));
         Add_Parameter (TC_Binding, To_Any (Arg_Name_binding_type));
         Disable_Reference_Counting (Object_Of (TC_Binding).all);
      end;

      TC_SEQUENCE_Binding := TC_Sequence;
      Add_Parameter (TC_SEQUENCE_Binding,
                              To_Any (PolyORB.Types.Unsigned_Long (0)));
      Add_Parameter (TC_SEQUENCE_Binding,
                              To_Any (TC_Binding));
      Disable_Reference_Counting (Object_Of (TC_SEQUENCE_Binding).all);

      declare
         Name : constant PolyORB.Types.String :=
                  To_PolyORB_String ("BindingList");
         Id   : constant PolyORB.Types.String :=
                  To_PolyORB_String ("IDL:omg.org/CosNaming/BindingList:1.0");
      begin
         TC_BindingList := TC_Alias;
         Add_Parameter (TC_BindingList, To_Any (Name));
         Add_Parameter (TC_BindingList, To_Any (Id));
         Add_Parameter (TC_BindingList, To_Any (TC_SEQUENCE_Binding));
         Disable_Reference_Counting (Object_Of (TC_BindingList).all);
      end;

      --  XXX to be declared in minimal servant ???
      declare
         Name : constant PolyORB.Types.String :=
                  To_PolyORB_String ("Object");
         Id   : constant PolyORB.Types.String :=
                  To_PolyORB_String ("IDL:CORBA/Object:1.0");
      begin
         Naming.Helper.TC_Object := TypeCode.TC_Object;
         Add_Parameter (Naming.Helper.TC_Object, To_Any (Name));
         Add_Parameter (Naming.Helper.TC_Object, To_Any (Id));
         Disable_Reference_Counting (Object_Of (Naming.Helper.TC_Object).all);
      end;

   end Initialize;

   use PolyORB.Initialization;
   use PolyORB.Initialization.String_Lists;
   use PolyORB.Utils.Strings;

begin
   Register_Module
     (Module_Info'
      (Name      => +"naming.Helper",
       Conflicts => Empty,
       Depends   => +"any",
       Provides  => Empty,
       Implicit  => False,
       Init      => Initialize'Access,
       Shutdown  => null));
end PolyORB.Services.Naming.Helper;
