------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                    C O N V _ F R A M E . H E L P E R                     --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2006 Free Software Foundation, Inc.             --
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

with PolyORB.Initialization;
with PolyORB.Sequences.Unbounded.CORBA_Helper;
with PolyORB.Utils.Strings;

package body CONV_FRAME.Helper is

   procedure Deferred_Initialization;

   package IDL_SEQUENCE_CONV_FRAME_CodeSetId_Helper is
     new IDL_SEQUENCE_CONV_FRAME_CodeSetId.CORBA_Helper
     (Element_To_Any   => To_Any,
      Element_From_Any => From_Any);

   --------------
   -- From_Any --
   --------------

   function From_Any
    (Item : CORBA.Any) return IDL_SEQUENCE_CONV_FRAME_CodeSetId.Sequence
     renames IDL_SEQUENCE_CONV_FRAME_CodeSetId_Helper.From_Any;

   function From_Any (Item : CORBA.Any) return CodeSetComponent is
      Index                       : CORBA.Any;
      Result_Native_Code_Set      : CodeSetId;
      Result_Conversion_Code_Sets : IDL_SEQUENCE_CONV_FRAME_CodeSetId.Sequence;

   begin
      Index :=
        CORBA.Internals.Get_Aggregate_Element
        (Item, TC_CodeSetId, CORBA.Unsigned_Long (0));
      Result_Native_Code_Set := From_Any (Index);
      Index :=
        CORBA.Internals.Get_Aggregate_Element
        (Item, TC_IDL_SEQUENCE_CONV_FRAME_CodeSetId, CORBA.Unsigned_Long (1));
      Result_Conversion_Code_Sets := From_Any (Index);

      return
        (Native_Code_Set      => Result_Native_Code_Set,
         Conversion_Code_Sets => Result_Conversion_Code_Sets);
   end From_Any;

   function From_Any (Item : CORBA.Any) return CodeSetComponentInfo is
      Index               : CORBA.Any;
      Result_ForCharData  : CodeSetComponent;
      Result_ForWcharData : CodeSetComponent;

   begin
      Index :=
        CORBA.Internals.Get_Aggregate_Element
        (Item, TC_CodeSetComponent, CORBA.Unsigned_Long (0));
      Result_ForCharData := From_Any (Index);
      Index :=
        CORBA.Internals.Get_Aggregate_Element
        (Item, TC_CodeSetComponent, CORBA.Unsigned_Long (1));
      Result_ForWcharData := From_Any (Index);

      return
        (ForCharData  => Result_ForCharData,
         ForWcharData => Result_ForWcharData);
   end From_Any;

   function From_Any (Item : CORBA.Any) return CodeSetContext is
      Index             : CORBA.Any;
      Result_Char_Data  : CodeSetId;
      Result_Wchar_Data : CodeSetId;

   begin
      Index :=
        CORBA.Internals.Get_Aggregate_Element
        (Item, TC_CodeSetId, CORBA.Unsigned_Long (0));
      Result_Char_Data := From_Any (Index);
      Index :=
        CORBA.Internals.Get_Aggregate_Element
        (Item, TC_CodeSetId, CORBA.Unsigned_Long (1));
      Result_Wchar_Data := From_Any (Index);

      return
        (Char_Data  => Result_Char_Data,
         Wchar_Data => Result_Wchar_Data);
   end From_Any;

   function From_Any (Item : CORBA.Any) return CodeSetId is
      Result : constant CORBA.Unsigned_Long := CORBA.From_Any (Item);

   begin
      return CodeSetId (Result);
   end From_Any;

   ------------
   -- To_Any --
   ------------

   function To_Any
     (Item : IDL_SEQUENCE_CONV_FRAME_CodeSetId.Sequence) return CORBA.Any
      renames IDL_SEQUENCE_CONV_FRAME_CodeSetId_Helper.To_Any;

   function To_Any (Item : CodeSetComponent) return CORBA.Any is
      Result : CORBA.Any
        := CORBA.Internals.Get_Empty_Any_Aggregate (TC_CodeSetComponent);

   begin
      CORBA.Internals.Add_Aggregate_Element
        (Result, To_Any (Item.Native_Code_Set));
      CORBA.Internals.Add_Aggregate_Element
        (Result, To_Any (Item.Conversion_Code_Sets));

      return Result;
   end To_Any;

   function To_Any (Item : CodeSetComponentInfo) return CORBA.Any is
      Result : CORBA.Any
        := CORBA.Internals.Get_Empty_Any_Aggregate (TC_CodeSetComponentInfo);

   begin
      CORBA.Internals.Add_Aggregate_Element
        (Result, To_Any (Item.ForCharData));
      CORBA.Internals.Add_Aggregate_Element
        (Result, To_Any (Item.ForWcharData));

      return Result;
   end To_Any;

   function To_Any (Item : CodeSetContext) return CORBA.Any is
      Result : CORBA.Any
        := CORBA.Internals.Get_Empty_Any_Aggregate (TC_CodeSetContext);

   begin
      CORBA.Internals.Add_Aggregate_Element (Result, To_Any (Item.Char_Data));
      CORBA.Internals.Add_Aggregate_Element (Result, To_Any (Item.Wchar_Data));

      return Result;
   end To_Any;

   function To_Any
     (Item : CodeSetId)
     return CORBA.Any
   is
      Result : CORBA.Any := CORBA.To_Any (CORBA.Unsigned_Long (Item));
   begin
      CORBA.Internals.Set_Type (Result, TC_CodeSetId);

      return Result;
   end To_Any;

   -----------------------------
   -- Deferred_Initialization --
   -----------------------------

   procedure Deferred_Initialization is
   begin
      declare
         Name : constant CORBA.String := CORBA.To_CORBA_String ("CodeSetId");
         Id   : constant CORBA.String
           := CORBA.To_CORBA_String ("IDL:omg.org/CONV_FRAME/CodeSetId:1.0");

      begin
         CORBA.TypeCode.Internals.Add_Parameter
           (TC_CodeSetId, CORBA.To_Any (Name));
         CORBA.TypeCode.Internals.Add_Parameter
           (TC_CodeSetId, CORBA.To_Any (Id));
         CORBA.TypeCode.Internals.Add_Parameter
           (TC_CodeSetId, CORBA.To_Any (CORBA.TC_Unsigned_Long));
      end;

      TC_IDL_SEQUENCE_CONV_FRAME_CodeSetId :=
        CORBA.TypeCode.Internals.Build_Sequence_TC (TC_CodeSetId, 0);
      IDL_SEQUENCE_CONV_FRAME_CodeSetId_Helper.Initialize
        (Element_TC  => TC_CodeSetId,
         Sequence_TC => TC_IDL_SEQUENCE_CONV_FRAME_CodeSetId);

      declare
         Name                          : constant CORBA.String
           := CORBA.To_CORBA_String ("CodeSetComponent");
         Id                            : constant CORBA.String
           := CORBA.To_CORBA_String
           ("IDL:omg.org/CONV_FRAME/CodeSetComponent:1.0");
         Arg_Name_Native_Code_Set      : constant CORBA.String
           := CORBA.To_CORBA_String ("native_code_set");
         Arg_Name_Conversion_Code_Sets : constant CORBA.String
           := CORBA.To_CORBA_String ("conversion_code_sets");

      begin
         CORBA.TypeCode.Internals.Add_Parameter
           (TC_CodeSetComponent, CORBA.To_Any (Name));
         CORBA.TypeCode.Internals.Add_Parameter
           (TC_CodeSetComponent, CORBA.To_Any (Id));
         CORBA.TypeCode.Internals.Add_Parameter
           (TC_CodeSetComponent, CORBA.To_Any (TC_CodeSetId));
         CORBA.TypeCode.Internals.Add_Parameter
           (TC_CodeSetComponent, CORBA.To_Any (Arg_Name_Native_Code_Set));
         CORBA.TypeCode.Internals.Add_Parameter
           (TC_CodeSetComponent,
            CORBA.To_Any (TC_IDL_SEQUENCE_CONV_FRAME_CodeSetId));
         CORBA.TypeCode.Internals.Add_Parameter
           (TC_CodeSetComponent, CORBA.To_Any (Arg_Name_Conversion_Code_Sets));
      end;

      declare
         Name                  : constant CORBA.String
           := CORBA.To_CORBA_String ("CodeSetComponentInfo");
         Id                    : constant CORBA.String
           := CORBA.To_CORBA_String
           ("IDL:omg.org/CONV_FRAME/CodeSetComponentInfo:1.0");
         Arg_Name_ForCharData  : constant CORBA.String
           := CORBA.To_CORBA_String ("ForCharData");
         Arg_Name_ForWcharData : constant CORBA.String
           := CORBA.To_CORBA_String ("ForWcharData");

      begin
         CORBA.TypeCode.Internals.Add_Parameter
           (TC_CodeSetComponentInfo, CORBA.To_Any (Name));
         CORBA.TypeCode.Internals.Add_Parameter
           (TC_CodeSetComponentInfo, CORBA.To_Any (Id));
         CORBA.TypeCode.Internals.Add_Parameter
           (TC_CodeSetComponentInfo, CORBA.To_Any (TC_CodeSetComponent));
         CORBA.TypeCode.Internals.Add_Parameter
           (TC_CodeSetComponentInfo, CORBA.To_Any (Arg_Name_ForCharData));
         CORBA.TypeCode.Internals.Add_Parameter
           (TC_CodeSetComponentInfo, CORBA.To_Any (TC_CodeSetComponent));
         CORBA.TypeCode.Internals.Add_Parameter
           (TC_CodeSetComponentInfo, CORBA.To_Any (Arg_Name_ForWcharData));
      end;

      declare
         Name                : constant CORBA.String
           := CORBA.To_CORBA_String ("CodeSetContext");
         Id                  : constant CORBA.String
           := CORBA.To_CORBA_String
           ("IDL:omg.org/CONV_FRAME/CodeSetContext:1.0");
         Arg_Name_Char_Data  : constant CORBA.String
           := CORBA.To_CORBA_String ("char_data");
         Arg_Name_Wchar_Data : constant CORBA.String
           := CORBA.To_CORBA_String ("wchar_data");

      begin
         CORBA.TypeCode.Internals.Add_Parameter
           (TC_CodeSetContext, CORBA.To_Any (Name));
         CORBA.TypeCode.Internals.Add_Parameter
           (TC_CodeSetContext, CORBA.To_Any (Id));
         CORBA.TypeCode.Internals.Add_Parameter
           (TC_CodeSetContext, CORBA.To_Any (TC_CodeSetId));
         CORBA.TypeCode.Internals.Add_Parameter
           (TC_CodeSetContext, CORBA.To_Any (Arg_Name_Char_Data));
         CORBA.TypeCode.Internals.Add_Parameter
           (TC_CodeSetContext, CORBA.To_Any (TC_CodeSetId));
         CORBA.TypeCode.Internals.Add_Parameter
           (TC_CodeSetContext, CORBA.To_Any (Arg_Name_Wchar_Data));
      end;
   end Deferred_Initialization;

begin
   declare
      use PolyORB.Initialization;
      use PolyORB.Initialization.String_Lists;
      use PolyORB.Utils.Strings;

   begin
      Register_Module
        (Module_Info'
         (Name      => +"CONV_FRAME.Helper",
          Conflicts => Empty,
          Depends   => +"any",
          Provides  => Empty,
          Implicit  => False,
          Init      => Deferred_Initialization'Access));
   end;
end CONV_FRAME.Helper;
