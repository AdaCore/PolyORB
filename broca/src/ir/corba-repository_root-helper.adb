----------------------------------------------
--  This file has been generated automatically
--  by AdaBroker (http://adabroker.eu.org/)
--
--  Do NOT hand-modify this file, as your
--  changes will be lost when you re-run the
--  IDL to Ada compiler.
----------------------------------------------

with CORBA.Repository_Root.ExceptionDef.Helper;
with CORBA.Repository_Root.IDLType.Helper;
with CORBA.Helper;
--  with CORBA.Repository_Root.ValueDef.Helper;
--  with CORBA.Repository_Root.InterfaceDef.Helper;
--  with CORBA.Repository_Root.Contained.Helper;

package body CORBA.Repository_Root.Helper is

   function From_Any (Item : in CORBA.Any)
      return CORBA.Repository_Root.DefinitionKind is
      Index : CORBA.Any :=
         CORBA.Get_Aggregate_Element (Item,
                                      CORBA.TC_Unsigned_Long,
                                      CORBA.Unsigned_Long (0));
      Position : CORBA.Unsigned_Long := CORBA.From_Any (Index);
   begin
      return DefinitionKind'Val (Position);
   end From_Any;

   function To_Any (Item : in CORBA.Repository_Root.DefinitionKind)
      return CORBA.Any is
      Result : CORBA.Any :=
         CORBA.Get_Empty_Any_Aggregate (TC_DefinitionKind);
   begin
      CORBA.Add_Aggregate_Element
         (Result,
          CORBA.To_Any (CORBA.Unsigned_Long (DefinitionKind'Pos (Item))));
      return Result;
   end To_Any;

   function From_Any (Item : in CORBA.Any)
      return CORBA.Repository_Root.VersionSpec is
      Result : CORBA.String:= CORBA.From_Any (Item);
   begin
      return CORBA.Repository_Root.VersionSpec (Result);
   end From_Any;

   function To_Any (Item : in CORBA.Repository_Root.VersionSpec)
      return CORBA.Any is
      Result : CORBA.Any := CORBA.To_Any (CORBA.String (Item));
   begin
      CORBA.Set_Type (Result, TC_VersionSpec);
      return Result;
   end To_Any;

--   function From_Any (Item : in CORBA.Any)
--      return CORBA.Repository_Root.IDL_SEQUENCE_CORBA_Repository_Root_Contained_Forward.Sequence is
--      use IDL_SEQUENCE_CORBA_Repository_Root_Contained_Forward;
--      Nb_Any : CORBA.Any :=
--         CORBA.Get_Aggregate_Element
--           (Item,
--            CORBA.TC_Unsigned_Long,
--            CORBA.Unsigned_Long (0));
--      Nb_Long : CORBA.Unsigned_Long := CORBA.From_Any (Nb_Any);
--      Nb : Integer := Integer (nb_Long);
--      Index : CORBA.Any;
--      Result : Element_Array (1 .. Nb);
--   begin
--      for I in 1 .. Nb loop
--         Index :=
--            CORBA.Get_Aggregate_Element (Item,
--                                         CORBA.Repository_Root.Contained.Helper.TC_Contained,
--                                         CORBA.Unsigned_Long (I));
--         Result (I) := CORBA.Repository_Root.Contained.Helper.From_Any (Index);
--      end loop;
--      return To_Sequence (Result);
--   end From_Any;
--
--   function To_Any (Item : in CORBA.Repository_Root.IDL_SEQUENCE_CORBA_Repository_Root_Contained_Forward.Sequence)
--      return CORBA.Any is
--      use IDL_SEQUENCE_CORBA_Repository_Root_Contained_Forward;
--      Array_Item : Element_Array := To_Element_Array (Item);
--      Result : CORBA.Any :=
--         CORBA.Get_Empty_Any_Aggregate (TC_IDL_SEQUENCE_CORBA_Repository_Root_Contained_Forward);
--   begin
--      CORBA.Add_Aggregate_Element
--         (Result,
--          CORBA.To_Any (CORBA.Unsigned_Long (Length (Item))));
--      for I in Array_Item'Range loop
--         CORBA.Add_Aggregate_Element (Result,
--                                      CORBA.Repository_Root.Contained.Helper.To_Any (Array_Item (I)));
--      end loop;
--      return Result;
--   end To_Any;
--
--   function From_Any (Item : in CORBA.Any)
--      return CORBA.Repository_Root.ContainedSeq is
--      Result : CORBA.Repository_Root.IDL_SEQUENCE_CORBA_Repository_Root_Contained_Forward.Sequence:= CORBA.Repository_Root.Helper.From_Any (Item);
--   begin
--      return CORBA.Repository_Root.ContainedSeq (Result);
--   end From_Any;
--
--   function To_Any (Item : in CORBA.Repository_Root.ContainedSeq)
--      return CORBA.Any is
--      Result : CORBA.Any := CORBA.Repository_Root.Helper.To_Any (CORBA.Repository_Root.IDL_SEQUENCE_CORBA_Repository_Root_Contained_Forward.Sequence (Item));
--   begin
--      CORBA.Set_Type (Result, TC_ContainedSeq);
--      return Result;
--   end To_Any;

--   function From_Any (Item : in CORBA.Any)
--      return CORBA.Repository_Root.IDL_SEQUENCE_CORBA_Repository_Root_InterfaceDef_Forward.Sequence is
--      use IDL_SEQUENCE_CORBA_Repository_Root_InterfaceDef_Forward;
--      Nb_Any : CORBA.Any :=
--         CORBA.Get_Aggregate_Element
--           (Item,
--            CORBA.TC_Unsigned_Long,
--            CORBA.Unsigned_Long (0));
--      Nb_Long : CORBA.Unsigned_Long := CORBA.From_Any (Nb_Any);
--      Nb : Integer := Integer (nb_Long);
--      Index : CORBA.Any;
--      Result : Element_Array (1 .. Nb);
--   begin
--      for I in 1 .. Nb loop
--         Index :=
--            CORBA.Get_Aggregate_Element (Item,
--                                         CORBA.Repository_Root.InterfaceDef.Helper.TC_InterfaceDef,
--                                         CORBA.Unsigned_Long (I));
--         Result (I) := CORBA.Repository_Root.InterfaceDef.Helper.From_Any (Index);
--      end loop;
--      return To_Sequence (Result);
--   end From_Any;
--
--   function To_Any (Item : in CORBA.Repository_Root.IDL_SEQUENCE_CORBA_Repository_Root_InterfaceDef_Forward.Sequence)
--      return CORBA.Any is
--      use IDL_SEQUENCE_CORBA_Repository_Root_InterfaceDef_Forward;
--      Array_Item : Element_Array := To_Element_Array (Item);
--      Result : CORBA.Any :=
--         CORBA.Get_Empty_Any_Aggregate (TC_IDL_SEQUENCE_CORBA_Repository_Root_InterfaceDef_Forward);
--   begin
--      CORBA.Add_Aggregate_Element
--         (Result,
--          CORBA.To_Any (CORBA.Unsigned_Long (Length (Item))));
--      for I in Array_Item'Range loop
--         CORBA.Add_Aggregate_Element (Result,
--                                      CORBA.Repository_Root.InterfaceDef.Helper.To_Any (Array_Item (I)));
--      end loop;
--      return Result;
--   end To_Any;
--
--   function From_Any (Item : in CORBA.Any)
--      return CORBA.Repository_Root.InterfaceDefSeq is
--      Result : CORBA.Repository_Root.IDL_SEQUENCE_CORBA_Repository_Root_InterfaceDef_Forward.Sequence:= CORBA.Repository_Root.Helper.From_Any (Item);
--   begin
--      return CORBA.Repository_Root.InterfaceDefSeq (Result);
--   end From_Any;
--
--   function To_Any (Item : in CORBA.Repository_Root.InterfaceDefSeq)
--      return CORBA.Any is
--     Result : CORBA.Any := CORBA.Repository_Root.Helper.To_Any (CORBA.Repository_Root.IDL_SEQUENCE_CORBA_Repository_Root_InterfaceDef_Forward.Sequence (Item));
--   begin
--      CORBA.Set_Type (Result, TC_InterfaceDefSeq);
--      return Result;
--   end To_Any;
--
--   function From_Any (Item : in CORBA.Any)
--      return CORBA.Repository_Root.IDL_SEQUENCE_CORBA_Repository_Root_ValueDef_Forward.Sequence is
--      use IDL_SEQUENCE_CORBA_Repository_Root_ValueDef_Forward;
--      Nb_Any : CORBA.Any :=
--         CORBA.Get_Aggregate_Element
--           (Item,
--            CORBA.TC_Unsigned_Long,
--            CORBA.Unsigned_Long (0));
--      Nb_Long : CORBA.Unsigned_Long := CORBA.From_Any (Nb_Any);
--      Nb : Integer := Integer (nb_Long);
--      Index : CORBA.Any;
--      Result : Element_Array (1 .. Nb);
--   begin
--      for I in 1 .. Nb loop
--         Index :=
--            CORBA.Get_Aggregate_Element (Item,
--                                         CORBA.Repository_Root.ValueDef.Helper.TC_ValueDef,
--                                         CORBA.Unsigned_Long (I));
--         Result (I) := CORBA.Repository_Root.ValueDef.Helper.From_Any (Index);
--      end loop;
--      return To_Sequence (Result);
--   end From_Any;

--   function To_Any (Item : in CORBA.Repository_Root.IDL_SEQUENCE_CORBA_Repository_Root_ValueDef_Forward.Sequence)
--      return CORBA.Any is
--      use IDL_SEQUENCE_CORBA_Repository_Root_ValueDef_Forward;
--      Array_Item : Element_Array := To_Element_Array (Item);
--      Result : CORBA.Any :=
--         CORBA.Get_Empty_Any_Aggregate (TC_IDL_SEQUENCE_CORBA_Repository_Root_ValueDef_Forward);
--   begin
--      CORBA.Add_Aggregate_Element
--         (Result,
--          CORBA.To_Any (CORBA.Unsigned_Long (Length (Item))));
--      for I in Array_Item'Range loop
--         CORBA.Add_Aggregate_Element (Result,
--                                      CORBA.Repository_Root.ValueDef.Helper.To_Any (Array_Item (I)));
--      end loop;
--      return Result;
--   end To_Any;

--   function From_Any (Item : in CORBA.Any)
--      return CORBA.Repository_Root.ValueDefSeq is
--      Result : CORBA.Repository_Root.IDL_SEQUENCE_CORBA_Repository_Root_ValueDef_Forward.Sequence:= CORBA.Repository_Root.Helper.From_Any (Item);
--   begin
--      return CORBA.Repository_Root.ValueDefSeq (Result);
--   end From_Any;

--   function To_Any (Item : in CORBA.Repository_Root.ValueDefSeq)
--      return CORBA.Any is
--      Result : CORBA.Any := CORBA.Repository_Root.Helper.To_Any (CORBA.Repository_Root.IDL_SEQUENCE_CORBA_Repository_Root_ValueDef_Forward.Sequence (Item));
--   begin
--      CORBA.Set_Type (Result, TC_ValueDefSeq);
--      return Result;
--   end To_Any;

   function From_Any (Item : in CORBA.Any)
      return CORBA.Repository_Root.OperationMode is
      Index : CORBA.Any :=
         CORBA.Get_Aggregate_Element (Item,
                                      CORBA.TC_Unsigned_Long,
                                      CORBA.Unsigned_Long (0));
      Position : CORBA.Unsigned_Long := CORBA.From_Any (Index);
   begin
      return OperationMode'Val (Position);
   end From_Any;

   function To_Any (Item : in CORBA.Repository_Root.OperationMode)
      return CORBA.Any is
      Result : CORBA.Any :=
         CORBA.Get_Empty_Any_Aggregate (TC_OperationMode);
   begin
      CORBA.Add_Aggregate_Element
         (Result,
          CORBA.To_Any (CORBA.Unsigned_Long (OperationMode'Pos (Item))));
      return Result;
   end To_Any;

   function From_Any (Item : in CORBA.Any)
      return CORBA.Repository_Root.ParameterMode is
      Index : CORBA.Any :=
         CORBA.Get_Aggregate_Element (Item,
                                      CORBA.TC_Unsigned_Long,
                                      CORBA.Unsigned_Long (0));
      Position : CORBA.Unsigned_Long := CORBA.From_Any (Index);
   begin
      return ParameterMode'Val (Position);
   end From_Any;

   function To_Any (Item : in CORBA.Repository_Root.ParameterMode)
      return CORBA.Any is
      Result : CORBA.Any :=
         CORBA.Get_Empty_Any_Aggregate (TC_ParameterMode);
   begin
      CORBA.Add_Aggregate_Element
         (Result,
          CORBA.To_Any (CORBA.Unsigned_Long (ParameterMode'Pos (Item))));
      return Result;
   end To_Any;

   function From_Any (Item : in CORBA.Any)
      return CORBA.Repository_Root.ParameterDescription is
      Index : CORBA.Any;
      Result_name : CORBA.Identifier;
      Result_IDL_type : CORBA.TypeCode.Object;
      Result_type_def : CORBA.Repository_Root.IDLType_Forward.Ref;
      Result_mode : CORBA.Repository_Root.ParameterMode;
   begin
      Index := CORBA.Get_Aggregate_Element (Item,
                                            CORBA.Helper.TC_Identifier,
                                            CORBA.Unsigned_Long ( 0));
      Result_name := CORBA.Helper.From_Any (Index);
      Index := CORBA.Get_Aggregate_Element (Item,
                                            CORBA.TC_TypeCode,
                                            CORBA.Unsigned_Long ( 1));
      Result_IDL_type := CORBA.From_Any (Index);
      Index := CORBA.Get_Aggregate_Element (Item,
                                            CORBA.Repository_Root.IDLType.Helper.TC_IDLType,
                                            CORBA.Unsigned_Long ( 2));
      Result_type_def := CORBA.Repository_Root.IDLType.Helper.From_Any (Index);
      Index := CORBA.Get_Aggregate_Element (Item,
                                            CORBA.Repository_Root.Helper.TC_ParameterMode,
                                            CORBA.Unsigned_Long ( 3));
      Result_mode := CORBA.Repository_Root.Helper.From_Any (Index);
      return
         (name => Result_name,
          IDL_type => Result_IDL_type,
          type_def => Result_type_def,
          mode => Result_mode);
   end From_Any;

   function To_Any (Item : in CORBA.Repository_Root.ParameterDescription)
      return CORBA.Any is
      Result : CORBA.Any :=
         CORBA.Get_Empty_Any_Aggregate (TC_ParameterDescription);
   begin
      CORBA.Add_Aggregate_Element
         (Result, CORBA.Helper.To_Any (Item.name));
      CORBA.Add_Aggregate_Element
         (Result, CORBA.To_Any (Item.IDL_type));
      CORBA.Add_Aggregate_Element
         (Result, CORBA.Repository_Root.IDLType.Helper.To_Any (Item.type_def));
      CORBA.Add_Aggregate_Element
         (Result, CORBA.Repository_Root.Helper.To_Any (Item.mode));
      return Result;
   end To_Any;

   function From_Any (Item : in CORBA.Any)
      return CORBA.Repository_Root.Visibility is
      Result : CORBA.Short:= CORBA.From_Any (Item);
   begin
      return CORBA.Repository_Root.Visibility (Result);
   end From_Any;

   function To_Any (Item : in CORBA.Repository_Root.Visibility)
      return CORBA.Any is
      Result : CORBA.Any := CORBA.To_Any (CORBA.Short (Item));
   begin
      CORBA.Set_Type (Result, TC_Visibility);
      return Result;
   end To_Any;

   function From_Any (Item : in CORBA.Any)
      return CORBA.Repository_Root.ValueMember is
      Index : CORBA.Any;
      Result_name : CORBA.Identifier;
      Result_id : CORBA.RepositoryId;
      Result_defined_in : CORBA.RepositoryId;
      Result_version : CORBA.Repository_Root.VersionSpec;
      Result_IDL_type : CORBA.TypeCode.Object;
      Result_type_def : CORBA.Repository_Root.IDLType_Forward.Ref;
      Result_IDL_access : CORBA.Repository_Root.Visibility;
   begin
      Index := CORBA.Get_Aggregate_Element (Item,
                                            CORBA.Helper.TC_Identifier,
                                            CORBA.Unsigned_Long ( 0));
      Result_name := CORBA.Helper.From_Any (Index);
      Index := CORBA.Get_Aggregate_Element (Item,
                                            CORBA.Helper.TC_RepositoryId,
                                            CORBA.Unsigned_Long ( 1));
      Result_id := CORBA.Helper.From_Any (Index);
      Index := CORBA.Get_Aggregate_Element (Item,
                                            CORBA.Helper.TC_RepositoryId,
                                            CORBA.Unsigned_Long ( 2));
      Result_defined_in := CORBA.Helper.From_Any (Index);
      Index := CORBA.Get_Aggregate_Element (Item,
                                            CORBA.Repository_Root.Helper.TC_VersionSpec,
                                            CORBA.Unsigned_Long ( 3));
      Result_version := CORBA.Repository_Root.Helper.From_Any (Index);
      Index := CORBA.Get_Aggregate_Element (Item,
                                            CORBA.TC_TypeCode,
                                            CORBA.Unsigned_Long ( 4));
      Result_IDL_type := CORBA.From_Any (Index);
      Index := CORBA.Get_Aggregate_Element (Item,
                                            CORBA.Repository_Root.IDLType.Helper.TC_IDLType,
                                            CORBA.Unsigned_Long ( 5));
      Result_type_def := CORBA.Repository_Root.IDLType.Helper.From_Any (Index);
      Index := CORBA.Get_Aggregate_Element (Item,
                                            CORBA.Repository_Root.Helper.TC_Visibility,
                                            CORBA.Unsigned_Long ( 6));
      Result_IDL_access := CORBA.Repository_Root.Helper.From_Any (Index);
      return
         (name => Result_name,
          id => Result_id,
          defined_in => Result_defined_in,
          version => Result_version,
          IDL_type => Result_IDL_type,
          type_def => Result_type_def,
          IDL_access => Result_IDL_access);
   end From_Any;

   function To_Any (Item : in CORBA.Repository_Root.ValueMember)
      return CORBA.Any is
      Result : CORBA.Any :=
         CORBA.Get_Empty_Any_Aggregate (TC_ValueMember);
   begin
      CORBA.Add_Aggregate_Element
         (Result, CORBA.Helper.To_Any (Item.name));
      CORBA.Add_Aggregate_Element
         (Result, CORBA.Helper.To_Any (Item.id));
      CORBA.Add_Aggregate_Element
         (Result, CORBA.Helper.To_Any (Item.defined_in));
      CORBA.Add_Aggregate_Element
         (Result, CORBA.Repository_Root.Helper.To_Any (Item.version));
      CORBA.Add_Aggregate_Element
         (Result, CORBA.To_Any (Item.IDL_type));
      CORBA.Add_Aggregate_Element
         (Result, CORBA.Repository_Root.IDLType.Helper.To_Any (Item.type_def));
      CORBA.Add_Aggregate_Element
         (Result, CORBA.Repository_Root.Helper.To_Any (Item.IDL_access));
      return Result;
   end To_Any;

   function From_Any (Item : in CORBA.Any)
      return CORBA.Repository_Root.IDL_SEQUENCE_CORBA_Repository_Root_ExceptionDef_Forward.Sequence is
      use IDL_SEQUENCE_CORBA_Repository_Root_ExceptionDef_Forward;
      Nb_Any : CORBA.Any :=
         CORBA.Get_Aggregate_Element
           (Item,
            CORBA.TC_Unsigned_Long,
            CORBA.Unsigned_Long (0));
      Nb_Long : CORBA.Unsigned_Long := CORBA.From_Any (Nb_Any);
      Nb : Integer := Integer (nb_Long);
      Index : CORBA.Any;
      Result : Element_Array (1 .. Nb);
   begin
      for I in 1 .. Nb loop
         Index :=
            CORBA.Get_Aggregate_Element (Item,
                                         CORBA.Repository_Root.ExceptionDef.Helper.TC_ExceptionDef,
                                         CORBA.Unsigned_Long (I));
         Result (I) := CORBA.Repository_Root.ExceptionDef.Helper.From_Any (Index);
      end loop;
      return To_Sequence (Result);
   end From_Any;

   function To_Any (Item : in CORBA.Repository_Root.IDL_SEQUENCE_CORBA_Repository_Root_ExceptionDef_Forward.Sequence)
      return CORBA.Any is
      use IDL_SEQUENCE_CORBA_Repository_Root_ExceptionDef_Forward;
      Array_Item : Element_Array := To_Element_Array (Item);
      Result : CORBA.Any :=
         CORBA.Get_Empty_Any_Aggregate (TC_IDL_SEQUENCE_CORBA_Repository_Root_ExceptionDef_Forward);
   begin
      CORBA.Add_Aggregate_Element
         (Result,
          CORBA.To_Any (CORBA.Unsigned_Long (Length (Item))));
      for I in Array_Item'Range loop
         CORBA.Add_Aggregate_Element (Result,
                                      CORBA.Repository_Root.ExceptionDef.Helper.To_Any (Array_Item (I)));
      end loop;
      return Result;
   end To_Any;

   function From_Any (Item : in CORBA.Any)
      return CORBA.Repository_Root.ExceptionDefSeq is
      Result : CORBA.Repository_Root.IDL_SEQUENCE_CORBA_Repository_Root_ExceptionDef_Forward.Sequence:= CORBA.Repository_Root.Helper.From_Any (Item);
   begin
      return CORBA.Repository_Root.ExceptionDefSeq (Result);
   end From_Any;

   function To_Any (Item : in CORBA.Repository_Root.ExceptionDefSeq)
      return CORBA.Any is
      Result : CORBA.Any := CORBA.Repository_Root.Helper.To_Any (CORBA.Repository_Root.IDL_SEQUENCE_CORBA_Repository_Root_ExceptionDef_Forward.Sequence (Item));
   begin
      CORBA.Set_Type (Result, TC_ExceptionDefSeq);
      return Result;
   end To_Any;

   function From_Any (Item : in CORBA.Any)
      return CORBA.Repository_Root.StructMember is
      Index : CORBA.Any;
      Result_name : CORBA.Identifier;
      Result_IDL_type : CORBA.TypeCode.Object;
      Result_type_def : CORBA.Repository_Root.IDLType_Forward.Ref;
   begin
      Index := CORBA.Get_Aggregate_Element (Item,
                                            CORBA.Helper.TC_Identifier,
                                            CORBA.Unsigned_Long ( 0));
      Result_name := CORBA.Helper.From_Any (Index);
      Index := CORBA.Get_Aggregate_Element (Item,
                                            CORBA.TC_TypeCode,
                                            CORBA.Unsigned_Long ( 1));
      Result_IDL_type := CORBA.From_Any (Index);
      Index := CORBA.Get_Aggregate_Element (Item,
                                            CORBA.Repository_Root.IDLType.Helper.TC_IDLType,
                                            CORBA.Unsigned_Long ( 2));
      Result_type_def := CORBA.Repository_Root.IDLType.Helper.From_Any (Index);
      return
         (name => Result_name,
          IDL_type => Result_IDL_type,
          type_def => Result_type_def);
   end From_Any;

   function To_Any (Item : in CORBA.Repository_Root.StructMember)
      return CORBA.Any is
      Result : CORBA.Any :=
         CORBA.Get_Empty_Any_Aggregate (TC_StructMember);
   begin
      CORBA.Add_Aggregate_Element
         (Result, CORBA.Helper.To_Any (Item.name));
      CORBA.Add_Aggregate_Element
         (Result, CORBA.To_Any (Item.IDL_type));
      CORBA.Add_Aggregate_Element
         (Result, CORBA.Repository_Root.IDLType.Helper.To_Any (Item.type_def));
      return Result;
   end To_Any;

   function From_Any (Item : in CORBA.Any)
      return CORBA.Repository_Root.IDL_SEQUENCE_CORBA_Repository_Root_StructMember.Sequence is
      use IDL_SEQUENCE_CORBA_Repository_Root_StructMember;
      Nb_Any : CORBA.Any :=
         CORBA.Get_Aggregate_Element
           (Item,
            CORBA.TC_Unsigned_Long,
            CORBA.Unsigned_Long (0));
      Nb_Long : CORBA.Unsigned_Long := CORBA.From_Any (Nb_Any);
      Nb : Integer := Integer (nb_Long);
      Index : CORBA.Any;
      Result : Element_Array (1 .. Nb);
   begin
      for I in 1 .. Nb loop
         Index :=
            CORBA.Get_Aggregate_Element (Item,
                                         CORBA.Repository_Root.Helper.TC_StructMember,
                                         CORBA.Unsigned_Long (I));
         Result (I) := CORBA.Repository_Root.Helper.From_Any (Index);
      end loop;
      return To_Sequence (Result);
   end From_Any;

   function To_Any (Item : in CORBA.Repository_Root.IDL_SEQUENCE_CORBA_Repository_Root_StructMember.Sequence)
      return CORBA.Any is
      use IDL_SEQUENCE_CORBA_Repository_Root_StructMember;
      Array_Item : Element_Array := To_Element_Array (Item);
      Result : CORBA.Any :=
         CORBA.Get_Empty_Any_Aggregate (TC_IDL_SEQUENCE_CORBA_Repository_Root_StructMember);
   begin
      CORBA.Add_Aggregate_Element
         (Result,
          CORBA.To_Any (CORBA.Unsigned_Long (Length (Item))));
      for I in Array_Item'Range loop
         CORBA.Add_Aggregate_Element (Result,
                                      CORBA.Repository_Root.Helper.To_Any (Array_Item (I)));
      end loop;
      return Result;
   end To_Any;

   function From_Any (Item : in CORBA.Any)
      return CORBA.Repository_Root.StructMemberSeq is
      Result : CORBA.Repository_Root.IDL_SEQUENCE_CORBA_Repository_Root_StructMember.Sequence:= CORBA.Repository_Root.Helper.From_Any (Item);
   begin
      return CORBA.Repository_Root.StructMemberSeq (Result);
   end From_Any;

   function To_Any (Item : in CORBA.Repository_Root.StructMemberSeq)
      return CORBA.Any is
      Result : CORBA.Any := CORBA.Repository_Root.Helper.To_Any (CORBA.Repository_Root.IDL_SEQUENCE_CORBA_Repository_Root_StructMember.Sequence (Item));
   begin
      CORBA.Set_Type (Result, TC_StructMemberSeq);
      return Result;
   end To_Any;

   function From_Any (Item : in CORBA.Any)
      return CORBA.Repository_Root.Initializer is
      Index : CORBA.Any;
      Result_members : CORBA.Repository_Root.StructMemberSeq;
      Result_name : CORBA.Identifier;
   begin
      Index := CORBA.Get_Aggregate_Element (Item,
                                            CORBA.Repository_Root.Helper.TC_StructMemberSeq,
                                            CORBA.Unsigned_Long ( 0));
      Result_members := CORBA.Repository_Root.Helper.From_Any (Index);
      Index := CORBA.Get_Aggregate_Element (Item,
                                            CORBA.Helper.TC_Identifier,
                                            CORBA.Unsigned_Long ( 1));
      Result_name := CORBA.Helper.From_Any (Index);
      return
         (members => Result_members,
          name => Result_name);
   end From_Any;

   function To_Any (Item : in CORBA.Repository_Root.Initializer)
      return CORBA.Any is
      Result : CORBA.Any :=
         CORBA.Get_Empty_Any_Aggregate (TC_Initializer);
   begin
      CORBA.Add_Aggregate_Element
         (Result, CORBA.Repository_Root.Helper.To_Any (Item.members));
      CORBA.Add_Aggregate_Element
         (Result, CORBA.Helper.To_Any (Item.name));
      return Result;
   end To_Any;

   function From_Any (Item : in CORBA.Any)
      return CORBA.Repository_Root.IDL_SEQUENCE_CORBA_Repository_Root_Initializer.Sequence is
      use IDL_SEQUENCE_CORBA_Repository_Root_Initializer;
      Nb_Any : CORBA.Any :=
         CORBA.Get_Aggregate_Element
           (Item,
            CORBA.TC_Unsigned_Long,
            CORBA.Unsigned_Long (0));
      Nb_Long : CORBA.Unsigned_Long := CORBA.From_Any (Nb_Any);
      Nb : Integer := Integer (nb_Long);
      Index : CORBA.Any;
      Result : Element_Array (1 .. Nb);
   begin
      for I in 1 .. Nb loop
         Index :=
            CORBA.Get_Aggregate_Element (Item,
                                         CORBA.Repository_Root.Helper.TC_Initializer,
                                         CORBA.Unsigned_Long (I));
         Result (I) := CORBA.Repository_Root.Helper.From_Any (Index);
      end loop;
      return To_Sequence (Result);
   end From_Any;

   function To_Any (Item : in CORBA.Repository_Root.IDL_SEQUENCE_CORBA_Repository_Root_Initializer.Sequence)
      return CORBA.Any is
      use IDL_SEQUENCE_CORBA_Repository_Root_Initializer;
      Array_Item : Element_Array := To_Element_Array (Item);
      Result : CORBA.Any :=
         CORBA.Get_Empty_Any_Aggregate (TC_IDL_SEQUENCE_CORBA_Repository_Root_Initializer);
   begin
      CORBA.Add_Aggregate_Element
         (Result,
          CORBA.To_Any (CORBA.Unsigned_Long (Length (Item))));
      for I in Array_Item'Range loop
         CORBA.Add_Aggregate_Element (Result,
                                      CORBA.Repository_Root.Helper.To_Any (Array_Item (I)));
      end loop;
      return Result;
   end To_Any;

   function From_Any (Item : in CORBA.Any)
      return CORBA.Repository_Root.InitializerSeq is
      Result : CORBA.Repository_Root.IDL_SEQUENCE_CORBA_Repository_Root_Initializer.Sequence:= CORBA.Repository_Root.Helper.From_Any (Item);
   begin
      return CORBA.Repository_Root.InitializerSeq (Result);
   end From_Any;

   function To_Any (Item : in CORBA.Repository_Root.InitializerSeq)
      return CORBA.Any is
      Result : CORBA.Any := CORBA.Repository_Root.Helper.To_Any (CORBA.Repository_Root.IDL_SEQUENCE_CORBA_Repository_Root_Initializer.Sequence (Item));
   begin
      CORBA.Set_Type (Result, TC_InitializerSeq);
      return Result;
   end To_Any;

   function From_Any (Item : in CORBA.Any)
      return CORBA.Repository_Root.UnionMember is
      Index : CORBA.Any;
      Result_name : CORBA.Identifier;
      Result_label : CORBA.Any;
      Result_IDL_type : CORBA.TypeCode.Object;
      Result_type_def : CORBA.Repository_Root.IDLType_Forward.Ref;
   begin
      Index := CORBA.Get_Aggregate_Element (Item,
                                            CORBA.Helper.TC_Identifier,
                                            CORBA.Unsigned_Long ( 0));
      Result_name := CORBA.Helper.From_Any (Index);
      Index := CORBA.Get_Aggregate_Element (Item,
                                            CORBA.TC_Any,
                                            CORBA.Unsigned_Long ( 1));
      Result_label := CORBA.From_Any (Index);
      Index := CORBA.Get_Aggregate_Element (Item,
                                            CORBA.TC_TypeCode,
                                            CORBA.Unsigned_Long ( 2));
      Result_IDL_type := CORBA.From_Any (Index);
      Index := CORBA.Get_Aggregate_Element (Item,
                                            CORBA.Repository_Root.IDLType.Helper.TC_IDLType,
                                            CORBA.Unsigned_Long ( 3));
      Result_type_def := CORBA.Repository_Root.IDLType.Helper.From_Any (Index);
      return
         (name => Result_name,
          label => Result_label,
          IDL_type => Result_IDL_type,
          type_def => Result_type_def);
   end From_Any;

   function To_Any (Item : in CORBA.Repository_Root.UnionMember)
      return CORBA.Any is
      Result : CORBA.Any :=
         CORBA.Get_Empty_Any_Aggregate (TC_UnionMember);
   begin
      CORBA.Add_Aggregate_Element
         (Result, CORBA.Helper.To_Any (Item.name));
      CORBA.Add_Aggregate_Element
         (Result, CORBA.To_Any (Item.label));
      CORBA.Add_Aggregate_Element
         (Result, CORBA.To_Any (Item.IDL_type));
      CORBA.Add_Aggregate_Element
         (Result, CORBA.Repository_Root.IDLType.Helper.To_Any (Item.type_def));
      return Result;
   end To_Any;

   function From_Any (Item : in CORBA.Any)
      return CORBA.Repository_Root.IDL_SEQUENCE_CORBA_Repository_Root_UnionMember.Sequence is
      use IDL_SEQUENCE_CORBA_Repository_Root_UnionMember;
      Nb_Any : CORBA.Any :=
         CORBA.Get_Aggregate_Element
           (Item,
            CORBA.TC_Unsigned_Long,
            CORBA.Unsigned_Long (0));
      Nb_Long : CORBA.Unsigned_Long := CORBA.From_Any (Nb_Any);
      Nb : Integer := Integer (nb_Long);
      Index : CORBA.Any;
      Result : Element_Array (1 .. Nb);
   begin
      for I in 1 .. Nb loop
         Index :=
            CORBA.Get_Aggregate_Element (Item,
                                         CORBA.Repository_Root.Helper.TC_UnionMember,
                                         CORBA.Unsigned_Long (I));
         Result (I) := CORBA.Repository_Root.Helper.From_Any (Index);
      end loop;
      return To_Sequence (Result);
   end From_Any;

   function To_Any (Item : in CORBA.Repository_Root.IDL_SEQUENCE_CORBA_Repository_Root_UnionMember.Sequence)
      return CORBA.Any is
      use IDL_SEQUENCE_CORBA_Repository_Root_UnionMember;
      Array_Item : Element_Array := To_Element_Array (Item);
      Result : CORBA.Any :=
         CORBA.Get_Empty_Any_Aggregate (TC_IDL_SEQUENCE_CORBA_Repository_Root_UnionMember);
   begin
      CORBA.Add_Aggregate_Element
         (Result,
          CORBA.To_Any (CORBA.Unsigned_Long (Length (Item))));
      for I in Array_Item'Range loop
         CORBA.Add_Aggregate_Element (Result,
                                      CORBA.Repository_Root.Helper.To_Any (Array_Item (I)));
      end loop;
      return Result;
   end To_Any;

   function From_Any (Item : in CORBA.Any)
      return CORBA.Repository_Root.UnionMemberSeq is
      Result : CORBA.Repository_Root.IDL_SEQUENCE_CORBA_Repository_Root_UnionMember.Sequence:= CORBA.Repository_Root.Helper.From_Any (Item);
   begin
      return CORBA.Repository_Root.UnionMemberSeq (Result);
   end From_Any;

   function To_Any (Item : in CORBA.Repository_Root.UnionMemberSeq)
      return CORBA.Any is
      Result : CORBA.Any := CORBA.Repository_Root.Helper.To_Any (CORBA.Repository_Root.IDL_SEQUENCE_CORBA_Repository_Root_UnionMember.Sequence (Item));
   begin
      CORBA.Set_Type (Result, TC_UnionMemberSeq);
      return Result;
   end To_Any;

   function From_Any (Item : in CORBA.Any)
      return CORBA.Repository_Root.IDL_SEQUENCE_CORBA_Identifier.Sequence is
      use IDL_SEQUENCE_CORBA_Identifier;
      Nb_Any : CORBA.Any :=
         CORBA.Get_Aggregate_Element
           (Item,
            CORBA.TC_Unsigned_Long,
            CORBA.Unsigned_Long (0));
      Nb_Long : CORBA.Unsigned_Long := CORBA.From_Any (Nb_Any);
      Nb : Integer := Integer (nb_Long);
      Index : CORBA.Any;
      Result : Element_Array (1 .. Nb);
   begin
      for I in 1 .. Nb loop
         Index :=
            CORBA.Get_Aggregate_Element (Item,
                                         CORBA.Helper.TC_Identifier,
                                         CORBA.Unsigned_Long (I));
         Result (I) := CORBA.Helper.From_Any (Index);
      end loop;
      return To_Sequence (Result);
   end From_Any;

   function To_Any (Item : in CORBA.Repository_Root.IDL_SEQUENCE_CORBA_Identifier.Sequence)
      return CORBA.Any is
      use IDL_SEQUENCE_CORBA_Identifier;
      Array_Item : Element_Array := To_Element_Array (Item);
      Result : CORBA.Any :=
         CORBA.Get_Empty_Any_Aggregate (TC_IDL_SEQUENCE_CORBA_Identifier);
   begin
      CORBA.Add_Aggregate_Element
         (Result,
          CORBA.To_Any (CORBA.Unsigned_Long (Length (Item))));
      for I in Array_Item'Range loop
         CORBA.Add_Aggregate_Element (Result,
                                      CORBA.Helper.To_Any (Array_Item (I)));
      end loop;
      return Result;
   end To_Any;

   function From_Any (Item : in CORBA.Any)
      return CORBA.Repository_Root.EnumMemberSeq is
      Result : CORBA.Repository_Root.IDL_SEQUENCE_CORBA_Identifier.Sequence:= CORBA.Repository_Root.Helper.From_Any (Item);
   begin
      return CORBA.Repository_Root.EnumMemberSeq (Result);
   end From_Any;

   function To_Any (Item : in CORBA.Repository_Root.EnumMemberSeq)
      return CORBA.Any is
      Result : CORBA.Any := CORBA.Repository_Root.Helper.To_Any (CORBA.Repository_Root.IDL_SEQUENCE_CORBA_Identifier.Sequence (Item));
   begin
      CORBA.Set_Type (Result, TC_EnumMemberSeq);
      return Result;
   end To_Any;

   function From_Any (Item : in CORBA.Any)
      return CORBA.Repository_Root.PrimitiveKind is
      Index : CORBA.Any :=
         CORBA.Get_Aggregate_Element (Item,
                                      CORBA.TC_Unsigned_Long,
                                      CORBA.Unsigned_Long (0));
      Position : CORBA.Unsigned_Long := CORBA.From_Any (Index);
   begin
      return PrimitiveKind'Val (Position);
   end From_Any;

   function To_Any (Item : in CORBA.Repository_Root.PrimitiveKind)
      return CORBA.Any is
      Result : CORBA.Any :=
         CORBA.Get_Empty_Any_Aggregate (TC_PrimitiveKind);
   begin
      CORBA.Add_Aggregate_Element
         (Result,
          CORBA.To_Any (CORBA.Unsigned_Long (PrimitiveKind'Pos (Item))));
      return Result;
   end To_Any;

   function From_Any (Item : in CORBA.Any)
      return CORBA.Repository_Root.ModuleDescription is
      Index : CORBA.Any;
      Result_name : CORBA.Identifier;
      Result_id : CORBA.RepositoryId;
      Result_defined_in : CORBA.RepositoryId;
      Result_version : CORBA.Repository_Root.VersionSpec;
   begin
      Index := CORBA.Get_Aggregate_Element (Item,
                                            CORBA.Helper.TC_Identifier,
                                            CORBA.Unsigned_Long ( 0));
      Result_name := CORBA.Helper.From_Any (Index);
      Index := CORBA.Get_Aggregate_Element (Item,
                                            CORBA.Helper.TC_RepositoryId,
                                            CORBA.Unsigned_Long ( 1));
      Result_id := CORBA.Helper.From_Any (Index);
      Index := CORBA.Get_Aggregate_Element (Item,
                                            CORBA.Helper.TC_RepositoryId,
                                            CORBA.Unsigned_Long ( 2));
      Result_defined_in := CORBA.Helper.From_Any (Index);
      Index := CORBA.Get_Aggregate_Element (Item,
                                            CORBA.Repository_Root.Helper.TC_VersionSpec,
                                            CORBA.Unsigned_Long ( 3));
      Result_version := CORBA.Repository_Root.Helper.From_Any (Index);
      return
         (name => Result_name,
          id => Result_id,
          defined_in => Result_defined_in,
          version => Result_version);
   end From_Any;

   function To_Any (Item : in CORBA.Repository_Root.ModuleDescription)
      return CORBA.Any is
      Result : CORBA.Any :=
         CORBA.Get_Empty_Any_Aggregate (TC_ModuleDescription);
   begin
      CORBA.Add_Aggregate_Element
         (Result, CORBA.Helper.To_Any (Item.name));
      CORBA.Add_Aggregate_Element
         (Result, CORBA.Helper.To_Any (Item.id));
      CORBA.Add_Aggregate_Element
         (Result, CORBA.Helper.To_Any (Item.defined_in));
      CORBA.Add_Aggregate_Element
         (Result, CORBA.Repository_Root.Helper.To_Any (Item.version));
      return Result;
   end To_Any;

   function From_Any (Item : in CORBA.Any)
      return CORBA.Repository_Root.ConstantDescription is
      Index : CORBA.Any;
      Result_name : CORBA.Identifier;
      Result_id : CORBA.RepositoryId;
      Result_defined_in : CORBA.RepositoryId;
      Result_version : CORBA.Repository_Root.VersionSpec;
      Result_IDL_type : CORBA.TypeCode.Object;
      Result_value : CORBA.Any;
   begin
      Index := CORBA.Get_Aggregate_Element (Item,
                                            CORBA.Helper.TC_Identifier,
                                            CORBA.Unsigned_Long ( 0));
      Result_name := CORBA.Helper.From_Any (Index);
      Index := CORBA.Get_Aggregate_Element (Item,
                                            CORBA.Helper.TC_RepositoryId,
                                            CORBA.Unsigned_Long ( 1));
      Result_id := CORBA.Helper.From_Any (Index);
      Index := CORBA.Get_Aggregate_Element (Item,
                                            CORBA.Helper.TC_RepositoryId,
                                            CORBA.Unsigned_Long ( 2));
      Result_defined_in := CORBA.Helper.From_Any (Index);
      Index := CORBA.Get_Aggregate_Element (Item,
                                            CORBA.Repository_Root.Helper.TC_VersionSpec,
                                            CORBA.Unsigned_Long ( 3));
      Result_version := CORBA.Repository_Root.Helper.From_Any (Index);
      Index := CORBA.Get_Aggregate_Element (Item,
                                            CORBA.TC_TypeCode,
                                            CORBA.Unsigned_Long ( 4));
      Result_IDL_type := CORBA.From_Any (Index);
      Index := CORBA.Get_Aggregate_Element (Item,
                                            CORBA.TC_Any,
                                            CORBA.Unsigned_Long ( 5));
      Result_value := CORBA.From_Any (Index);
      return
         (name => Result_name,
          id => Result_id,
          defined_in => Result_defined_in,
          version => Result_version,
          IDL_type => Result_IDL_type,
          value => Result_value);
   end From_Any;

   function To_Any (Item : in CORBA.Repository_Root.ConstantDescription)
      return CORBA.Any is
      Result : CORBA.Any :=
         CORBA.Get_Empty_Any_Aggregate (TC_ConstantDescription);
   begin
      CORBA.Add_Aggregate_Element
         (Result, CORBA.Helper.To_Any (Item.name));
      CORBA.Add_Aggregate_Element
         (Result, CORBA.Helper.To_Any (Item.id));
      CORBA.Add_Aggregate_Element
         (Result, CORBA.Helper.To_Any (Item.defined_in));
      CORBA.Add_Aggregate_Element
         (Result, CORBA.Repository_Root.Helper.To_Any (Item.version));
      CORBA.Add_Aggregate_Element
         (Result, CORBA.To_Any (Item.IDL_type));
      CORBA.Add_Aggregate_Element
         (Result, CORBA.To_Any (Item.value));
      return Result;
   end To_Any;

   function From_Any (Item : in CORBA.Any)
      return CORBA.Repository_Root.TypeDescription is
      Index : CORBA.Any;
      Result_name : CORBA.Identifier;
      Result_id : CORBA.RepositoryId;
      Result_defined_in : CORBA.RepositoryId;
      Result_version : CORBA.Repository_Root.VersionSpec;
      Result_IDL_type : CORBA.TypeCode.Object;
   begin
      Index := CORBA.Get_Aggregate_Element (Item,
                                            CORBA.Helper.TC_Identifier,
                                            CORBA.Unsigned_Long ( 0));
      Result_name := CORBA.Helper.From_Any (Index);
      Index := CORBA.Get_Aggregate_Element (Item,
                                            CORBA.Helper.TC_RepositoryId,
                                            CORBA.Unsigned_Long ( 1));
      Result_id := CORBA.Helper.From_Any (Index);
      Index := CORBA.Get_Aggregate_Element (Item,
                                            CORBA.Helper.TC_RepositoryId,
                                            CORBA.Unsigned_Long ( 2));
      Result_defined_in := CORBA.Helper.From_Any (Index);
      Index := CORBA.Get_Aggregate_Element (Item,
                                            CORBA.Repository_Root.Helper.TC_VersionSpec,
                                            CORBA.Unsigned_Long ( 3));
      Result_version := CORBA.Repository_Root.Helper.From_Any (Index);
      Index := CORBA.Get_Aggregate_Element (Item,
                                            CORBA.TC_TypeCode,
                                            CORBA.Unsigned_Long ( 4));
      Result_IDL_type := CORBA.From_Any (Index);
      return
         (name => Result_name,
          id => Result_id,
          defined_in => Result_defined_in,
          version => Result_version,
          IDL_type => Result_IDL_type);
   end From_Any;

   function To_Any (Item : in CORBA.Repository_Root.TypeDescription)
      return CORBA.Any is
      Result : CORBA.Any :=
         CORBA.Get_Empty_Any_Aggregate (TC_TypeDescription);
   begin
      CORBA.Add_Aggregate_Element
         (Result, CORBA.Helper.To_Any (Item.name));
      CORBA.Add_Aggregate_Element
         (Result, CORBA.Helper.To_Any (Item.id));
      CORBA.Add_Aggregate_Element
         (Result, CORBA.Helper.To_Any (Item.defined_in));
      CORBA.Add_Aggregate_Element
         (Result, CORBA.Repository_Root.Helper.To_Any (Item.version));
      CORBA.Add_Aggregate_Element
         (Result, CORBA.To_Any (Item.IDL_type));
      return Result;
   end To_Any;

   function From_Any (Item : in CORBA.Any)
      return CORBA.Repository_Root.ExceptionDescription is
      Index : CORBA.Any;
      Result_name : CORBA.Identifier;
      Result_id : CORBA.RepositoryId;
      Result_defined_in : CORBA.RepositoryId;
      Result_version : CORBA.Repository_Root.VersionSpec;
      Result_IDL_type : CORBA.TypeCode.Object;
   begin
      Index := CORBA.Get_Aggregate_Element (Item,
                                            CORBA.Helper.TC_Identifier,
                                            CORBA.Unsigned_Long ( 0));
      Result_name := CORBA.Helper.From_Any (Index);
      Index := CORBA.Get_Aggregate_Element (Item,
                                            CORBA.Helper.TC_RepositoryId,
                                            CORBA.Unsigned_Long ( 1));
      Result_id := CORBA.Helper.From_Any (Index);
      Index := CORBA.Get_Aggregate_Element (Item,
                                            CORBA.Helper.TC_RepositoryId,
                                            CORBA.Unsigned_Long ( 2));
      Result_defined_in := CORBA.Helper.From_Any (Index);
      Index := CORBA.Get_Aggregate_Element (Item,
                                            CORBA.Repository_Root.Helper.TC_VersionSpec,
                                            CORBA.Unsigned_Long ( 3));
      Result_version := CORBA.Repository_Root.Helper.From_Any (Index);
      Index := CORBA.Get_Aggregate_Element (Item,
                                            CORBA.TC_TypeCode,
                                            CORBA.Unsigned_Long ( 4));
      Result_IDL_type := CORBA.From_Any (Index);
      return
         (name => Result_name,
          id => Result_id,
          defined_in => Result_defined_in,
          version => Result_version,
          IDL_type => Result_IDL_type);
   end From_Any;

   function To_Any (Item : in CORBA.Repository_Root.ExceptionDescription)
      return CORBA.Any is
      Result : CORBA.Any :=
         CORBA.Get_Empty_Any_Aggregate (TC_ExceptionDescription);
   begin
      CORBA.Add_Aggregate_Element
         (Result, CORBA.Helper.To_Any (Item.name));
      CORBA.Add_Aggregate_Element
         (Result, CORBA.Helper.To_Any (Item.id));
      CORBA.Add_Aggregate_Element
         (Result, CORBA.Helper.To_Any (Item.defined_in));
      CORBA.Add_Aggregate_Element
         (Result, CORBA.Repository_Root.Helper.To_Any (Item.version));
      CORBA.Add_Aggregate_Element
         (Result, CORBA.To_Any (Item.IDL_type));
      return Result;
   end To_Any;

   function From_Any (Item : in CORBA.Any)
      return CORBA.Repository_Root.AttributeMode is
      Index : CORBA.Any :=
         CORBA.Get_Aggregate_Element (Item,
                                      CORBA.TC_Unsigned_Long,
                                      CORBA.Unsigned_Long (0));
      Position : CORBA.Unsigned_Long := CORBA.From_Any (Index);
   begin
      return AttributeMode'Val (Position);
   end From_Any;

   function To_Any (Item : in CORBA.Repository_Root.AttributeMode)
      return CORBA.Any is
      Result : CORBA.Any :=
         CORBA.Get_Empty_Any_Aggregate (TC_AttributeMode);
   begin
      CORBA.Add_Aggregate_Element
         (Result,
          CORBA.To_Any (CORBA.Unsigned_Long (AttributeMode'Pos (Item))));
      return Result;
   end To_Any;

   function From_Any (Item : in CORBA.Any)
      return CORBA.Repository_Root.AttributeDescription is
      Index : CORBA.Any;
      Result_name : CORBA.Identifier;
      Result_id : CORBA.RepositoryId;
      Result_defined_in : CORBA.RepositoryId;
      Result_version : CORBA.Repository_Root.VersionSpec;
      Result_IDL_type : CORBA.TypeCode.Object;
      Result_mode : CORBA.Repository_Root.AttributeMode;
   begin
      Index := CORBA.Get_Aggregate_Element (Item,
                                            CORBA.Helper.TC_Identifier,
                                            CORBA.Unsigned_Long ( 0));
      Result_name := CORBA.Helper.From_Any (Index);
      Index := CORBA.Get_Aggregate_Element (Item,
                                            CORBA.Helper.TC_RepositoryId,
                                            CORBA.Unsigned_Long ( 1));
      Result_id := CORBA.Helper.From_Any (Index);
      Index := CORBA.Get_Aggregate_Element (Item,
                                            CORBA.Helper.TC_RepositoryId,
                                            CORBA.Unsigned_Long ( 2));
      Result_defined_in := CORBA.Helper.From_Any (Index);
      Index := CORBA.Get_Aggregate_Element (Item,
                                            CORBA.Repository_Root.Helper.TC_VersionSpec,
                                            CORBA.Unsigned_Long ( 3));
      Result_version := CORBA.Repository_Root.Helper.From_Any (Index);
      Index := CORBA.Get_Aggregate_Element (Item,
                                            CORBA.TC_TypeCode,
                                            CORBA.Unsigned_Long ( 4));
      Result_IDL_type := CORBA.From_Any (Index);
      Index := CORBA.Get_Aggregate_Element (Item,
                                            CORBA.Repository_Root.Helper.TC_AttributeMode,
                                            CORBA.Unsigned_Long ( 5));
      Result_mode := CORBA.Repository_Root.Helper.From_Any (Index);
      return
         (name => Result_name,
          id => Result_id,
          defined_in => Result_defined_in,
          version => Result_version,
          IDL_type => Result_IDL_type,
          mode => Result_mode);
   end From_Any;

   function To_Any (Item : in CORBA.Repository_Root.AttributeDescription)
      return CORBA.Any is
      Result : CORBA.Any :=
         CORBA.Get_Empty_Any_Aggregate (TC_AttributeDescription);
   begin
      CORBA.Add_Aggregate_Element
         (Result, CORBA.Helper.To_Any (Item.name));
      CORBA.Add_Aggregate_Element
         (Result, CORBA.Helper.To_Any (Item.id));
      CORBA.Add_Aggregate_Element
         (Result, CORBA.Helper.To_Any (Item.defined_in));
      CORBA.Add_Aggregate_Element
         (Result, CORBA.Repository_Root.Helper.To_Any (Item.version));
      CORBA.Add_Aggregate_Element
         (Result, CORBA.To_Any (Item.IDL_type));
      CORBA.Add_Aggregate_Element
         (Result, CORBA.Repository_Root.Helper.To_Any (Item.mode));
      return Result;
   end To_Any;

   function From_Any (Item : in CORBA.Any)
      return CORBA.Repository_Root.IDL_SEQUENCE_CORBA_Repository_Root_ParameterDescription.Sequence is
      use IDL_SEQUENCE_CORBA_Repository_Root_ParameterDescription;
      Nb_Any : CORBA.Any :=
         CORBA.Get_Aggregate_Element
           (Item,
            CORBA.TC_Unsigned_Long,
            CORBA.Unsigned_Long (0));
      Nb_Long : CORBA.Unsigned_Long := CORBA.From_Any (Nb_Any);
      Nb : Integer := Integer (nb_Long);
      Index : CORBA.Any;
      Result : Element_Array (1 .. Nb);
   begin
      for I in 1 .. Nb loop
         Index :=
            CORBA.Get_Aggregate_Element (Item,
                                         CORBA.Repository_Root.Helper.TC_ParameterDescription,
                                         CORBA.Unsigned_Long (I));
         Result (I) := CORBA.Repository_Root.Helper.From_Any (Index);
      end loop;
      return To_Sequence (Result);
   end From_Any;

   function To_Any (Item : in CORBA.Repository_Root.IDL_SEQUENCE_CORBA_Repository_Root_ParameterDescription.Sequence)
      return CORBA.Any is
      use IDL_SEQUENCE_CORBA_Repository_Root_ParameterDescription;
      Array_Item : Element_Array := To_Element_Array (Item);
      Result : CORBA.Any :=
         CORBA.Get_Empty_Any_Aggregate (TC_IDL_SEQUENCE_CORBA_Repository_Root_ParameterDescription);
   begin
      CORBA.Add_Aggregate_Element
         (Result,
          CORBA.To_Any (CORBA.Unsigned_Long (Length (Item))));
      for I in Array_Item'Range loop
         CORBA.Add_Aggregate_Element (Result,
                                      CORBA.Repository_Root.Helper.To_Any (Array_Item (I)));
      end loop;
      return Result;
   end To_Any;

   function From_Any (Item : in CORBA.Any)
      return CORBA.Repository_Root.ParDescriptionSeq is
      Result : CORBA.Repository_Root.IDL_SEQUENCE_CORBA_Repository_Root_ParameterDescription.Sequence:= CORBA.Repository_Root.Helper.From_Any (Item);
   begin
      return CORBA.Repository_Root.ParDescriptionSeq (Result);
   end From_Any;

   function To_Any (Item : in CORBA.Repository_Root.ParDescriptionSeq)
      return CORBA.Any is
      Result : CORBA.Any := CORBA.Repository_Root.Helper.To_Any (CORBA.Repository_Root.IDL_SEQUENCE_CORBA_Repository_Root_ParameterDescription.Sequence (Item));
   begin
      CORBA.Set_Type (Result, TC_ParDescriptionSeq);
      return Result;
   end To_Any;

   function From_Any (Item : in CORBA.Any)
      return CORBA.Repository_Root.ContextIdentifier is
      Result : CORBA.Identifier:= CORBA.Helper.From_Any (Item);
   begin
      return CORBA.Repository_Root.ContextIdentifier (Result);
   end From_Any;

   function To_Any (Item : in CORBA.Repository_Root.ContextIdentifier)
      return CORBA.Any is
      Result : CORBA.Any := CORBA.Helper.To_Any (CORBA.Identifier (Item));
   begin
      CORBA.Set_Type (Result, TC_ContextIdentifier);
      return Result;
   end To_Any;

   function From_Any (Item : in CORBA.Any)
      return CORBA.Repository_Root.IDL_SEQUENCE_CORBA_Repository_Root_ContextIdentifier.Sequence is
      use IDL_SEQUENCE_CORBA_Repository_Root_ContextIdentifier;
      Nb_Any : CORBA.Any :=
         CORBA.Get_Aggregate_Element
           (Item,
            CORBA.TC_Unsigned_Long,
            CORBA.Unsigned_Long (0));
      Nb_Long : CORBA.Unsigned_Long := CORBA.From_Any (Nb_Any);
      Nb : Integer := Integer (nb_Long);
      Index : CORBA.Any;
      Result : Element_Array (1 .. Nb);
   begin
      for I in 1 .. Nb loop
         Index :=
            CORBA.Get_Aggregate_Element (Item,
                                         CORBA.Repository_Root.Helper.TC_ContextIdentifier,
                                         CORBA.Unsigned_Long (I));
         Result (I) := CORBA.Repository_Root.Helper.From_Any (Index);
      end loop;
      return To_Sequence (Result);
   end From_Any;

   function To_Any (Item : in CORBA.Repository_Root.IDL_SEQUENCE_CORBA_Repository_Root_ContextIdentifier.Sequence)
      return CORBA.Any is
      use IDL_SEQUENCE_CORBA_Repository_Root_ContextIdentifier;
      Array_Item : Element_Array := To_Element_Array (Item);
      Result : CORBA.Any :=
         CORBA.Get_Empty_Any_Aggregate (TC_IDL_SEQUENCE_CORBA_Repository_Root_ContextIdentifier);
   begin
      CORBA.Add_Aggregate_Element
         (Result,
          CORBA.To_Any (CORBA.Unsigned_Long (Length (Item))));
      for I in Array_Item'Range loop
         CORBA.Add_Aggregate_Element (Result,
                                      CORBA.Repository_Root.Helper.To_Any (Array_Item (I)));
      end loop;
      return Result;
   end To_Any;

   function From_Any (Item : in CORBA.Any)
      return CORBA.Repository_Root.ContextIdSeq is
      Result : CORBA.Repository_Root.IDL_SEQUENCE_CORBA_Repository_Root_ContextIdentifier.Sequence:= CORBA.Repository_Root.Helper.From_Any (Item);
   begin
      return CORBA.Repository_Root.ContextIdSeq (Result);
   end From_Any;

   function To_Any (Item : in CORBA.Repository_Root.ContextIdSeq)
      return CORBA.Any is
      Result : CORBA.Any := CORBA.Repository_Root.Helper.To_Any (CORBA.Repository_Root.IDL_SEQUENCE_CORBA_Repository_Root_ContextIdentifier.Sequence (Item));
   begin
      CORBA.Set_Type (Result, TC_ContextIdSeq);
      return Result;
   end To_Any;

   function From_Any (Item : in CORBA.Any)
      return CORBA.Repository_Root.IDL_SEQUENCE_CORBA_Repository_Root_ExceptionDescription.Sequence is
      use IDL_SEQUENCE_CORBA_Repository_Root_ExceptionDescription;
      Nb_Any : CORBA.Any :=
         CORBA.Get_Aggregate_Element
           (Item,
            CORBA.TC_Unsigned_Long,
            CORBA.Unsigned_Long (0));
      Nb_Long : CORBA.Unsigned_Long := CORBA.From_Any (Nb_Any);
      Nb : Integer := Integer (nb_Long);
      Index : CORBA.Any;
      Result : Element_Array (1 .. Nb);
   begin
      for I in 1 .. Nb loop
         Index :=
            CORBA.Get_Aggregate_Element (Item,
                                         CORBA.Repository_Root.Helper.TC_ExceptionDescription,
                                         CORBA.Unsigned_Long (I));
         Result (I) := CORBA.Repository_Root.Helper.From_Any (Index);
      end loop;
      return To_Sequence (Result);
   end From_Any;

   function To_Any (Item : in CORBA.Repository_Root.IDL_SEQUENCE_CORBA_Repository_Root_ExceptionDescription.Sequence)
      return CORBA.Any is
      use IDL_SEQUENCE_CORBA_Repository_Root_ExceptionDescription;
      Array_Item : Element_Array := To_Element_Array (Item);
      Result : CORBA.Any :=
         CORBA.Get_Empty_Any_Aggregate (TC_IDL_SEQUENCE_CORBA_Repository_Root_ExceptionDescription);
   begin
      CORBA.Add_Aggregate_Element
         (Result,
          CORBA.To_Any (CORBA.Unsigned_Long (Length (Item))));
      for I in Array_Item'Range loop
         CORBA.Add_Aggregate_Element (Result,
                                      CORBA.Repository_Root.Helper.To_Any (Array_Item (I)));
      end loop;
      return Result;
   end To_Any;

   function From_Any (Item : in CORBA.Any)
      return CORBA.Repository_Root.ExcDescriptionSeq is
      Result : CORBA.Repository_Root.IDL_SEQUENCE_CORBA_Repository_Root_ExceptionDescription.Sequence:= CORBA.Repository_Root.Helper.From_Any (Item);
   begin
      return CORBA.Repository_Root.ExcDescriptionSeq (Result);
   end From_Any;

   function To_Any (Item : in CORBA.Repository_Root.ExcDescriptionSeq)
      return CORBA.Any is
      Result : CORBA.Any := CORBA.Repository_Root.Helper.To_Any (CORBA.Repository_Root.IDL_SEQUENCE_CORBA_Repository_Root_ExceptionDescription.Sequence (Item));
   begin
      CORBA.Set_Type (Result, TC_ExcDescriptionSeq);
      return Result;
   end To_Any;

   function From_Any (Item : in CORBA.Any)
      return CORBA.Repository_Root.OperationDescription is
      Index : CORBA.Any;
      Result_name : CORBA.Identifier;
      Result_id : CORBA.RepositoryId;
      Result_defined_in : CORBA.RepositoryId;
      Result_version : CORBA.Repository_Root.VersionSpec;
      Result_result : CORBA.TypeCode.Object;
      Result_mode : CORBA.Repository_Root.OperationMode;
      Result_contexts : CORBA.Repository_Root.ContextIdSeq;
      Result_parameters : CORBA.Repository_Root.ParDescriptionSeq;
      Result_exceptions : CORBA.Repository_Root.ExcDescriptionSeq;
   begin
      Index := CORBA.Get_Aggregate_Element (Item,
                                            CORBA.Helper.TC_Identifier,
                                            CORBA.Unsigned_Long ( 0));
      Result_name := CORBA.Helper.From_Any (Index);
      Index := CORBA.Get_Aggregate_Element (Item,
                                            CORBA.Helper.TC_RepositoryId,
                                            CORBA.Unsigned_Long ( 1));
      Result_id := CORBA.Helper.From_Any (Index);
      Index := CORBA.Get_Aggregate_Element (Item,
                                            CORBA.Helper.TC_RepositoryId,
                                            CORBA.Unsigned_Long ( 2));
      Result_defined_in := CORBA.Helper.From_Any (Index);
      Index := CORBA.Get_Aggregate_Element (Item,
                                            CORBA.Repository_Root.Helper.TC_VersionSpec,
                                            CORBA.Unsigned_Long ( 3));
      Result_version := CORBA.Repository_Root.Helper.From_Any (Index);
      Index := CORBA.Get_Aggregate_Element (Item,
                                            CORBA.TC_TypeCode,
                                            CORBA.Unsigned_Long ( 4));
      Result_result := CORBA.From_Any (Index);
      Index := CORBA.Get_Aggregate_Element (Item,
                                            CORBA.Repository_Root.Helper.TC_OperationMode,
                                            CORBA.Unsigned_Long ( 5));
      Result_mode := CORBA.Repository_Root.Helper.From_Any (Index);
      Index := CORBA.Get_Aggregate_Element (Item,
                                            CORBA.Repository_Root.Helper.TC_ContextIdSeq,
                                            CORBA.Unsigned_Long ( 6));
      Result_contexts := CORBA.Repository_Root.Helper.From_Any (Index);
      Index := CORBA.Get_Aggregate_Element (Item,
                                            CORBA.Repository_Root.Helper.TC_ParDescriptionSeq,
                                            CORBA.Unsigned_Long ( 7));
      Result_parameters := CORBA.Repository_Root.Helper.From_Any (Index);
      Index := CORBA.Get_Aggregate_Element (Item,
                                            CORBA.Repository_Root.Helper.TC_ExcDescriptionSeq,
                                            CORBA.Unsigned_Long ( 8));
      Result_exceptions := CORBA.Repository_Root.Helper.From_Any (Index);
      return
         (name => Result_name,
          id => Result_id,
          defined_in => Result_defined_in,
          version => Result_version,
          result => Result_result,
          mode => Result_mode,
          contexts => Result_contexts,
          parameters => Result_parameters,
          exceptions => Result_exceptions);
   end From_Any;

   function To_Any (Item : in CORBA.Repository_Root.OperationDescription)
      return CORBA.Any is
      Result : CORBA.Any :=
         CORBA.Get_Empty_Any_Aggregate (TC_OperationDescription);
   begin
      CORBA.Add_Aggregate_Element
         (Result, CORBA.Helper.To_Any (Item.name));
      CORBA.Add_Aggregate_Element
         (Result, CORBA.Helper.To_Any (Item.id));
      CORBA.Add_Aggregate_Element
         (Result, CORBA.Helper.To_Any (Item.defined_in));
      CORBA.Add_Aggregate_Element
         (Result, CORBA.Repository_Root.Helper.To_Any (Item.version));
      CORBA.Add_Aggregate_Element
         (Result, CORBA.To_Any (Item.result));
      CORBA.Add_Aggregate_Element
         (Result, CORBA.Repository_Root.Helper.To_Any (Item.mode));
      CORBA.Add_Aggregate_Element
         (Result, CORBA.Repository_Root.Helper.To_Any (Item.contexts));
      CORBA.Add_Aggregate_Element
         (Result, CORBA.Repository_Root.Helper.To_Any (Item.parameters));
      CORBA.Add_Aggregate_Element
         (Result, CORBA.Repository_Root.Helper.To_Any (Item.exceptions));
      return Result;
   end To_Any;

   function From_Any (Item : in CORBA.Any)
      return CORBA.Repository_Root.IDL_SEQUENCE_CORBA_RepositoryId.Sequence is
      use IDL_SEQUENCE_CORBA_RepositoryId;
      Nb_Any : CORBA.Any :=
         CORBA.Get_Aggregate_Element
           (Item,
            CORBA.TC_Unsigned_Long,
            CORBA.Unsigned_Long (0));
      Nb_Long : CORBA.Unsigned_Long := CORBA.From_Any (Nb_Any);
      Nb : Integer := Integer (nb_Long);
      Index : CORBA.Any;
      Result : Element_Array (1 .. Nb);
   begin
      for I in 1 .. Nb loop
         Index :=
            CORBA.Get_Aggregate_Element (Item,
                                         CORBA.Helper.TC_RepositoryId,
                                         CORBA.Unsigned_Long (I));
         Result (I) := CORBA.Helper.From_Any (Index);
      end loop;
      return To_Sequence (Result);
   end From_Any;

   function To_Any (Item : in CORBA.Repository_Root.IDL_SEQUENCE_CORBA_RepositoryId.Sequence)
      return CORBA.Any is
      use IDL_SEQUENCE_CORBA_RepositoryId;
      Array_Item : Element_Array := To_Element_Array (Item);
      Result : CORBA.Any :=
         CORBA.Get_Empty_Any_Aggregate (TC_IDL_SEQUENCE_CORBA_RepositoryId);
   begin
      CORBA.Add_Aggregate_Element
         (Result,
          CORBA.To_Any (CORBA.Unsigned_Long (Length (Item))));
      for I in Array_Item'Range loop
         CORBA.Add_Aggregate_Element (Result,
                                      CORBA.Helper.To_Any (Array_Item (I)));
      end loop;
      return Result;
   end To_Any;

   function From_Any (Item : in CORBA.Any)
      return CORBA.Repository_Root.RepositoryIdSeq is
      Result : CORBA.Repository_Root.IDL_SEQUENCE_CORBA_RepositoryId.Sequence:= CORBA.Repository_Root.Helper.From_Any (Item);
   begin
      return CORBA.Repository_Root.RepositoryIdSeq (Result);
   end From_Any;

   function To_Any (Item : in CORBA.Repository_Root.RepositoryIdSeq)
      return CORBA.Any is
      Result : CORBA.Any := CORBA.Repository_Root.Helper.To_Any (CORBA.Repository_Root.IDL_SEQUENCE_CORBA_RepositoryId.Sequence (Item));
   begin
      CORBA.Set_Type (Result, TC_RepositoryIdSeq);
      return Result;
   end To_Any;

   function From_Any (Item : in CORBA.Any)
      return CORBA.Repository_Root.IDL_SEQUENCE_CORBA_Repository_Root_OperationDescription.Sequence is
      use IDL_SEQUENCE_CORBA_Repository_Root_OperationDescription;
      Nb_Any : CORBA.Any :=
         CORBA.Get_Aggregate_Element
           (Item,
            CORBA.TC_Unsigned_Long,
            CORBA.Unsigned_Long (0));
      Nb_Long : CORBA.Unsigned_Long := CORBA.From_Any (Nb_Any);
      Nb : Integer := Integer (nb_Long);
      Index : CORBA.Any;
      Result : Element_Array (1 .. Nb);
   begin
      for I in 1 .. Nb loop
         Index :=
            CORBA.Get_Aggregate_Element (Item,
                                         CORBA.Repository_Root.Helper.TC_OperationDescription,
                                         CORBA.Unsigned_Long (I));
         Result (I) := CORBA.Repository_Root.Helper.From_Any (Index);
      end loop;
      return To_Sequence (Result);
   end From_Any;

   function To_Any (Item : in CORBA.Repository_Root.IDL_SEQUENCE_CORBA_Repository_Root_OperationDescription.Sequence)
      return CORBA.Any is
      use IDL_SEQUENCE_CORBA_Repository_Root_OperationDescription;
      Array_Item : Element_Array := To_Element_Array (Item);
      Result : CORBA.Any :=
         CORBA.Get_Empty_Any_Aggregate (TC_IDL_SEQUENCE_CORBA_Repository_Root_OperationDescription);
   begin
      CORBA.Add_Aggregate_Element
         (Result,
          CORBA.To_Any (CORBA.Unsigned_Long (Length (Item))));
      for I in Array_Item'Range loop
         CORBA.Add_Aggregate_Element (Result,
                                      CORBA.Repository_Root.Helper.To_Any (Array_Item (I)));
      end loop;
      return Result;
   end To_Any;

   function From_Any (Item : in CORBA.Any)
      return CORBA.Repository_Root.OpDescriptionSeq is
      Result : CORBA.Repository_Root.IDL_SEQUENCE_CORBA_Repository_Root_OperationDescription.Sequence:= CORBA.Repository_Root.Helper.From_Any (Item);
   begin
      return CORBA.Repository_Root.OpDescriptionSeq (Result);
   end From_Any;

   function To_Any (Item : in CORBA.Repository_Root.OpDescriptionSeq)
      return CORBA.Any is
      Result : CORBA.Any := CORBA.Repository_Root.Helper.To_Any (CORBA.Repository_Root.IDL_SEQUENCE_CORBA_Repository_Root_OperationDescription.Sequence (Item));
   begin
      CORBA.Set_Type (Result, TC_OpDescriptionSeq);
      return Result;
   end To_Any;

   function From_Any (Item : in CORBA.Any)
      return CORBA.Repository_Root.IDL_SEQUENCE_CORBA_Repository_Root_AttributeDescription.Sequence is
      use IDL_SEQUENCE_CORBA_Repository_Root_AttributeDescription;
      Nb_Any : CORBA.Any :=
         CORBA.Get_Aggregate_Element
           (Item,
            CORBA.TC_Unsigned_Long,
            CORBA.Unsigned_Long (0));
      Nb_Long : CORBA.Unsigned_Long := CORBA.From_Any (Nb_Any);
      Nb : Integer := Integer (nb_Long);
      Index : CORBA.Any;
      Result : Element_Array (1 .. Nb);
   begin
      for I in 1 .. Nb loop
         Index :=
            CORBA.Get_Aggregate_Element (Item,
                                         CORBA.Repository_Root.Helper.TC_AttributeDescription,
                                         CORBA.Unsigned_Long (I));
         Result (I) := CORBA.Repository_Root.Helper.From_Any (Index);
      end loop;
      return To_Sequence (Result);
   end From_Any;

   function To_Any (Item : in CORBA.Repository_Root.IDL_SEQUENCE_CORBA_Repository_Root_AttributeDescription.Sequence)
      return CORBA.Any is
      use IDL_SEQUENCE_CORBA_Repository_Root_AttributeDescription;
      Array_Item : Element_Array := To_Element_Array (Item);
      Result : CORBA.Any :=
         CORBA.Get_Empty_Any_Aggregate (TC_IDL_SEQUENCE_CORBA_Repository_Root_AttributeDescription);
   begin
      CORBA.Add_Aggregate_Element
         (Result,
          CORBA.To_Any (CORBA.Unsigned_Long (Length (Item))));
      for I in Array_Item'Range loop
         CORBA.Add_Aggregate_Element (Result,
                                      CORBA.Repository_Root.Helper.To_Any (Array_Item (I)));
      end loop;
      return Result;
   end To_Any;

   function From_Any (Item : in CORBA.Any)
      return CORBA.Repository_Root.AttrDescriptionSeq is
      Result : CORBA.Repository_Root.IDL_SEQUENCE_CORBA_Repository_Root_AttributeDescription.Sequence:= CORBA.Repository_Root.Helper.From_Any (Item);
   begin
      return CORBA.Repository_Root.AttrDescriptionSeq (Result);
   end From_Any;

   function To_Any (Item : in CORBA.Repository_Root.AttrDescriptionSeq)
      return CORBA.Any is
      Result : CORBA.Any := CORBA.Repository_Root.Helper.To_Any (CORBA.Repository_Root.IDL_SEQUENCE_CORBA_Repository_Root_AttributeDescription.Sequence (Item));
   begin
      CORBA.Set_Type (Result, TC_AttrDescriptionSeq);
      return Result;
   end To_Any;

   function From_Any (Item : in CORBA.Any)
      return CORBA.Repository_Root.InterfaceDescription is
      Index : CORBA.Any;
      Result_name : CORBA.Identifier;
      Result_id : CORBA.RepositoryId;
      Result_defined_in : CORBA.RepositoryId;
      Result_version : CORBA.Repository_Root.VersionSpec;
      Result_base_interfaces : CORBA.Repository_Root.RepositoryIdSeq;
      Result_is_abstract : CORBA.Boolean;
   begin
      Index := CORBA.Get_Aggregate_Element (Item,
                                            CORBA.Helper.TC_Identifier,
                                            CORBA.Unsigned_Long ( 0));
      Result_name := CORBA.Helper.From_Any (Index);
      Index := CORBA.Get_Aggregate_Element (Item,
                                            CORBA.Helper.TC_RepositoryId,
                                            CORBA.Unsigned_Long ( 1));
      Result_id := CORBA.Helper.From_Any (Index);
      Index := CORBA.Get_Aggregate_Element (Item,
                                            CORBA.Helper.TC_RepositoryId,
                                            CORBA.Unsigned_Long ( 2));
      Result_defined_in := CORBA.Helper.From_Any (Index);
      Index := CORBA.Get_Aggregate_Element (Item,
                                            CORBA.Repository_Root.Helper.TC_VersionSpec,
                                            CORBA.Unsigned_Long ( 3));
      Result_version := CORBA.Repository_Root.Helper.From_Any (Index);
      Index := CORBA.Get_Aggregate_Element (Item,
                                            CORBA.Repository_Root.Helper.TC_RepositoryIdSeq,
                                            CORBA.Unsigned_Long ( 4));
      Result_base_interfaces := CORBA.Repository_Root.Helper.From_Any (Index);
      Index := CORBA.Get_Aggregate_Element (Item,
                                            CORBA.TC_Boolean,
                                            CORBA.Unsigned_Long ( 5));
      Result_is_abstract := CORBA.From_Any (Index);
      return
         (name => Result_name,
          id => Result_id,
          defined_in => Result_defined_in,
          version => Result_version,
          base_interfaces => Result_base_interfaces,
          is_abstract => Result_is_abstract);
   end From_Any;

   function To_Any (Item : in CORBA.Repository_Root.InterfaceDescription)
      return CORBA.Any is
      Result : CORBA.Any :=
         CORBA.Get_Empty_Any_Aggregate (TC_InterfaceDescription);
   begin
      CORBA.Add_Aggregate_Element
         (Result, CORBA.Helper.To_Any (Item.name));
      CORBA.Add_Aggregate_Element
         (Result, CORBA.Helper.To_Any (Item.id));
      CORBA.Add_Aggregate_Element
         (Result, CORBA.Helper.To_Any (Item.defined_in));
      CORBA.Add_Aggregate_Element
         (Result, CORBA.Repository_Root.Helper.To_Any (Item.version));
      CORBA.Add_Aggregate_Element
         (Result, CORBA.Repository_Root.Helper.To_Any (Item.base_interfaces));
      CORBA.Add_Aggregate_Element
         (Result, CORBA.To_Any (Item.is_abstract));
      return Result;
   end To_Any;

   function From_Any (Item : in CORBA.Any)
      return CORBA.Repository_Root.IDL_SEQUENCE_CORBA_Repository_Root_ValueMember.Sequence is
      use IDL_SEQUENCE_CORBA_Repository_Root_ValueMember;
      Nb_Any : CORBA.Any :=
         CORBA.Get_Aggregate_Element
           (Item,
            CORBA.TC_Unsigned_Long,
            CORBA.Unsigned_Long (0));
      Nb_Long : CORBA.Unsigned_Long := CORBA.From_Any (Nb_Any);
      Nb : Integer := Integer (nb_Long);
      Index : CORBA.Any;
      Result : Element_Array (1 .. Nb);
   begin
      for I in 1 .. Nb loop
         Index :=
            CORBA.Get_Aggregate_Element (Item,
                                         CORBA.Repository_Root.Helper.TC_ValueMember,
                                         CORBA.Unsigned_Long (I));
         Result (I) := CORBA.Repository_Root.Helper.From_Any (Index);
      end loop;
      return To_Sequence (Result);
   end From_Any;

   function To_Any (Item : in CORBA.Repository_Root.IDL_SEQUENCE_CORBA_Repository_Root_ValueMember.Sequence)
      return CORBA.Any is
      use IDL_SEQUENCE_CORBA_Repository_Root_ValueMember;
      Array_Item : Element_Array := To_Element_Array (Item);
      Result : CORBA.Any :=
         CORBA.Get_Empty_Any_Aggregate (TC_IDL_SEQUENCE_CORBA_Repository_Root_ValueMember);
   begin
      CORBA.Add_Aggregate_Element
         (Result,
          CORBA.To_Any (CORBA.Unsigned_Long (Length (Item))));
      for I in Array_Item'Range loop
         CORBA.Add_Aggregate_Element (Result,
                                      CORBA.Repository_Root.Helper.To_Any (Array_Item (I)));
      end loop;
      return Result;
   end To_Any;

   function From_Any (Item : in CORBA.Any)
      return CORBA.Repository_Root.ValueMemberSeq is
      Result : CORBA.Repository_Root.IDL_SEQUENCE_CORBA_Repository_Root_ValueMember.Sequence:= CORBA.Repository_Root.Helper.From_Any (Item);
   begin
      return CORBA.Repository_Root.ValueMemberSeq (Result);
   end From_Any;

   function To_Any (Item : in CORBA.Repository_Root.ValueMemberSeq)
      return CORBA.Any is
      Result : CORBA.Any := CORBA.Repository_Root.Helper.To_Any (CORBA.Repository_Root.IDL_SEQUENCE_CORBA_Repository_Root_ValueMember.Sequence (Item));
   begin
      CORBA.Set_Type (Result, TC_ValueMemberSeq);
      return Result;
   end To_Any;

   function From_Any (Item : in CORBA.Any)
      return CORBA.Repository_Root.ValueDescription is
      Index : CORBA.Any;
      Result_name : CORBA.Identifier;
      Result_id : CORBA.RepositoryId;
      Result_is_abstract : CORBA.Boolean;
      Result_is_custom : CORBA.Boolean;
      Result_defined_in : CORBA.RepositoryId;
      Result_version : CORBA.Repository_Root.VersionSpec;
      Result_supported_interfaces : CORBA.Repository_Root.RepositoryIdSeq;
      Result_abstract_base_values : CORBA.Repository_Root.RepositoryIdSeq;
      Result_is_truncatable : CORBA.Boolean;
      Result_base_value : CORBA.RepositoryId;
   begin
      Index := CORBA.Get_Aggregate_Element (Item,
                                            CORBA.Helper.TC_Identifier,
                                            CORBA.Unsigned_Long ( 0));
      Result_name := CORBA.Helper.From_Any (Index);
      Index := CORBA.Get_Aggregate_Element (Item,
                                            CORBA.Helper.TC_RepositoryId,
                                            CORBA.Unsigned_Long ( 1));
      Result_id := CORBA.Helper.From_Any (Index);
      Index := CORBA.Get_Aggregate_Element (Item,
                                            CORBA.TC_Boolean,
                                            CORBA.Unsigned_Long ( 2));
      Result_is_abstract := CORBA.From_Any (Index);
      Index := CORBA.Get_Aggregate_Element (Item,
                                            CORBA.TC_Boolean,
                                            CORBA.Unsigned_Long ( 3));
      Result_is_custom := CORBA.From_Any (Index);
      Index := CORBA.Get_Aggregate_Element (Item,
                                            CORBA.Helper.TC_RepositoryId,
                                            CORBA.Unsigned_Long ( 4));
      Result_defined_in := CORBA.Helper.From_Any (Index);
      Index := CORBA.Get_Aggregate_Element (Item,
                                            CORBA.Repository_Root.Helper.TC_VersionSpec,
                                            CORBA.Unsigned_Long ( 5));
      Result_version := CORBA.Repository_Root.Helper.From_Any (Index);
      Index := CORBA.Get_Aggregate_Element (Item,
                                            CORBA.Repository_Root.Helper.TC_RepositoryIdSeq,
                                            CORBA.Unsigned_Long ( 6));
      Result_supported_interfaces := CORBA.Repository_Root.Helper.From_Any (Index);
      Index := CORBA.Get_Aggregate_Element (Item,
                                            CORBA.Repository_Root.Helper.TC_RepositoryIdSeq,
                                            CORBA.Unsigned_Long ( 7));
      Result_abstract_base_values := CORBA.Repository_Root.Helper.From_Any (Index);
      Index := CORBA.Get_Aggregate_Element (Item,
                                            CORBA.TC_Boolean,
                                            CORBA.Unsigned_Long ( 8));
      Result_is_truncatable := CORBA.From_Any (Index);
      Index := CORBA.Get_Aggregate_Element (Item,
                                            CORBA.Helper.TC_RepositoryId,
                                            CORBA.Unsigned_Long ( 9));
      Result_base_value := CORBA.Helper.From_Any (Index);
      return
         (name => Result_name,
          id => Result_id,
          is_abstract => Result_is_abstract,
          is_custom => Result_is_custom,
          defined_in => Result_defined_in,
          version => Result_version,
          supported_interfaces => Result_supported_interfaces,
          abstract_base_values => Result_abstract_base_values,
          is_truncatable => Result_is_truncatable,
          base_value => Result_base_value);
   end From_Any;

   function To_Any (Item : in CORBA.Repository_Root.ValueDescription)
      return CORBA.Any is
      Result : CORBA.Any :=
         CORBA.Get_Empty_Any_Aggregate (TC_ValueDescription);
   begin
      CORBA.Add_Aggregate_Element
         (Result, CORBA.Helper.To_Any (Item.name));
      CORBA.Add_Aggregate_Element
         (Result, CORBA.Helper.To_Any (Item.id));
      CORBA.Add_Aggregate_Element
         (Result, CORBA.To_Any (Item.is_abstract));
      CORBA.Add_Aggregate_Element
         (Result, CORBA.To_Any (Item.is_custom));
      CORBA.Add_Aggregate_Element
         (Result, CORBA.Helper.To_Any (Item.defined_in));
      CORBA.Add_Aggregate_Element
         (Result, CORBA.Repository_Root.Helper.To_Any (Item.version));
      CORBA.Add_Aggregate_Element
         (Result, CORBA.Repository_Root.Helper.To_Any (Item.supported_interfaces));
      CORBA.Add_Aggregate_Element
         (Result, CORBA.Repository_Root.Helper.To_Any (Item.abstract_base_values));
      CORBA.Add_Aggregate_Element
         (Result, CORBA.To_Any (Item.is_truncatable));
      CORBA.Add_Aggregate_Element
         (Result, CORBA.Helper.To_Any (Item.base_value));
      return Result;
   end To_Any;

begin
   declare
      Name : CORBA.String := CORBA.To_CORBA_String ("DefinitionKind");
      Id : CORBA.String := CORBA.To_CORBA_String ("IDL:omg.org/CORBA/Repository_Root/DefinitionKind:2.3");
      dk_none_Name : CORBA.String := CORBA.To_CORBA_String ("dk_none");
      dk_all_Name : CORBA.String := CORBA.To_CORBA_String ("dk_all");
      dk_Attribute_Name : CORBA.String := CORBA.To_CORBA_String ("dk_Attribute");
      dk_Constant_Name : CORBA.String := CORBA.To_CORBA_String ("dk_Constant");
      dk_Exception_Name : CORBA.String := CORBA.To_CORBA_String ("dk_Exception");
      dk_Interface_Name : CORBA.String := CORBA.To_CORBA_String ("dk_Interface");
      dk_Module_Name : CORBA.String := CORBA.To_CORBA_String ("dk_Module");
      dk_Operation_Name : CORBA.String := CORBA.To_CORBA_String ("dk_Operation");
      dk_Typedef_Name : CORBA.String := CORBA.To_CORBA_String ("dk_Typedef");
      dk_Alias_Name : CORBA.String := CORBA.To_CORBA_String ("dk_Alias");
      dk_Struct_Name : CORBA.String := CORBA.To_CORBA_String ("dk_Struct");
      dk_Union_Name : CORBA.String := CORBA.To_CORBA_String ("dk_Union");
      dk_Enum_Name : CORBA.String := CORBA.To_CORBA_String ("dk_Enum");
      dk_Primitive_Name : CORBA.String := CORBA.To_CORBA_String ("dk_Primitive");
      dk_String_Name : CORBA.String := CORBA.To_CORBA_String ("dk_String");
      dk_Sequence_Name : CORBA.String := CORBA.To_CORBA_String ("dk_Sequence");
      dk_Array_Name : CORBA.String := CORBA.To_CORBA_String ("dk_Array");
      dk_Repository_Name : CORBA.String := CORBA.To_CORBA_String ("dk_Repository");
      dk_Wstring_Name : CORBA.String := CORBA.To_CORBA_String ("dk_Wstring");
      dk_Fixed_Name : CORBA.String := CORBA.To_CORBA_String ("dk_Fixed");
      dk_Value_Name : CORBA.String := CORBA.To_CORBA_String ("dk_Value");
      dk_ValueBox_Name : CORBA.String := CORBA.To_CORBA_String ("dk_ValueBox");
      dk_ValueMember_Name : CORBA.String := CORBA.To_CORBA_String ("dk_ValueMember");
      dk_Native_Name : CORBA.String := CORBA.To_CORBA_String ("dk_Native");
   begin
      CORBA.TypeCode.Add_Parameter (TC_DefinitionKind, CORBA.To_Any (Name));
      CORBA.TypeCode.Add_Parameter (TC_DefinitionKind, CORBA.To_Any (Id));
      CORBA.TypeCode.Add_Parameter (TC_DefinitionKind, CORBA.To_Any (dk_none_Name));
      CORBA.TypeCode.Add_Parameter (TC_DefinitionKind, CORBA.To_Any (dk_all_Name));
      CORBA.TypeCode.Add_Parameter (TC_DefinitionKind, CORBA.To_Any (dk_Attribute_Name));
      CORBA.TypeCode.Add_Parameter (TC_DefinitionKind, CORBA.To_Any (dk_Constant_Name));
      CORBA.TypeCode.Add_Parameter (TC_DefinitionKind, CORBA.To_Any (dk_Exception_Name));
      CORBA.TypeCode.Add_Parameter (TC_DefinitionKind, CORBA.To_Any (dk_Interface_Name));
      CORBA.TypeCode.Add_Parameter (TC_DefinitionKind, CORBA.To_Any (dk_Module_Name));
      CORBA.TypeCode.Add_Parameter (TC_DefinitionKind, CORBA.To_Any (dk_Operation_Name));
      CORBA.TypeCode.Add_Parameter (TC_DefinitionKind, CORBA.To_Any (dk_Typedef_Name));
      CORBA.TypeCode.Add_Parameter (TC_DefinitionKind, CORBA.To_Any (dk_Alias_Name));
      CORBA.TypeCode.Add_Parameter (TC_DefinitionKind, CORBA.To_Any (dk_Struct_Name));
      CORBA.TypeCode.Add_Parameter (TC_DefinitionKind, CORBA.To_Any (dk_Union_Name));
      CORBA.TypeCode.Add_Parameter (TC_DefinitionKind, CORBA.To_Any (dk_Enum_Name));
      CORBA.TypeCode.Add_Parameter (TC_DefinitionKind, CORBA.To_Any (dk_Primitive_Name));
      CORBA.TypeCode.Add_Parameter (TC_DefinitionKind, CORBA.To_Any (dk_String_Name));
      CORBA.TypeCode.Add_Parameter (TC_DefinitionKind, CORBA.To_Any (dk_Sequence_Name));
      CORBA.TypeCode.Add_Parameter (TC_DefinitionKind, CORBA.To_Any (dk_Array_Name));
      CORBA.TypeCode.Add_Parameter (TC_DefinitionKind, CORBA.To_Any (dk_Repository_Name));
      CORBA.TypeCode.Add_Parameter (TC_DefinitionKind, CORBA.To_Any (dk_Wstring_Name));
      CORBA.TypeCode.Add_Parameter (TC_DefinitionKind, CORBA.To_Any (dk_Fixed_Name));
      CORBA.TypeCode.Add_Parameter (TC_DefinitionKind, CORBA.To_Any (dk_Value_Name));
      CORBA.TypeCode.Add_Parameter (TC_DefinitionKind, CORBA.To_Any (dk_ValueBox_Name));
      CORBA.TypeCode.Add_Parameter (TC_DefinitionKind, CORBA.To_Any (dk_ValueMember_Name));
      CORBA.TypeCode.Add_Parameter (TC_DefinitionKind, CORBA.To_Any (dk_Native_Name));
   end;

   declare
      Name : CORBA.String := CORBA.To_CORBA_String ("VersionSpec");
      Id : CORBA.String := CORBA.To_CORBA_String ("IDL:omg.org/CORBA/Repository_Root/VersionSpec:1.0");
   begin
      CORBA.TypeCode.Add_Parameter (TC_VersionSpec, CORBA.To_Any (Name));
      CORBA.TypeCode.Add_Parameter (TC_VersionSpec, CORBA.To_Any (Id));
      CORBA.TypeCode.Add_Parameter (TC_VersionSpec, CORBA.To_Any (CORBA.TC_String));
   end;

--   CORBA.TypeCode.Add_Parameter (TC_IDL_SEQUENCE_CORBA_Repository_Root_Contained_Forward, CORBA.To_Any (CORBA.Unsigned_Long (0)));
--   CORBA.TypeCode.Add_Parameter (TC_IDL_SEQUENCE_CORBA_Repository_Root_Contained_Forward, CORBA.To_Any (CORBA.Repository_Root.Contained.Helper.TC_Contained));
--
--   declare
--      Name : CORBA.String := CORBA.To_CORBA_String ("ContainedSeq");
--      Id : CORBA.String := CORBA.To_CORBA_String ("IDL:omg.org/CORBA/Repository_Root/ContainedSeq:1.0");
--   begin
--      CORBA.TypeCode.Add_Parameter (TC_ContainedSeq, CORBA.To_Any (Name));
--      CORBA.TypeCode.Add_Parameter (TC_ContainedSeq, CORBA.To_Any (Id));
--      CORBA.TypeCode.Add_Parameter (TC_ContainedSeq, CORBA.To_Any (CORBA.Repository_Root.Helper.TC_IDL_SEQUENCE_CORBA_Repository_Root_Contained_Forward));
--   end;

--   CORBA.TypeCode.Add_Parameter (TC_IDL_SEQUENCE_CORBA_Repository_Root_InterfaceDef_Forward, CORBA.To_Any (CORBA.Unsigned_Long (0)));
--   CORBA.TypeCode.Add_Parameter (TC_IDL_SEQUENCE_CORBA_Repository_Root_InterfaceDef_Forward, CORBA.To_Any (CORBA.Repository_Root.InterfaceDef.Helper.TC_InterfaceDef));
--
--   declare
--      Name : CORBA.String := CORBA.To_CORBA_String ("InterfaceDefSeq");
--      Id : CORBA.String := CORBA.To_CORBA_String ("IDL:omg.org/CORBA/Repository_Root/InterfaceDefSeq:1.0");
--   begin
--      CORBA.TypeCode.Add_Parameter (TC_InterfaceDefSeq, CORBA.To_Any (Name));
--      CORBA.TypeCode.Add_Parameter (TC_InterfaceDefSeq, CORBA.To_Any (Id));
--      CORBA.TypeCode.Add_Parameter (TC_InterfaceDefSeq, CORBA.To_Any (CORBA.Repository_Root.Helper.TC_IDL_SEQUENCE_CORBA_Repository_Root_InterfaceDef_Forward));
--   end;

--   CORBA.TypeCode.Add_Parameter (TC_IDL_SEQUENCE_CORBA_Repository_Root_ValueDef_Forward, CORBA.To_Any (CORBA.Unsigned_Long (0)));
--   CORBA.TypeCode.Add_Parameter (TC_IDL_SEQUENCE_CORBA_Repository_Root_ValueDef_Forward, CORBA.To_Any (CORBA.Repository_Root.ValueDef.Helper.TC_ValueDef));

--   declare
--      Name : CORBA.String := CORBA.To_CORBA_String ("ValueDefSeq");
--      Id : CORBA.String := CORBA.To_CORBA_String ("IDL:omg.org/CORBA/Repository_Root/ValueDefSeq:1.0");
--   begin
--      CORBA.TypeCode.Add_Parameter (TC_ValueDefSeq, CORBA.To_Any (Name));
--      CORBA.TypeCode.Add_Parameter (TC_ValueDefSeq, CORBA.To_Any (Id));
--      CORBA.TypeCode.Add_Parameter (TC_ValueDefSeq, CORBA.To_Any (CORBA.Repository_Root.Helper.TC_IDL_SEQUENCE_CORBA_Repository_Root_ValueDef_Forward));
--   end;
   declare
      Name : CORBA.String := CORBA.To_CORBA_String ("OperationMode");
      Id : CORBA.String := CORBA.To_CORBA_String ("IDL:omg.org/CORBA/Repository_Root/OperationMode:1.0");
      OP_NORMAL_Name : CORBA.String := CORBA.To_CORBA_String ("OP_NORMAL");
      OP_ONEWAY_Name : CORBA.String := CORBA.To_CORBA_String ("OP_ONEWAY");
   begin
      CORBA.TypeCode.Add_Parameter (TC_OperationMode, CORBA.To_Any (Name));
      CORBA.TypeCode.Add_Parameter (TC_OperationMode, CORBA.To_Any (Id));
      CORBA.TypeCode.Add_Parameter (TC_OperationMode, CORBA.To_Any (OP_NORMAL_Name));
      CORBA.TypeCode.Add_Parameter (TC_OperationMode, CORBA.To_Any (OP_ONEWAY_Name));
   end;
   declare
      Name : CORBA.String := CORBA.To_CORBA_String ("ParameterMode");
      Id : CORBA.String := CORBA.To_CORBA_String ("IDL:omg.org/CORBA/Repository_Root/ParameterMode:1.0");
      PARAM_IN_Name : CORBA.String := CORBA.To_CORBA_String ("PARAM_IN");
      PARAM_OUT_Name : CORBA.String := CORBA.To_CORBA_String ("PARAM_OUT");
      PARAM_INOUT_Name : CORBA.String := CORBA.To_CORBA_String ("PARAM_INOUT");
   begin
      CORBA.TypeCode.Add_Parameter (TC_ParameterMode, CORBA.To_Any (Name));
      CORBA.TypeCode.Add_Parameter (TC_ParameterMode, CORBA.To_Any (Id));
      CORBA.TypeCode.Add_Parameter (TC_ParameterMode, CORBA.To_Any (PARAM_IN_Name));
      CORBA.TypeCode.Add_Parameter (TC_ParameterMode, CORBA.To_Any (PARAM_OUT_Name));
      CORBA.TypeCode.Add_Parameter (TC_ParameterMode, CORBA.To_Any (PARAM_INOUT_Name));
   end;

   declare
      Name : CORBA.String := CORBA.To_CORBA_String ("ParameterDescription");
      Id : CORBA.String := CORBA.To_CORBA_String ("IDL:omg.org/CORBA/Repository_Root/ParameterDescription:1.0");
      Arg_Name_name : CORBA.String := CORBA.To_CORBA_String ("name");
      Arg_Name_IDL_type : CORBA.String := CORBA.To_CORBA_String ("IDL_type");
      Arg_Name_type_def : CORBA.String := CORBA.To_CORBA_String ("type_def");
      Arg_Name_mode : CORBA.String := CORBA.To_CORBA_String ("mode");
   begin
      CORBA.TypeCode.Add_Parameter (TC_ParameterDescription, CORBA.To_Any (Name));
      CORBA.TypeCode.Add_Parameter (TC_ParameterDescription, CORBA.To_Any (Id));
      CORBA.TypeCode.Add_Parameter (TC_ParameterDescription, CORBA.To_Any (CORBA.Helper.TC_Identifier));
      CORBA.TypeCode.Add_Parameter (TC_ParameterDescription, CORBA.To_Any (Arg_Name_name));
      CORBA.TypeCode.Add_Parameter (TC_ParameterDescription, CORBA.To_Any (CORBA.TC_TypeCode));
      CORBA.TypeCode.Add_Parameter (TC_ParameterDescription, CORBA.To_Any (Arg_Name_IDL_type));
      CORBA.TypeCode.Add_Parameter (TC_ParameterDescription, CORBA.To_Any (CORBA.Repository_Root.IDLType.Helper.TC_IDLType));
      CORBA.TypeCode.Add_Parameter (TC_ParameterDescription, CORBA.To_Any (Arg_Name_type_def));
      CORBA.TypeCode.Add_Parameter (TC_ParameterDescription, CORBA.To_Any (CORBA.Repository_Root.Helper.TC_ParameterMode));
      CORBA.TypeCode.Add_Parameter (TC_ParameterDescription, CORBA.To_Any (Arg_Name_mode));
   end;

   declare
      Name : CORBA.String := CORBA.To_CORBA_String ("Visibility");
      Id : CORBA.String := CORBA.To_CORBA_String ("IDL:omg.org/CORBA/Repository_Root/Visibility:1.0");
   begin
      CORBA.TypeCode.Add_Parameter (TC_Visibility, CORBA.To_Any (Name));
      CORBA.TypeCode.Add_Parameter (TC_Visibility, CORBA.To_Any (Id));
      CORBA.TypeCode.Add_Parameter (TC_Visibility, CORBA.To_Any (CORBA.TC_Short));
   end;

   declare
      Name : CORBA.String := CORBA.To_CORBA_String ("ValueMember");
      Id : CORBA.String := CORBA.To_CORBA_String ("IDL:omg.org/CORBA/Repository_Root/ValueMember:2.3");
      Arg_Name_name : CORBA.String := CORBA.To_CORBA_String ("name");
      Arg_Name_id : CORBA.String := CORBA.To_CORBA_String ("id");
      Arg_Name_defined_in : CORBA.String := CORBA.To_CORBA_String ("defined_in");
      Arg_Name_version : CORBA.String := CORBA.To_CORBA_String ("version");
      Arg_Name_IDL_type : CORBA.String := CORBA.To_CORBA_String ("IDL_type");
      Arg_Name_type_def : CORBA.String := CORBA.To_CORBA_String ("type_def");
      Arg_Name_IDL_access : CORBA.String := CORBA.To_CORBA_String ("IDL_access");
   begin
      CORBA.TypeCode.Add_Parameter (TC_ValueMember, CORBA.To_Any (Name));
      CORBA.TypeCode.Add_Parameter (TC_ValueMember, CORBA.To_Any (Id));
      CORBA.TypeCode.Add_Parameter (TC_ValueMember, CORBA.To_Any (CORBA.Helper.TC_Identifier));
      CORBA.TypeCode.Add_Parameter (TC_ValueMember, CORBA.To_Any (Arg_Name_name));
      CORBA.TypeCode.Add_Parameter (TC_ValueMember, CORBA.To_Any (CORBA.Helper.TC_RepositoryId));
      CORBA.TypeCode.Add_Parameter (TC_ValueMember, CORBA.To_Any (Arg_Name_id));
      CORBA.TypeCode.Add_Parameter (TC_ValueMember, CORBA.To_Any (CORBA.Helper.TC_RepositoryId));
      CORBA.TypeCode.Add_Parameter (TC_ValueMember, CORBA.To_Any (Arg_Name_defined_in));
      CORBA.TypeCode.Add_Parameter (TC_ValueMember, CORBA.To_Any (CORBA.Repository_Root.Helper.TC_VersionSpec));
      CORBA.TypeCode.Add_Parameter (TC_ValueMember, CORBA.To_Any (Arg_Name_version));
      CORBA.TypeCode.Add_Parameter (TC_ValueMember, CORBA.To_Any (CORBA.TC_TypeCode));
      CORBA.TypeCode.Add_Parameter (TC_ValueMember, CORBA.To_Any (Arg_Name_IDL_type));
      CORBA.TypeCode.Add_Parameter (TC_ValueMember, CORBA.To_Any (CORBA.Repository_Root.IDLType.Helper.TC_IDLType));
      CORBA.TypeCode.Add_Parameter (TC_ValueMember, CORBA.To_Any (Arg_Name_type_def));
      CORBA.TypeCode.Add_Parameter (TC_ValueMember, CORBA.To_Any (CORBA.Repository_Root.Helper.TC_Visibility));
      CORBA.TypeCode.Add_Parameter (TC_ValueMember, CORBA.To_Any (Arg_Name_IDL_access));
   end;

   CORBA.TypeCode.Add_Parameter (TC_IDL_SEQUENCE_CORBA_Repository_Root_ExceptionDef_Forward, CORBA.To_Any (CORBA.Unsigned_Long (0)));
   CORBA.TypeCode.Add_Parameter (TC_IDL_SEQUENCE_CORBA_Repository_Root_ExceptionDef_Forward, CORBA.To_Any (CORBA.Repository_Root.ExceptionDef.Helper.TC_ExceptionDef));

   declare
      Name : CORBA.String := CORBA.To_CORBA_String ("ExceptionDefSeq");
      Id : CORBA.String := CORBA.To_CORBA_String ("IDL:omg.org/CORBA/Repository_Root/ExceptionDefSeq:1.0");
   begin
      CORBA.TypeCode.Add_Parameter (TC_ExceptionDefSeq, CORBA.To_Any (Name));
      CORBA.TypeCode.Add_Parameter (TC_ExceptionDefSeq, CORBA.To_Any (Id));
      CORBA.TypeCode.Add_Parameter (TC_ExceptionDefSeq, CORBA.To_Any (CORBA.Repository_Root.Helper.TC_IDL_SEQUENCE_CORBA_Repository_Root_ExceptionDef_Forward));
   end;

   declare
      Name : CORBA.String := CORBA.To_CORBA_String ("StructMember");
      Id : CORBA.String := CORBA.To_CORBA_String ("IDL:omg.org/CORBA/Repository_Root/StructMember:1.0");
      Arg_Name_name : CORBA.String := CORBA.To_CORBA_String ("name");
      Arg_Name_IDL_type : CORBA.String := CORBA.To_CORBA_String ("IDL_type");
      Arg_Name_type_def : CORBA.String := CORBA.To_CORBA_String ("type_def");
   begin
      CORBA.TypeCode.Add_Parameter (TC_StructMember, CORBA.To_Any (Name));
      CORBA.TypeCode.Add_Parameter (TC_StructMember, CORBA.To_Any (Id));
      CORBA.TypeCode.Add_Parameter (TC_StructMember, CORBA.To_Any (CORBA.Helper.TC_Identifier));
      CORBA.TypeCode.Add_Parameter (TC_StructMember, CORBA.To_Any (Arg_Name_name));
      CORBA.TypeCode.Add_Parameter (TC_StructMember, CORBA.To_Any (CORBA.TC_TypeCode));
      CORBA.TypeCode.Add_Parameter (TC_StructMember, CORBA.To_Any (Arg_Name_IDL_type));
      CORBA.TypeCode.Add_Parameter (TC_StructMember, CORBA.To_Any (CORBA.Repository_Root.IDLType.Helper.TC_IDLType));
      CORBA.TypeCode.Add_Parameter (TC_StructMember, CORBA.To_Any (Arg_Name_type_def));
   end;

   CORBA.TypeCode.Add_Parameter (TC_IDL_SEQUENCE_CORBA_Repository_Root_StructMember, CORBA.To_Any (CORBA.Unsigned_Long (0)));
   CORBA.TypeCode.Add_Parameter (TC_IDL_SEQUENCE_CORBA_Repository_Root_StructMember, CORBA.To_Any (CORBA.Repository_Root.Helper.TC_StructMember));

   declare
      Name : CORBA.String := CORBA.To_CORBA_String ("StructMemberSeq");
      Id : CORBA.String := CORBA.To_CORBA_String ("IDL:omg.org/CORBA/Repository_Root/StructMemberSeq:1.0");
   begin
      CORBA.TypeCode.Add_Parameter (TC_StructMemberSeq, CORBA.To_Any (Name));
      CORBA.TypeCode.Add_Parameter (TC_StructMemberSeq, CORBA.To_Any (Id));
      CORBA.TypeCode.Add_Parameter (TC_StructMemberSeq, CORBA.To_Any (CORBA.Repository_Root.Helper.TC_IDL_SEQUENCE_CORBA_Repository_Root_StructMember));
   end;

   declare
      Name : CORBA.String := CORBA.To_CORBA_String ("Initializer");
      Id : CORBA.String := CORBA.To_CORBA_String ("IDL:omg.org/CORBA/Repository_Root/Initializer:2.3");
      Arg_Name_members : CORBA.String := CORBA.To_CORBA_String ("members");
      Arg_Name_name : CORBA.String := CORBA.To_CORBA_String ("name");
   begin
      CORBA.TypeCode.Add_Parameter (TC_Initializer, CORBA.To_Any (Name));
      CORBA.TypeCode.Add_Parameter (TC_Initializer, CORBA.To_Any (Id));
      CORBA.TypeCode.Add_Parameter (TC_Initializer, CORBA.To_Any (CORBA.Repository_Root.Helper.TC_StructMemberSeq));
      CORBA.TypeCode.Add_Parameter (TC_Initializer, CORBA.To_Any (Arg_Name_members));
      CORBA.TypeCode.Add_Parameter (TC_Initializer, CORBA.To_Any (CORBA.Helper.TC_Identifier));
      CORBA.TypeCode.Add_Parameter (TC_Initializer, CORBA.To_Any (Arg_Name_name));
   end;

   CORBA.TypeCode.Add_Parameter (TC_IDL_SEQUENCE_CORBA_Repository_Root_Initializer, CORBA.To_Any (CORBA.Unsigned_Long (0)));
   CORBA.TypeCode.Add_Parameter (TC_IDL_SEQUENCE_CORBA_Repository_Root_Initializer, CORBA.To_Any (CORBA.Repository_Root.Helper.TC_Initializer));

   declare
      Name : CORBA.String := CORBA.To_CORBA_String ("InitializerSeq");
      Id : CORBA.String := CORBA.To_CORBA_String ("IDL:omg.org/CORBA/Repository_Root/InitializerSeq:1.0");
   begin
      CORBA.TypeCode.Add_Parameter (TC_InitializerSeq, CORBA.To_Any (Name));
      CORBA.TypeCode.Add_Parameter (TC_InitializerSeq, CORBA.To_Any (Id));
      CORBA.TypeCode.Add_Parameter (TC_InitializerSeq, CORBA.To_Any (CORBA.Repository_Root.Helper.TC_IDL_SEQUENCE_CORBA_Repository_Root_Initializer));
   end;

   declare
      Name : CORBA.String := CORBA.To_CORBA_String ("UnionMember");
      Id : CORBA.String := CORBA.To_CORBA_String ("IDL:omg.org/CORBA/Repository_Root/UnionMember:1.0");
      Arg_Name_name : CORBA.String := CORBA.To_CORBA_String ("name");
      Arg_Name_label : CORBA.String := CORBA.To_CORBA_String ("label");
      Arg_Name_IDL_type : CORBA.String := CORBA.To_CORBA_String ("IDL_type");
      Arg_Name_type_def : CORBA.String := CORBA.To_CORBA_String ("type_def");
   begin
      CORBA.TypeCode.Add_Parameter (TC_UnionMember, CORBA.To_Any (Name));
      CORBA.TypeCode.Add_Parameter (TC_UnionMember, CORBA.To_Any (Id));
      CORBA.TypeCode.Add_Parameter (TC_UnionMember, CORBA.To_Any (CORBA.Helper.TC_Identifier));
      CORBA.TypeCode.Add_Parameter (TC_UnionMember, CORBA.To_Any (Arg_Name_name));
      CORBA.TypeCode.Add_Parameter (TC_UnionMember, CORBA.To_Any (CORBA.TC_Any));
      CORBA.TypeCode.Add_Parameter (TC_UnionMember, CORBA.To_Any (Arg_Name_label));
      CORBA.TypeCode.Add_Parameter (TC_UnionMember, CORBA.To_Any (CORBA.TC_TypeCode));
      CORBA.TypeCode.Add_Parameter (TC_UnionMember, CORBA.To_Any (Arg_Name_IDL_type));
      CORBA.TypeCode.Add_Parameter (TC_UnionMember, CORBA.To_Any (CORBA.Repository_Root.IDLType.Helper.TC_IDLType));
      CORBA.TypeCode.Add_Parameter (TC_UnionMember, CORBA.To_Any (Arg_Name_type_def));
   end;

   CORBA.TypeCode.Add_Parameter (TC_IDL_SEQUENCE_CORBA_Repository_Root_UnionMember, CORBA.To_Any (CORBA.Unsigned_Long (0)));
   CORBA.TypeCode.Add_Parameter (TC_IDL_SEQUENCE_CORBA_Repository_Root_UnionMember, CORBA.To_Any (CORBA.Repository_Root.Helper.TC_UnionMember));

   declare
      Name : CORBA.String := CORBA.To_CORBA_String ("UnionMemberSeq");
      Id : CORBA.String := CORBA.To_CORBA_String ("IDL:omg.org/CORBA/Repository_Root/UnionMemberSeq:1.0");
   begin
      CORBA.TypeCode.Add_Parameter (TC_UnionMemberSeq, CORBA.To_Any (Name));
      CORBA.TypeCode.Add_Parameter (TC_UnionMemberSeq, CORBA.To_Any (Id));
      CORBA.TypeCode.Add_Parameter (TC_UnionMemberSeq, CORBA.To_Any (CORBA.Repository_Root.Helper.TC_IDL_SEQUENCE_CORBA_Repository_Root_UnionMember));
   end;

   CORBA.TypeCode.Add_Parameter (TC_IDL_SEQUENCE_CORBA_Identifier, CORBA.To_Any (CORBA.Unsigned_Long (0)));
   CORBA.TypeCode.Add_Parameter (TC_IDL_SEQUENCE_CORBA_Identifier, CORBA.To_Any (CORBA.Helper.TC_Identifier));

   declare
      Name : CORBA.String := CORBA.To_CORBA_String ("EnumMemberSeq");
      Id : CORBA.String := CORBA.To_CORBA_String ("IDL:omg.org/CORBA/Repository_Root/EnumMemberSeq:1.0");
   begin
      CORBA.TypeCode.Add_Parameter (TC_EnumMemberSeq, CORBA.To_Any (Name));
      CORBA.TypeCode.Add_Parameter (TC_EnumMemberSeq, CORBA.To_Any (Id));
      CORBA.TypeCode.Add_Parameter (TC_EnumMemberSeq, CORBA.To_Any (CORBA.Repository_Root.Helper.TC_IDL_SEQUENCE_CORBA_Identifier));
   end;
   declare
      Name : CORBA.String := CORBA.To_CORBA_String ("PrimitiveKind");
      Id : CORBA.String := CORBA.To_CORBA_String ("IDL:omg.org/CORBA/Repository_Root/PrimitiveKind:2.3");
      pk_null_Name : CORBA.String := CORBA.To_CORBA_String ("pk_null");
      pk_void_Name : CORBA.String := CORBA.To_CORBA_String ("pk_void");
      pk_short_Name : CORBA.String := CORBA.To_CORBA_String ("pk_short");
      pk_long_Name : CORBA.String := CORBA.To_CORBA_String ("pk_long");
      pk_ushort_Name : CORBA.String := CORBA.To_CORBA_String ("pk_ushort");
      pk_ulong_Name : CORBA.String := CORBA.To_CORBA_String ("pk_ulong");
      pk_float_Name : CORBA.String := CORBA.To_CORBA_String ("pk_float");
      pk_double_Name : CORBA.String := CORBA.To_CORBA_String ("pk_double");
      pk_boolean_Name : CORBA.String := CORBA.To_CORBA_String ("pk_boolean");
      pk_char_Name : CORBA.String := CORBA.To_CORBA_String ("pk_char");
      pk_octet_Name : CORBA.String := CORBA.To_CORBA_String ("pk_octet");
      pk_any_Name : CORBA.String := CORBA.To_CORBA_String ("pk_any");
      pk_TypeCode_Name : CORBA.String := CORBA.To_CORBA_String ("pk_TypeCode");
      pk_Principal_Name : CORBA.String := CORBA.To_CORBA_String ("pk_Principal");
      pk_string_Name : CORBA.String := CORBA.To_CORBA_String ("pk_string");
      pk_objref_Name : CORBA.String := CORBA.To_CORBA_String ("pk_objref");
      pk_longlong_Name : CORBA.String := CORBA.To_CORBA_String ("pk_longlong");
      pk_ulonglong_Name : CORBA.String := CORBA.To_CORBA_String ("pk_ulonglong");
      pk_longdouble_Name : CORBA.String := CORBA.To_CORBA_String ("pk_longdouble");
      pk_wchar_Name : CORBA.String := CORBA.To_CORBA_String ("pk_wchar");
      pk_wstring_Name : CORBA.String := CORBA.To_CORBA_String ("pk_wstring");
      pk_value_base_Name : CORBA.String := CORBA.To_CORBA_String ("pk_value_base");
   begin
      CORBA.TypeCode.Add_Parameter (TC_PrimitiveKind, CORBA.To_Any (Name));
      CORBA.TypeCode.Add_Parameter (TC_PrimitiveKind, CORBA.To_Any (Id));
      CORBA.TypeCode.Add_Parameter (TC_PrimitiveKind, CORBA.To_Any (pk_null_Name));
      CORBA.TypeCode.Add_Parameter (TC_PrimitiveKind, CORBA.To_Any (pk_void_Name));
      CORBA.TypeCode.Add_Parameter (TC_PrimitiveKind, CORBA.To_Any (pk_short_Name));
      CORBA.TypeCode.Add_Parameter (TC_PrimitiveKind, CORBA.To_Any (pk_long_Name));
      CORBA.TypeCode.Add_Parameter (TC_PrimitiveKind, CORBA.To_Any (pk_ushort_Name));
      CORBA.TypeCode.Add_Parameter (TC_PrimitiveKind, CORBA.To_Any (pk_ulong_Name));
      CORBA.TypeCode.Add_Parameter (TC_PrimitiveKind, CORBA.To_Any (pk_float_Name));
      CORBA.TypeCode.Add_Parameter (TC_PrimitiveKind, CORBA.To_Any (pk_double_Name));
      CORBA.TypeCode.Add_Parameter (TC_PrimitiveKind, CORBA.To_Any (pk_boolean_Name));
      CORBA.TypeCode.Add_Parameter (TC_PrimitiveKind, CORBA.To_Any (pk_char_Name));
      CORBA.TypeCode.Add_Parameter (TC_PrimitiveKind, CORBA.To_Any (pk_octet_Name));
      CORBA.TypeCode.Add_Parameter (TC_PrimitiveKind, CORBA.To_Any (pk_any_Name));
      CORBA.TypeCode.Add_Parameter (TC_PrimitiveKind, CORBA.To_Any (pk_TypeCode_Name));
      CORBA.TypeCode.Add_Parameter (TC_PrimitiveKind, CORBA.To_Any (pk_Principal_Name));
      CORBA.TypeCode.Add_Parameter (TC_PrimitiveKind, CORBA.To_Any (pk_string_Name));
      CORBA.TypeCode.Add_Parameter (TC_PrimitiveKind, CORBA.To_Any (pk_objref_Name));
      CORBA.TypeCode.Add_Parameter (TC_PrimitiveKind, CORBA.To_Any (pk_longlong_Name));
      CORBA.TypeCode.Add_Parameter (TC_PrimitiveKind, CORBA.To_Any (pk_ulonglong_Name));
      CORBA.TypeCode.Add_Parameter (TC_PrimitiveKind, CORBA.To_Any (pk_longdouble_Name));
      CORBA.TypeCode.Add_Parameter (TC_PrimitiveKind, CORBA.To_Any (pk_wchar_Name));
      CORBA.TypeCode.Add_Parameter (TC_PrimitiveKind, CORBA.To_Any (pk_wstring_Name));
      CORBA.TypeCode.Add_Parameter (TC_PrimitiveKind, CORBA.To_Any (pk_value_base_Name));
   end;

   declare
      Name : CORBA.String := CORBA.To_CORBA_String ("ModuleDescription");
      Id : CORBA.String := CORBA.To_CORBA_String ("IDL:omg.org/CORBA/Repository_Root/ModuleDescription:1.0");
      Arg_Name_name : CORBA.String := CORBA.To_CORBA_String ("name");
      Arg_Name_id : CORBA.String := CORBA.To_CORBA_String ("id");
      Arg_Name_defined_in : CORBA.String := CORBA.To_CORBA_String ("defined_in");
      Arg_Name_version : CORBA.String := CORBA.To_CORBA_String ("version");
   begin
      CORBA.TypeCode.Add_Parameter (TC_ModuleDescription, CORBA.To_Any (Name));
      CORBA.TypeCode.Add_Parameter (TC_ModuleDescription, CORBA.To_Any (Id));
      CORBA.TypeCode.Add_Parameter (TC_ModuleDescription, CORBA.To_Any (CORBA.Helper.TC_Identifier));
      CORBA.TypeCode.Add_Parameter (TC_ModuleDescription, CORBA.To_Any (Arg_Name_name));
      CORBA.TypeCode.Add_Parameter (TC_ModuleDescription, CORBA.To_Any (CORBA.Helper.TC_RepositoryId));
      CORBA.TypeCode.Add_Parameter (TC_ModuleDescription, CORBA.To_Any (Arg_Name_id));
      CORBA.TypeCode.Add_Parameter (TC_ModuleDescription, CORBA.To_Any (CORBA.Helper.TC_RepositoryId));
      CORBA.TypeCode.Add_Parameter (TC_ModuleDescription, CORBA.To_Any (Arg_Name_defined_in));
      CORBA.TypeCode.Add_Parameter (TC_ModuleDescription, CORBA.To_Any (CORBA.Repository_Root.Helper.TC_VersionSpec));
      CORBA.TypeCode.Add_Parameter (TC_ModuleDescription, CORBA.To_Any (Arg_Name_version));
   end;

   declare
      Name : CORBA.String := CORBA.To_CORBA_String ("ConstantDescription");
      Id : CORBA.String := CORBA.To_CORBA_String ("IDL:omg.org/CORBA/Repository_Root/ConstantDescription:1.0");
      Arg_Name_name : CORBA.String := CORBA.To_CORBA_String ("name");
      Arg_Name_id : CORBA.String := CORBA.To_CORBA_String ("id");
      Arg_Name_defined_in : CORBA.String := CORBA.To_CORBA_String ("defined_in");
      Arg_Name_version : CORBA.String := CORBA.To_CORBA_String ("version");
      Arg_Name_IDL_type : CORBA.String := CORBA.To_CORBA_String ("IDL_type");
      Arg_Name_value : CORBA.String := CORBA.To_CORBA_String ("value");
   begin
      CORBA.TypeCode.Add_Parameter (TC_ConstantDescription, CORBA.To_Any (Name));
      CORBA.TypeCode.Add_Parameter (TC_ConstantDescription, CORBA.To_Any (Id));
      CORBA.TypeCode.Add_Parameter (TC_ConstantDescription, CORBA.To_Any (CORBA.Helper.TC_Identifier));
      CORBA.TypeCode.Add_Parameter (TC_ConstantDescription, CORBA.To_Any (Arg_Name_name));
      CORBA.TypeCode.Add_Parameter (TC_ConstantDescription, CORBA.To_Any (CORBA.Helper.TC_RepositoryId));
      CORBA.TypeCode.Add_Parameter (TC_ConstantDescription, CORBA.To_Any (Arg_Name_id));
      CORBA.TypeCode.Add_Parameter (TC_ConstantDescription, CORBA.To_Any (CORBA.Helper.TC_RepositoryId));
      CORBA.TypeCode.Add_Parameter (TC_ConstantDescription, CORBA.To_Any (Arg_Name_defined_in));
      CORBA.TypeCode.Add_Parameter (TC_ConstantDescription, CORBA.To_Any (CORBA.Repository_Root.Helper.TC_VersionSpec));
      CORBA.TypeCode.Add_Parameter (TC_ConstantDescription, CORBA.To_Any (Arg_Name_version));
      CORBA.TypeCode.Add_Parameter (TC_ConstantDescription, CORBA.To_Any (CORBA.TC_TypeCode));
      CORBA.TypeCode.Add_Parameter (TC_ConstantDescription, CORBA.To_Any (Arg_Name_IDL_type));
      CORBA.TypeCode.Add_Parameter (TC_ConstantDescription, CORBA.To_Any (CORBA.TC_Any));
      CORBA.TypeCode.Add_Parameter (TC_ConstantDescription, CORBA.To_Any (Arg_Name_value));
   end;

   declare
      Name : CORBA.String := CORBA.To_CORBA_String ("TypeDescription");
      Id : CORBA.String := CORBA.To_CORBA_String ("IDL:omg.org/CORBA/Repository_Root/TypeDescription:1.0");
      Arg_Name_name : CORBA.String := CORBA.To_CORBA_String ("name");
      Arg_Name_id : CORBA.String := CORBA.To_CORBA_String ("id");
      Arg_Name_defined_in : CORBA.String := CORBA.To_CORBA_String ("defined_in");
      Arg_Name_version : CORBA.String := CORBA.To_CORBA_String ("version");
      Arg_Name_IDL_type : CORBA.String := CORBA.To_CORBA_String ("IDL_type");
   begin
      CORBA.TypeCode.Add_Parameter (TC_TypeDescription, CORBA.To_Any (Name));
      CORBA.TypeCode.Add_Parameter (TC_TypeDescription, CORBA.To_Any (Id));
      CORBA.TypeCode.Add_Parameter (TC_TypeDescription, CORBA.To_Any (CORBA.Helper.TC_Identifier));
      CORBA.TypeCode.Add_Parameter (TC_TypeDescription, CORBA.To_Any (Arg_Name_name));
      CORBA.TypeCode.Add_Parameter (TC_TypeDescription, CORBA.To_Any (CORBA.Helper.TC_RepositoryId));
      CORBA.TypeCode.Add_Parameter (TC_TypeDescription, CORBA.To_Any (Arg_Name_id));
      CORBA.TypeCode.Add_Parameter (TC_TypeDescription, CORBA.To_Any (CORBA.Helper.TC_RepositoryId));
      CORBA.TypeCode.Add_Parameter (TC_TypeDescription, CORBA.To_Any (Arg_Name_defined_in));
      CORBA.TypeCode.Add_Parameter (TC_TypeDescription, CORBA.To_Any (CORBA.Repository_Root.Helper.TC_VersionSpec));
      CORBA.TypeCode.Add_Parameter (TC_TypeDescription, CORBA.To_Any (Arg_Name_version));
      CORBA.TypeCode.Add_Parameter (TC_TypeDescription, CORBA.To_Any (CORBA.TC_TypeCode));
      CORBA.TypeCode.Add_Parameter (TC_TypeDescription, CORBA.To_Any (Arg_Name_IDL_type));
   end;

   declare
      Name : CORBA.String := CORBA.To_CORBA_String ("ExceptionDescription");
      Id : CORBA.String := CORBA.To_CORBA_String ("IDL:omg.org/CORBA/Repository_Root/ExceptionDescription:1.0");
      Arg_Name_name : CORBA.String := CORBA.To_CORBA_String ("name");
      Arg_Name_id : CORBA.String := CORBA.To_CORBA_String ("id");
      Arg_Name_defined_in : CORBA.String := CORBA.To_CORBA_String ("defined_in");
      Arg_Name_version : CORBA.String := CORBA.To_CORBA_String ("version");
      Arg_Name_IDL_type : CORBA.String := CORBA.To_CORBA_String ("IDL_type");
   begin
      CORBA.TypeCode.Add_Parameter (TC_ExceptionDescription, CORBA.To_Any (Name));
      CORBA.TypeCode.Add_Parameter (TC_ExceptionDescription, CORBA.To_Any (Id));
      CORBA.TypeCode.Add_Parameter (TC_ExceptionDescription, CORBA.To_Any (CORBA.Helper.TC_Identifier));
      CORBA.TypeCode.Add_Parameter (TC_ExceptionDescription, CORBA.To_Any (Arg_Name_name));
      CORBA.TypeCode.Add_Parameter (TC_ExceptionDescription, CORBA.To_Any (CORBA.Helper.TC_RepositoryId));
      CORBA.TypeCode.Add_Parameter (TC_ExceptionDescription, CORBA.To_Any (Arg_Name_id));
      CORBA.TypeCode.Add_Parameter (TC_ExceptionDescription, CORBA.To_Any (CORBA.Helper.TC_RepositoryId));
      CORBA.TypeCode.Add_Parameter (TC_ExceptionDescription, CORBA.To_Any (Arg_Name_defined_in));
      CORBA.TypeCode.Add_Parameter (TC_ExceptionDescription, CORBA.To_Any (CORBA.Repository_Root.Helper.TC_VersionSpec));
      CORBA.TypeCode.Add_Parameter (TC_ExceptionDescription, CORBA.To_Any (Arg_Name_version));
      CORBA.TypeCode.Add_Parameter (TC_ExceptionDescription, CORBA.To_Any (CORBA.TC_TypeCode));
      CORBA.TypeCode.Add_Parameter (TC_ExceptionDescription, CORBA.To_Any (Arg_Name_IDL_type));
   end;
   declare
      Name : CORBA.String := CORBA.To_CORBA_String ("AttributeMode");
      Id : CORBA.String := CORBA.To_CORBA_String ("IDL:omg.org/CORBA/Repository_Root/AttributeMode:1.0");
      ATTR_NORMAL_Name : CORBA.String := CORBA.To_CORBA_String ("ATTR_NORMAL");
      ATTR_READONLY_Name : CORBA.String := CORBA.To_CORBA_String ("ATTR_READONLY");
   begin
      CORBA.TypeCode.Add_Parameter (TC_AttributeMode, CORBA.To_Any (Name));
      CORBA.TypeCode.Add_Parameter (TC_AttributeMode, CORBA.To_Any (Id));
      CORBA.TypeCode.Add_Parameter (TC_AttributeMode, CORBA.To_Any (ATTR_NORMAL_Name));
      CORBA.TypeCode.Add_Parameter (TC_AttributeMode, CORBA.To_Any (ATTR_READONLY_Name));
   end;

   declare
      Name : CORBA.String := CORBA.To_CORBA_String ("AttributeDescription");
      Id : CORBA.String := CORBA.To_CORBA_String ("IDL:omg.org/CORBA/Repository_Root/AttributeDescription:1.0");
      Arg_Name_name : CORBA.String := CORBA.To_CORBA_String ("name");
      Arg_Name_id : CORBA.String := CORBA.To_CORBA_String ("id");
      Arg_Name_defined_in : CORBA.String := CORBA.To_CORBA_String ("defined_in");
      Arg_Name_version : CORBA.String := CORBA.To_CORBA_String ("version");
      Arg_Name_IDL_type : CORBA.String := CORBA.To_CORBA_String ("IDL_type");
      Arg_Name_mode : CORBA.String := CORBA.To_CORBA_String ("mode");
   begin
      CORBA.TypeCode.Add_Parameter (TC_AttributeDescription, CORBA.To_Any (Name));
      CORBA.TypeCode.Add_Parameter (TC_AttributeDescription, CORBA.To_Any (Id));
      CORBA.TypeCode.Add_Parameter (TC_AttributeDescription, CORBA.To_Any (CORBA.Helper.TC_Identifier));
      CORBA.TypeCode.Add_Parameter (TC_AttributeDescription, CORBA.To_Any (Arg_Name_name));
      CORBA.TypeCode.Add_Parameter (TC_AttributeDescription, CORBA.To_Any (CORBA.Helper.TC_RepositoryId));
      CORBA.TypeCode.Add_Parameter (TC_AttributeDescription, CORBA.To_Any (Arg_Name_id));
      CORBA.TypeCode.Add_Parameter (TC_AttributeDescription, CORBA.To_Any (CORBA.Helper.TC_RepositoryId));
      CORBA.TypeCode.Add_Parameter (TC_AttributeDescription, CORBA.To_Any (Arg_Name_defined_in));
      CORBA.TypeCode.Add_Parameter (TC_AttributeDescription, CORBA.To_Any (CORBA.Repository_Root.Helper.TC_VersionSpec));
      CORBA.TypeCode.Add_Parameter (TC_AttributeDescription, CORBA.To_Any (Arg_Name_version));
      CORBA.TypeCode.Add_Parameter (TC_AttributeDescription, CORBA.To_Any (CORBA.TC_TypeCode));
      CORBA.TypeCode.Add_Parameter (TC_AttributeDescription, CORBA.To_Any (Arg_Name_IDL_type));
      CORBA.TypeCode.Add_Parameter (TC_AttributeDescription, CORBA.To_Any (CORBA.Repository_Root.Helper.TC_AttributeMode));
      CORBA.TypeCode.Add_Parameter (TC_AttributeDescription, CORBA.To_Any (Arg_Name_mode));
   end;

   CORBA.TypeCode.Add_Parameter (TC_IDL_SEQUENCE_CORBA_Repository_Root_ParameterDescription, CORBA.To_Any (CORBA.Unsigned_Long (0)));
   CORBA.TypeCode.Add_Parameter (TC_IDL_SEQUENCE_CORBA_Repository_Root_ParameterDescription, CORBA.To_Any (CORBA.Repository_Root.Helper.TC_ParameterDescription));

   declare
      Name : CORBA.String := CORBA.To_CORBA_String ("ParDescriptionSeq");
      Id : CORBA.String := CORBA.To_CORBA_String ("IDL:omg.org/CORBA/Repository_Root/ParDescriptionSeq:1.0");
   begin
      CORBA.TypeCode.Add_Parameter (TC_ParDescriptionSeq, CORBA.To_Any (Name));
      CORBA.TypeCode.Add_Parameter (TC_ParDescriptionSeq, CORBA.To_Any (Id));
      CORBA.TypeCode.Add_Parameter (TC_ParDescriptionSeq, CORBA.To_Any (CORBA.Repository_Root.Helper.TC_IDL_SEQUENCE_CORBA_Repository_Root_ParameterDescription));
   end;

   declare
      Name : CORBA.String := CORBA.To_CORBA_String ("ContextIdentifier");
      Id : CORBA.String := CORBA.To_CORBA_String ("IDL:omg.org/CORBA/Repository_Root/ContextIdentifier:1.0");
   begin
      CORBA.TypeCode.Add_Parameter (TC_ContextIdentifier, CORBA.To_Any (Name));
      CORBA.TypeCode.Add_Parameter (TC_ContextIdentifier, CORBA.To_Any (Id));
      CORBA.TypeCode.Add_Parameter (TC_ContextIdentifier, CORBA.To_Any (CORBA.Helper.TC_Identifier));
   end;

   CORBA.TypeCode.Add_Parameter (TC_IDL_SEQUENCE_CORBA_Repository_Root_ContextIdentifier, CORBA.To_Any (CORBA.Unsigned_Long (0)));
   CORBA.TypeCode.Add_Parameter (TC_IDL_SEQUENCE_CORBA_Repository_Root_ContextIdentifier, CORBA.To_Any (CORBA.Repository_Root.Helper.TC_ContextIdentifier));

   declare
      Name : CORBA.String := CORBA.To_CORBA_String ("ContextIdSeq");
      Id : CORBA.String := CORBA.To_CORBA_String ("IDL:omg.org/CORBA/Repository_Root/ContextIdSeq:1.0");
   begin
      CORBA.TypeCode.Add_Parameter (TC_ContextIdSeq, CORBA.To_Any (Name));
      CORBA.TypeCode.Add_Parameter (TC_ContextIdSeq, CORBA.To_Any (Id));
      CORBA.TypeCode.Add_Parameter (TC_ContextIdSeq, CORBA.To_Any (CORBA.Repository_Root.Helper.TC_IDL_SEQUENCE_CORBA_Repository_Root_ContextIdentifier));
   end;

   CORBA.TypeCode.Add_Parameter (TC_IDL_SEQUENCE_CORBA_Repository_Root_ExceptionDescription, CORBA.To_Any (CORBA.Unsigned_Long (0)));
   CORBA.TypeCode.Add_Parameter (TC_IDL_SEQUENCE_CORBA_Repository_Root_ExceptionDescription, CORBA.To_Any (CORBA.Repository_Root.Helper.TC_ExceptionDescription));

   declare
      Name : CORBA.String := CORBA.To_CORBA_String ("ExcDescriptionSeq");
      Id : CORBA.String := CORBA.To_CORBA_String ("IDL:omg.org/CORBA/Repository_Root/ExcDescriptionSeq:1.0");
   begin
      CORBA.TypeCode.Add_Parameter (TC_ExcDescriptionSeq, CORBA.To_Any (Name));
      CORBA.TypeCode.Add_Parameter (TC_ExcDescriptionSeq, CORBA.To_Any (Id));
      CORBA.TypeCode.Add_Parameter (TC_ExcDescriptionSeq, CORBA.To_Any (CORBA.Repository_Root.Helper.TC_IDL_SEQUENCE_CORBA_Repository_Root_ExceptionDescription));
   end;

   declare
      Name : CORBA.String := CORBA.To_CORBA_String ("OperationDescription");
      Id : CORBA.String := CORBA.To_CORBA_String ("IDL:omg.org/CORBA/Repository_Root/OperationDescription:1.0");
      Arg_Name_name : CORBA.String := CORBA.To_CORBA_String ("name");
      Arg_Name_id : CORBA.String := CORBA.To_CORBA_String ("id");
      Arg_Name_defined_in : CORBA.String := CORBA.To_CORBA_String ("defined_in");
      Arg_Name_version : CORBA.String := CORBA.To_CORBA_String ("version");
      Arg_Name_result : CORBA.String := CORBA.To_CORBA_String ("result");
      Arg_Name_mode : CORBA.String := CORBA.To_CORBA_String ("mode");
      Arg_Name_contexts : CORBA.String := CORBA.To_CORBA_String ("contexts");
      Arg_Name_parameters : CORBA.String := CORBA.To_CORBA_String ("parameters");
      Arg_Name_exceptions : CORBA.String := CORBA.To_CORBA_String ("exceptions");
   begin
      CORBA.TypeCode.Add_Parameter (TC_OperationDescription, CORBA.To_Any (Name));
      CORBA.TypeCode.Add_Parameter (TC_OperationDescription, CORBA.To_Any (Id));
      CORBA.TypeCode.Add_Parameter (TC_OperationDescription, CORBA.To_Any (CORBA.Helper.TC_Identifier));
      CORBA.TypeCode.Add_Parameter (TC_OperationDescription, CORBA.To_Any (Arg_Name_name));
      CORBA.TypeCode.Add_Parameter (TC_OperationDescription, CORBA.To_Any (CORBA.Helper.TC_RepositoryId));
      CORBA.TypeCode.Add_Parameter (TC_OperationDescription, CORBA.To_Any (Arg_Name_id));
      CORBA.TypeCode.Add_Parameter (TC_OperationDescription, CORBA.To_Any (CORBA.Helper.TC_RepositoryId));
      CORBA.TypeCode.Add_Parameter (TC_OperationDescription, CORBA.To_Any (Arg_Name_defined_in));
      CORBA.TypeCode.Add_Parameter (TC_OperationDescription, CORBA.To_Any (CORBA.Repository_Root.Helper.TC_VersionSpec));
      CORBA.TypeCode.Add_Parameter (TC_OperationDescription, CORBA.To_Any (Arg_Name_version));
      CORBA.TypeCode.Add_Parameter (TC_OperationDescription, CORBA.To_Any (CORBA.TC_TypeCode));
      CORBA.TypeCode.Add_Parameter (TC_OperationDescription, CORBA.To_Any (Arg_Name_result));
      CORBA.TypeCode.Add_Parameter (TC_OperationDescription, CORBA.To_Any (CORBA.Repository_Root.Helper.TC_OperationMode));
      CORBA.TypeCode.Add_Parameter (TC_OperationDescription, CORBA.To_Any (Arg_Name_mode));
      CORBA.TypeCode.Add_Parameter (TC_OperationDescription, CORBA.To_Any (CORBA.Repository_Root.Helper.TC_ContextIdSeq));
      CORBA.TypeCode.Add_Parameter (TC_OperationDescription, CORBA.To_Any (Arg_Name_contexts));
      CORBA.TypeCode.Add_Parameter (TC_OperationDescription, CORBA.To_Any (CORBA.Repository_Root.Helper.TC_ParDescriptionSeq));
      CORBA.TypeCode.Add_Parameter (TC_OperationDescription, CORBA.To_Any (Arg_Name_parameters));
      CORBA.TypeCode.Add_Parameter (TC_OperationDescription, CORBA.To_Any (CORBA.Repository_Root.Helper.TC_ExcDescriptionSeq));
      CORBA.TypeCode.Add_Parameter (TC_OperationDescription, CORBA.To_Any (Arg_Name_exceptions));
   end;

   CORBA.TypeCode.Add_Parameter (TC_IDL_SEQUENCE_CORBA_RepositoryId, CORBA.To_Any (CORBA.Unsigned_Long (0)));
   CORBA.TypeCode.Add_Parameter (TC_IDL_SEQUENCE_CORBA_RepositoryId, CORBA.To_Any (CORBA.Helper.TC_RepositoryId));

   declare
      Name : CORBA.String := CORBA.To_CORBA_String ("RepositoryIdSeq");
      Id : CORBA.String := CORBA.To_CORBA_String ("IDL:omg.org/CORBA/Repository_Root/RepositoryIdSeq:1.0");
   begin
      CORBA.TypeCode.Add_Parameter (TC_RepositoryIdSeq, CORBA.To_Any (Name));
      CORBA.TypeCode.Add_Parameter (TC_RepositoryIdSeq, CORBA.To_Any (Id));
      CORBA.TypeCode.Add_Parameter (TC_RepositoryIdSeq, CORBA.To_Any (CORBA.Repository_Root.Helper.TC_IDL_SEQUENCE_CORBA_RepositoryId));
   end;

   CORBA.TypeCode.Add_Parameter (TC_IDL_SEQUENCE_CORBA_Repository_Root_OperationDescription, CORBA.To_Any (CORBA.Unsigned_Long (0)));
   CORBA.TypeCode.Add_Parameter (TC_IDL_SEQUENCE_CORBA_Repository_Root_OperationDescription, CORBA.To_Any (CORBA.Repository_Root.Helper.TC_OperationDescription));

   declare
      Name : CORBA.String := CORBA.To_CORBA_String ("OpDescriptionSeq");
      Id : CORBA.String := CORBA.To_CORBA_String ("IDL:omg.org/CORBA/Repository_Root/OpDescriptionSeq:1.0");
   begin
      CORBA.TypeCode.Add_Parameter (TC_OpDescriptionSeq, CORBA.To_Any (Name));
      CORBA.TypeCode.Add_Parameter (TC_OpDescriptionSeq, CORBA.To_Any (Id));
      CORBA.TypeCode.Add_Parameter (TC_OpDescriptionSeq, CORBA.To_Any (CORBA.Repository_Root.Helper.TC_IDL_SEQUENCE_CORBA_Repository_Root_OperationDescription));
   end;

   CORBA.TypeCode.Add_Parameter (TC_IDL_SEQUENCE_CORBA_Repository_Root_AttributeDescription, CORBA.To_Any (CORBA.Unsigned_Long (0)));
   CORBA.TypeCode.Add_Parameter (TC_IDL_SEQUENCE_CORBA_Repository_Root_AttributeDescription, CORBA.To_Any (CORBA.Repository_Root.Helper.TC_AttributeDescription));

   declare
      Name : CORBA.String := CORBA.To_CORBA_String ("AttrDescriptionSeq");
      Id : CORBA.String := CORBA.To_CORBA_String ("IDL:omg.org/CORBA/Repository_Root/AttrDescriptionSeq:1.0");
   begin
      CORBA.TypeCode.Add_Parameter (TC_AttrDescriptionSeq, CORBA.To_Any (Name));
      CORBA.TypeCode.Add_Parameter (TC_AttrDescriptionSeq, CORBA.To_Any (Id));
      CORBA.TypeCode.Add_Parameter (TC_AttrDescriptionSeq, CORBA.To_Any (CORBA.Repository_Root.Helper.TC_IDL_SEQUENCE_CORBA_Repository_Root_AttributeDescription));
   end;

   declare
      Name : CORBA.String := CORBA.To_CORBA_String ("InterfaceDescription");
      Id : CORBA.String := CORBA.To_CORBA_String ("IDL:omg.org/CORBA/Repository_Root/InterfaceDescription:2.3");
      Arg_Name_name : CORBA.String := CORBA.To_CORBA_String ("name");
      Arg_Name_id : CORBA.String := CORBA.To_CORBA_String ("id");
      Arg_Name_defined_in : CORBA.String := CORBA.To_CORBA_String ("defined_in");
      Arg_Name_version : CORBA.String := CORBA.To_CORBA_String ("version");
      Arg_Name_base_interfaces : CORBA.String := CORBA.To_CORBA_String ("base_interfaces");
      Arg_Name_is_abstract : CORBA.String := CORBA.To_CORBA_String ("is_abstract");
   begin
      CORBA.TypeCode.Add_Parameter (TC_InterfaceDescription, CORBA.To_Any (Name));
      CORBA.TypeCode.Add_Parameter (TC_InterfaceDescription, CORBA.To_Any (Id));
      CORBA.TypeCode.Add_Parameter (TC_InterfaceDescription, CORBA.To_Any (CORBA.Helper.TC_Identifier));
      CORBA.TypeCode.Add_Parameter (TC_InterfaceDescription, CORBA.To_Any (Arg_Name_name));
      CORBA.TypeCode.Add_Parameter (TC_InterfaceDescription, CORBA.To_Any (CORBA.Helper.TC_RepositoryId));
      CORBA.TypeCode.Add_Parameter (TC_InterfaceDescription, CORBA.To_Any (Arg_Name_id));
      CORBA.TypeCode.Add_Parameter (TC_InterfaceDescription, CORBA.To_Any (CORBA.Helper.TC_RepositoryId));
      CORBA.TypeCode.Add_Parameter (TC_InterfaceDescription, CORBA.To_Any (Arg_Name_defined_in));
      CORBA.TypeCode.Add_Parameter (TC_InterfaceDescription, CORBA.To_Any (CORBA.Repository_Root.Helper.TC_VersionSpec));
      CORBA.TypeCode.Add_Parameter (TC_InterfaceDescription, CORBA.To_Any (Arg_Name_version));
      CORBA.TypeCode.Add_Parameter (TC_InterfaceDescription, CORBA.To_Any (CORBA.Repository_Root.Helper.TC_RepositoryIdSeq));
      CORBA.TypeCode.Add_Parameter (TC_InterfaceDescription, CORBA.To_Any (Arg_Name_base_interfaces));
      CORBA.TypeCode.Add_Parameter (TC_InterfaceDescription, CORBA.To_Any (CORBA.TC_Boolean));
      CORBA.TypeCode.Add_Parameter (TC_InterfaceDescription, CORBA.To_Any (Arg_Name_is_abstract));
   end;

   CORBA.TypeCode.Add_Parameter (TC_IDL_SEQUENCE_CORBA_Repository_Root_ValueMember, CORBA.To_Any (CORBA.Unsigned_Long (0)));
   CORBA.TypeCode.Add_Parameter (TC_IDL_SEQUENCE_CORBA_Repository_Root_ValueMember, CORBA.To_Any (CORBA.Repository_Root.Helper.TC_ValueMember));

   declare
      Name : CORBA.String := CORBA.To_CORBA_String ("ValueMemberSeq");
      Id : CORBA.String := CORBA.To_CORBA_String ("IDL:omg.org/CORBA/Repository_Root/ValueMemberSeq:1.0");
   begin
      CORBA.TypeCode.Add_Parameter (TC_ValueMemberSeq, CORBA.To_Any (Name));
      CORBA.TypeCode.Add_Parameter (TC_ValueMemberSeq, CORBA.To_Any (Id));
      CORBA.TypeCode.Add_Parameter (TC_ValueMemberSeq, CORBA.To_Any (CORBA.Repository_Root.Helper.TC_IDL_SEQUENCE_CORBA_Repository_Root_ValueMember));
   end;

   declare
      Name : CORBA.String := CORBA.To_CORBA_String ("ValueDescription");
      Id : CORBA.String := CORBA.To_CORBA_String ("IDL:omg.org/CORBA/Repository_Root/ValueDescription:2.3");
      Arg_Name_name : CORBA.String := CORBA.To_CORBA_String ("name");
      Arg_Name_id : CORBA.String := CORBA.To_CORBA_String ("id");
      Arg_Name_is_abstract : CORBA.String := CORBA.To_CORBA_String ("is_abstract");
      Arg_Name_is_custom : CORBA.String := CORBA.To_CORBA_String ("is_custom");
      Arg_Name_defined_in : CORBA.String := CORBA.To_CORBA_String ("defined_in");
      Arg_Name_version : CORBA.String := CORBA.To_CORBA_String ("version");
      Arg_Name_supported_interfaces : CORBA.String := CORBA.To_CORBA_String ("supported_interfaces");
      Arg_Name_abstract_base_values : CORBA.String := CORBA.To_CORBA_String ("abstract_base_values");
      Arg_Name_is_truncatable : CORBA.String := CORBA.To_CORBA_String ("is_truncatable");
      Arg_Name_base_value : CORBA.String := CORBA.To_CORBA_String ("base_value");
   begin
      CORBA.TypeCode.Add_Parameter (TC_ValueDescription, CORBA.To_Any (Name));
      CORBA.TypeCode.Add_Parameter (TC_ValueDescription, CORBA.To_Any (Id));
      CORBA.TypeCode.Add_Parameter (TC_ValueDescription, CORBA.To_Any (CORBA.Helper.TC_Identifier));
      CORBA.TypeCode.Add_Parameter (TC_ValueDescription, CORBA.To_Any (Arg_Name_name));
      CORBA.TypeCode.Add_Parameter (TC_ValueDescription, CORBA.To_Any (CORBA.Helper.TC_RepositoryId));
      CORBA.TypeCode.Add_Parameter (TC_ValueDescription, CORBA.To_Any (Arg_Name_id));
      CORBA.TypeCode.Add_Parameter (TC_ValueDescription, CORBA.To_Any (CORBA.TC_Boolean));
      CORBA.TypeCode.Add_Parameter (TC_ValueDescription, CORBA.To_Any (Arg_Name_is_abstract));
      CORBA.TypeCode.Add_Parameter (TC_ValueDescription, CORBA.To_Any (CORBA.TC_Boolean));
      CORBA.TypeCode.Add_Parameter (TC_ValueDescription, CORBA.To_Any (Arg_Name_is_custom));
      CORBA.TypeCode.Add_Parameter (TC_ValueDescription, CORBA.To_Any (CORBA.Helper.TC_RepositoryId));
      CORBA.TypeCode.Add_Parameter (TC_ValueDescription, CORBA.To_Any (Arg_Name_defined_in));
      CORBA.TypeCode.Add_Parameter (TC_ValueDescription, CORBA.To_Any (CORBA.Repository_Root.Helper.TC_VersionSpec));
      CORBA.TypeCode.Add_Parameter (TC_ValueDescription, CORBA.To_Any (Arg_Name_version));
      CORBA.TypeCode.Add_Parameter (TC_ValueDescription, CORBA.To_Any (CORBA.Repository_Root.Helper.TC_RepositoryIdSeq));
      CORBA.TypeCode.Add_Parameter (TC_ValueDescription, CORBA.To_Any (Arg_Name_supported_interfaces));
      CORBA.TypeCode.Add_Parameter (TC_ValueDescription, CORBA.To_Any (CORBA.Repository_Root.Helper.TC_RepositoryIdSeq));
      CORBA.TypeCode.Add_Parameter (TC_ValueDescription, CORBA.To_Any (Arg_Name_abstract_base_values));
      CORBA.TypeCode.Add_Parameter (TC_ValueDescription, CORBA.To_Any (CORBA.TC_Boolean));
      CORBA.TypeCode.Add_Parameter (TC_ValueDescription, CORBA.To_Any (Arg_Name_is_truncatable));
      CORBA.TypeCode.Add_Parameter (TC_ValueDescription, CORBA.To_Any (CORBA.Helper.TC_RepositoryId));
      CORBA.TypeCode.Add_Parameter (TC_ValueDescription, CORBA.To_Any (Arg_Name_base_value));
   end;

end CORBA.Repository_Root.Helper;
