----------------------------------------------
--  This file has been generated automatically
--  by AdaBroker (http://adabroker.eu.org/)
----------------------------------------------

with Ada.Tags;
with Ada.Strings.Unbounded;
with CORBA.AbstractBase;
with CORBA.Impl;
with CORBA.Object;

with CORBA.Repository_Root; use CORBA.Repository_Root;
with CORBA.Repository_Root.Contained;
with CORBA.Repository_Root.Container.Helper;
with CORBA.Repository_Root.Container.Skel;
with CORBA.Repository_Root.StructDef.Impl;
with CORBA.Repository_Root.UnionDef.Impl;
with CORBA.Repository_Root.EnumDef.Impl;
with CORBA.Repository_Root.AliasDef.Impl;
with CORBA.Repository_Root.NativeDef.Impl;
with CORBA.Repository_Root.ExceptionDef.Impl;
with CORBA.Repository_Root.ValueBoxDef.Impl;
with CORBA.Repository_Root.Repository.Impl;
with CORBA.Repository_Root.InterfaceDef.Impl;
with CORBA.Repository_Root.ValueDef.Impl;
with CORBA.Repository_Root.ModuleDef.Impl;
with CORBA.Repository_Root.IDLType;
with CORBA.Repository_Root.IDLType.Impl;
with CORBA.Repository_Root.ConstantDef.Impl;

with CORBA.Repository_Root.ModuleDef;
with CORBA.Repository_Root.ExceptionDef;
with CORBA.Repository_Root.InterfaceDef;
with CORBA.Repository_Root.ValueDef;
with CORBA.Repository_Root.StructDef;
with CORBA.Repository_Root.UnionDef;
with CORBA.Repository_Root.Repository;

with Broca.Exceptions;
with Broca.Debug;
with Broca.Server_Tools;
with PortableServer;

package body CORBA.Repository_Root.Container.Impl is

   -----------
   -- Debug --
   -----------

   Flag : constant Natural
     := Broca.Debug.Is_Active ("container.impl");
   procedure O is new Broca.Debug.Output (Flag);

   Flag2 : constant Natural
     := Broca.Debug.Is_Active ("container.impl_method_trace");
   procedure O2 is new Broca.Debug.Output (Flag2);


   ----------------------
   --  Procedure init  --
   ----------------------
   procedure Init (Self : access Object;
                   Real_Object : IRObject.Impl.Object_Ptr;
                   Def_Kind : Corba.Repository_Root.DefinitionKind;
                   Contents :
                     CORBA.Repository_Root.Contained.Impl.Contained_Seq.Sequence) is
   begin
      pragma Debug (O2 ("init (container)"));
      IRObject.Impl.Init (IRObject.Impl.Object_Ptr (Self), Real_Object, Def_Kind);
      Self.Contents := Contents;
   end Init;


   -----------------
   --  To_Object  --
   -----------------
   function To_Object (Fw_Ref : Container_Forward.Ref)
                       return Object_Ptr is
--      Result : IRObject.Impl.Object_Ptr;
      The_Ref : Container.Ref;
      Obj : Portableserver.Servant;
   begin
      pragma Debug (O2 ("to_object (container)"));
      The_Ref := Container.Convert_Forward.To_Ref (Fw_Ref);
      pragma Debug (O2 ("to_object, before object_of"));
      Broca.Server_Tools.Reference_To_Servant (The_Ref,
                                               Obj);
      return To_Container (IROBject.Impl.Object_Ptr (Obj));
   end To_Object;

   ------------------
   --  To_Forward  --
   ------------------
   function To_Forward (Obj : Object_Ptr)
                        return Container_Forward.Ref is
      Result : Container_Forward.Ref;
   begin
      case Get_Def_Kind (Obj) is
         when
           Dk_Primitive  |
           Dk_String     |
           Dk_Sequence   |
           Dk_Array      |
           Dk_Wstring    |
           Dk_Fixed      |
           Dk_All        |
           Dk_Typedef    |
           Dk_Enum       |
           Dk_Alias      |
           Dk_Native     |
           Dk_ValueBox   |
           Dk_Attribute  |
           Dk_Constant   |
           Dk_Operation  |
           Dk_ValueMember|
           Dk_None       =>
            Broca.Exceptions.Raise_Internal;
            return Result;
         when Dk_Interface  =>
            declare
               The_Ref : InterfaceDef.Ref;
            begin
               Broca.Server_Tools.Initiate_Servant (PortableServer.Servant
                                                    (Get_Real_Object (Obj)),
                                                    The_Ref);
               return Container.Convert_Forward.To_Forward
                 (Container.Helper.To_Ref (The_Ref));
            end;
         when Dk_Value      =>
            declare
               The_Ref : ValueDef.Ref;
            begin
               Broca.Server_Tools.Initiate_Servant (PortableServer.Servant
                                                    (Get_Real_Object (Obj)),
                                                    The_Ref);
               return Container.Convert_Forward.To_Forward
                 (Container.Helper.To_Ref (The_Ref));
            end;
         when  Dk_Struct     =>
            declare
               The_Ref : StructDef.Ref;
            begin
               Broca.Server_Tools.Initiate_Servant (PortableServer.Servant
                                                    (Get_Real_Object (Obj)),
                                                    The_Ref);
               return Container.Convert_Forward.To_Forward
                 (Container.Helper.To_Ref (The_Ref));
            end;
         when  Dk_Union      =>
            declare
               The_Ref : UnionDef.Ref;
            begin
               Broca.Server_Tools.Initiate_Servant (PortableServer.Servant
                                                    (Get_Real_Object (Obj)),
                                                    The_Ref);
               return Container.Convert_Forward.To_Forward
                 (Container.Helper.To_Ref (The_Ref));
            end;
         when Dk_Exception  =>
            declare
               The_Ref : ExceptionDef.Ref;
            begin
               Broca.Server_Tools.Initiate_Servant (PortableServer.Servant
                                                    (Get_Real_Object (Obj)),
                                                    The_Ref);
               return Container.Convert_Forward.To_Forward
                 (Container.Helper.To_Ref (The_Ref));
            end;
         when Dk_Module     =>
            declare
               The_Ref : ModuleDef.Ref;
            begin
               Broca.Server_Tools.Initiate_Servant (PortableServer.Servant
                                                    (Get_Real_Object (Obj)),
                                                    The_Ref);
               return Container.Convert_Forward.To_Forward
                 (Container.Helper.To_Ref (The_Ref));
            end;
         when Dk_Repository     =>
            declare
               The_Ref : Repository.Ref;
            begin
               Broca.Server_Tools.Initiate_Servant (PortableServer.Servant
                                                    (Get_Real_Object (Obj)),
                                                    The_Ref);
               return Container.Convert_Forward.To_Forward
                 (Container.Helper.To_Ref (The_Ref));
            end;

      end case;
   end To_Forward;

   ------------------------------------------
   --  manipulation of the contents field  --
   ------------------------------------------
   function Get_Contents
     (Self : access Object)
      return CORBA.Repository_Root.Contained.Impl.Contained_Seq.Sequence is
   begin
      return Self.Contents;
   end Get_Contents;

   procedure Set_Contents
     (Self : access Object;
      New_List : in CORBA.Repository_Root.Contained.Impl.Contained_Seq.Sequence) is
   begin
      Self.Contents := New_List;
   end Set_Contents;

   procedure Append_To_Contents (Self : access Object;
                                 Element : Contained.Impl.Object_Ptr)
   is
   begin
      pragma Debug (O2 ("Append_To_Contents (container)"));
      Contained.Impl.Contained_Seq.Append (Self.Contents,
                                           Element);
   end Append_To_Contents;

   procedure Delete_From_Contents (Self : access Object;
                                   Element : Contained.Impl.Object_Ptr)
   is
      Index : Positive;
      Cont_Array : Contained.Impl.Contained_Seq.Element_Array (1 .. 1);
   begin
      Cont_Array (1) := Element;
      Index := Contained.Impl.Contained_Seq.Index
        (Self.Contents,
         Cont_Array);
      Contained.Impl.Contained_Seq.Delete (Self.Contents,
                                           Index,
                                           Natural (Index));
   end Delete_From_Contents;

   --------------------
   --  To_Container  --
   --------------------
   procedure To_Container
     (Self : IRObject.Impl.Object_Ptr;
      Success : out Boolean;
      Result : out Object_ptr)
   is
   begin
      pragma Debug (O2 ("to_container (container)"));
      Success := True;
      case IRObject.Impl.Get_Def_Kind
        (Self) is
         when
           Dk_Attribute  |
           Dk_Constant   |
           Dk_Operation  |
           Dk_Typedef    |
           Dk_Alias      |
           Dk_Primitive  |
           Dk_String     |
           Dk_Sequence   |
           Dk_Array      |
           Dk_Wstring    |
           Dk_Fixed      |
           Dk_Enum       |
           Dk_ValueBox   |
           dk_ValueMember|
           dk_Native     |
           Dk_All        |
           Dk_None       =>
            Success := False;
            Result := null;
         when
           --  inherited types
           Dk_Repository |
           Dk_Value      |
           Dk_Module     |
           Dk_Exception  |
           Dk_Interface  =>
            Result := Object_Ptr (Self);
         when
           -- types containing a "container_view" field
           Dk_Struct     =>
            declare
               Interm : Structdef.Impl.Object_Ptr :=
                 Structdef.Impl.Object_Ptr (Self);
            begin
               Result := Structdef.Impl.Get_Container_View (Interm);
            end;
         when
           -- types containing a "container_view" field
           Dk_Union      =>
            declare
               Interm : Uniondef.Impl.Object_Ptr :=
                 Uniondef.Impl.Object_Ptr (Self);
            begin
               Result := Uniondef.Impl.Get_Container_View (Interm);
            end;
      end case;
      return;
   end To_Container;

   function To_Container
     (Self : IRObject.Impl.Object_Ptr)
      return Object_ptr
   is
   begin
      pragma Debug (O2 ("to_container (container)"));
      case IRObject.Impl.Get_Def_Kind
        (Self) is
         when
           Dk_Attribute  |
           Dk_Constant   |
           Dk_Operation  |
           Dk_Typedef    |
           Dk_Alias      |
           Dk_Primitive  |
           Dk_String     |
           Dk_Sequence   |
           Dk_Array      |
           Dk_Wstring    |
           Dk_Fixed      |
           Dk_Enum       |
           Dk_ValueBox   |
           dk_ValueMember|
           dk_Native     |
           Dk_All        |
           Dk_None       =>
            Broca.Exceptions.Raise_Internal;
            return null;
         when
           --  inherited types
           Dk_Repository |
           Dk_Value      |
           Dk_Module     |
           Dk_Exception  |
           Dk_Interface  =>
            return Object_Ptr (Self);
         when
           -- types containing a "container_view" field
           Dk_Struct     =>
            declare
               Interm : Structdef.Impl.Object_Ptr :=
                 Structdef.Impl.Object_Ptr (Self);
            begin
               return Structdef.Impl.Get_Container_View (Interm);
            end;
         when
           -- types containing a "container_view" field
           Dk_Union      =>
            declare
               Interm : Uniondef.Impl.Object_Ptr :=
                 Uniondef.Impl.Object_Ptr (Self);
            begin
               return Uniondef.Impl.Get_Container_View (Interm);
            end;
      end case;
   end To_Container;

   ----------------
   --  Check_Id  --
   ----------------
   function Check_Id (Self : access Object;
                        Id : RepositoryId) return Boolean
   is
      Rep : Repository.Impl.Object_Ptr;
      use Contained.Impl;
   begin
      pragma Debug (O2 ("Check_Id (container)"));
      if Get_Def_Kind (Self) = Dk_Repository then
         Rep := Repository.Impl.Object_Ptr (Object_Ptr (Self));
      else
         Rep := Repository.Impl.To_Object
           (Get_Containing_Repository
            (To_Contained (Get_Real_Object (Self))));
      end if;
      if not Contained.Is_Nil (Repository.Impl.Lookup_Id (Rep, Id)) then
         --  The same Id already exists in this repository
         Broca.Exceptions.Raise_Bad_Param (2);
         return False;
      end if;

      return True;
   end Check_Id;

   ------------------
   --  Check_Name  --
   ------------------
   function Check_Name (Self : access Object;
                        Name : Identifier) return Boolean
   is
      package Contained_For_Seq renames IDL_SEQUENCE_CORBA_Repository_Root_Contained_Forward;
      use Contained_For_Seq;
   begin
      pragma Debug (O2 ("Check_Name (container)"));
      if Contained_For_Seq.Sequence
        (Lookup_Name (Self, Name, -1, Dk_All, True)) /=
        Contained_For_Seq.Null_Sequence then
         --  there is already a node using this name in this scope.
         Broca.Exceptions.Raise_Bad_Param (Minor => 3);
         return False;
      end if;

      return True;
   end Check_Name;

   -----------------------
   --  Check_structure  --
   -----------------------
   function Check_Structure (Self : access Object;
                             Kind : DefinitionKind) return Boolean
   is
      Not_Allowed : Boolean := False;
   begin
      pragma Debug (O2 ("Check_Structure (container)"));
      --  the move or creation should comply with p10-8 of the IR spec.
      --  (structure and navigation in the IR)
      case Kind is
         when
           Dk_Operation |
           Dk_Attribute =>
            if Get_Def_Kind (Self) = Dk_Repository
              or Get_Def_Kind (Self) = Dk_Module then
               Not_Allowed := True;
            end if;
         when
           Dk_ValueMember =>
            if Get_Def_Kind (Self) /= Dk_Value
            then
               Not_Allowed := True;
            end if;
         when
           Dk_Module    |
           Dk_Interface |
           Dk_Value  =>
            if Get_Def_Kind (Self) = Dk_Interface
              or Get_Def_Kind (Self) = Dk_Value then
               Not_Allowed := True;
            end if;
         when others =>
            null;
      end case;

      --  exception, union or struct can only contain union, struct or enum.
      if (Get_Def_Kind (Self) = Dk_Struct) or
        (Get_Def_Kind (Self) = Dk_Union) or
        (Get_Def_Kind (Self) = Dk_Exception) then
         if (Kind /= Dk_Struct) and
           (Kind /= Dk_Union) and
           (Kind /= Dk_Enum) then
            Not_Allowed := True;
         end if;
      end if;

      if Not_Allowed then
         Broca.Exceptions.Raise_Bad_Param (Minor => 4);
         return False;
      end if;

      return True;
   end Check_Structure;

   -------------
   -- IR spec --
   -------------

   --------------
   --  Lookup  --
   --------------
   function lookup
     (Self : access Object;
      search_name : in CORBA.ScopedName)
      return CORBA.Repository_Root.Contained.Ref
   is
      Result_Obj : Contained.Impl.Object_Ptr := null;
      use Contained.Impl;
      use Ada.Strings.Unbounded;
   begin
      --  if it begins with :: then lookup in all the repository
      if Head (Unbounded_String (Search_Name), 2) = "::" then
         declare
            New_Search : ScopedName
              := ScopedName (Tail (Unbounded_String (Search_Name),
                                   Length (Unbounded_String (Search_Name)) - 2));
         begin
            if Get_Def_Kind (Self) = Dk_Repository then
               Result_Obj := Lookup_ScopedName (Self.Contents,
                                                New_Search);
            else
               Result_Obj := Lookup_ScopedName
                 (Repository.Impl.Get_Contents
                  (Repository.Impl.To_Object
                   (Get_Containing_Repository
                    (To_Contained (Get_Real_Object (Self))))),
                  New_Search);
            end if;
         end;
      else
         Result_Obj := Lookup_ScopedName (Self.Contents,
                                          Search_Name);
      end if;

      --  return a nil_ref if not found
      if Result_Obj = null then
         return (CORBA.Object.Nil_Ref with null record);
      end if;

      return Contained.Convert_Forward.To_Ref
        (Contained.Impl.To_Forward (Result_Obj));

   end lookup;


   ----------------
   --  contents  --
   ----------------
   function contents
     (Self : access Object;
      limit_type : in CORBA.Repository_Root.DefinitionKind;
      exclude_inherited : in CORBA.Boolean)
      return CORBA.Repository_Root.ContainedSeq
   is
      Result : CORBA.Repository_Root.ContainedSeq;
      package Contained_For_Seq renames IDL_SEQUENCE_CORBA_Repository_Root_Contained_Forward;
      package IDF renames IDL_SEQUENCE_CORBA_Repository_Root_InterfaceDef_Forward;
      package VDF renames IDL_SEQUENCE_CORBA_Repository_Root_ValueDef_Forward;
   begin
      --  Get the direct contained
      Result := Contained.Impl.Contents (Self.Contents,
                                         Limit_Type);

      --  Do we look into the inherited
      if not Exclude_Inherited then
         case Get_Def_Kind (Self) is
            when Dk_Interface =>
               declare
                  IntDefSeq : InterfaceDefSeq
                    := InterfaceDef.Impl.Get_Base_Interfaces
                    (InterfaceDef.Impl.Object_Ptr (Get_Real_Object (Self)));
                  --  create the array of the parent interfaces
                  Int_Array : IDF.Element_Array
                    := IDF.To_Element_Array (IDF.Sequence (IntDefSeq));
               begin
                  for I in Int_Array'Range loop
                     declare
                        Int : InterfaceDef.Impl.Object_Ptr
                          := InterfaceDef.Impl.To_Object (Int_Array (I));
                        Res : ContainedSeq;
                     begin
                        --  we will get all the contained of the inherited interface
                        Res := contents (Object_Ptr (Int),
                                         Limit_Type,
                                         Exclude_Inherited);
                        --  append the current result to the global one
                        Contained_For_Seq.Append
                          (Contained_For_Seq.Sequence (Result),
                           Contained_For_Seq.Sequence (Res));
                     end;
                  end loop;
               end;
            when Dk_Value =>
               --  check the supported interfaces
               declare
                  IntDefSeq : InterfaceDefSeq
                    := ValueDef.Impl.Get_Supported_Interfaces
                    (ValueDef.Impl.Object_Ptr (Get_Real_Object (Self)));
                  --  create the array of the supported interfaces
                  Int_Array : IDF.Element_Array
                    := IDF.To_Element_Array (IDF.Sequence (IntDefSeq));
               begin
                  for I in Int_Array'Range loop
                     declare
                       Int : InterfaceDef.Impl.Object_Ptr
                         := InterfaceDef.Impl.To_Object (Int_Array (I));
                       Res : ContainedSeq;
                     begin
                        --  we will get all the definition of the inherited interface
                        Res := Contents (Object_Ptr (Int),
                                         Limit_Type,
                                         Exclude_Inherited);
                        --  append the current result to the global one
                        Contained_For_Seq.Append
                          (Contained_For_Seq.Sequence (Result),
                           Contained_For_Seq.Sequence (Res));
                     end;
                  end loop;
               end;
               --  check the abstract_base_value
               declare
                  ValDefSeq : ValueDefSeq
                    := ValueDef.Impl.Get_Abstract_Base_Values
                    (ValueDef.Impl.Object_Ptr (Get_Real_Object (Self)));
                  --  create the array of the supported Values
                  Val_Array : VDF.Element_Array
                    := VDF.To_Element_Array (VDF.Sequence (ValDefSeq));
               begin
                  for I in Val_Array'Range loop
                     declare
                        Val : ValueDef.Impl.Object_Ptr
                          := ValueDef.Impl.To_Object (Val_Array (I));
                        Res : ContainedSeq;
                     begin
                        --  we will get all the definition of the inherited Value
                        Res := Contents (Object_Ptr (Val),
                                         Limit_Type,
                                         Exclude_Inherited);
                        --  append the current result to the global one
                        Contained_For_Seq.Append
                          (Contained_For_Seq.Sequence (Result),
                           Contained_For_Seq.Sequence (Res));
                     end;
                  end loop;
               end;
               --  check the base_value
               declare
                  Obj : Portableserver.Servant;
                  Res : ContainedSeq;
               begin
                  Broca.Server_Tools.Reference_To_Servant
                    (ValueDef.Impl.Get_Base_Value
                     (ValueDef.Impl.Object_Ptr (Get_Real_Object (Self))),
                     Obj);

                  --  we will get all the definition of the inherited Value
                  Res := Contents (Object_Ptr (Obj),
                                   Limit_Type,
                                   Exclude_Inherited);
                  --  append the current result to the global one
                  Contained_For_Seq.Append
                    (Contained_For_Seq.Sequence (Result),
                     Contained_For_Seq.Sequence (Res));
               end;
            when others =>
               null;
         end case;
      end if;

      --  remove the twins (in case of a diamond inheritance)
      Contained.Impl.Simplify_ContainedSeq (Result);

      return Result;
   end contents;


   -------------------
   --  Lookup_Name  --
   -------------------
   function Lookup_Name
     (Self : access Object;
      search_name : in CORBA.Identifier;
      levels_to_search : in CORBA.Long;
      limit_type : in CORBA.Repository_Root.DefinitionKind;
      exclude_inherited : in CORBA.Boolean)
      return CORBA.Repository_Root.ContainedSeq
   is
      package Contained_For_Seq renames IDL_SEQUENCE_CORBA_Repository_Root_Contained_Forward;
      package IDF renames IDL_SEQUENCE_CORBA_Repository_Root_InterfaceDef_Forward;
      package VDF renames IDL_SEQUENCE_CORBA_Repository_Root_ValueDef_Forward;
      Result : CORBA.Repository_Root.ContainedSeq;
   begin
      Result := Contained.Impl.Lookup_Name (Self.Contents,
                                            Search_Name,
                                            Limit_Type);

      --  Do we look into the inherited
      if not Exclude_Inherited then
         case Get_Def_Kind (Self) is
            when Dk_Interface =>
               declare
                  IntDefSeq : InterfaceDefSeq
                    := InterfaceDef.Impl.Get_Base_Interfaces
                    (InterfaceDef.Impl.Object_Ptr (Get_Real_Object (Self)));
                  --  create the array of the parent interfaces
                  Int_Array : IDF.Element_Array
                    := IDF.To_Element_Array (IDF.Sequence (IntDefSeq));
               begin
                  for I in Int_Array'Range loop
                     declare
                        Int : InterfaceDef.Impl.Object_Ptr
                          := InterfaceDef.Impl.To_Object (Int_Array (I));
                        Res : ContainedSeq;
                     begin
                        --  we will get all the definition of the inherited interface
                        Res := Lookup_Name (Object_Ptr (Int),
                                            Search_Name,
                                            -1,
                                            Limit_Type,
                                            Exclude_Inherited);
                        --  append the current result to the global one
                        Contained_For_Seq.Append
                          (Contained_For_Seq.Sequence (Result),
                           Contained_For_Seq.Sequence (Res));
                     end;
                  end loop;
               end;
            when Dk_Value =>
               --  check the supported interfaces
               declare
                  IntDefSeq : InterfaceDefSeq
                    := ValueDef.Impl.Get_Supported_Interfaces
                    (ValueDef.Impl.Object_Ptr (Get_Real_Object (Self)));
                  --  create the array of the supported interfaces
                  Int_Array : IDF.Element_Array
                    := IDF.To_Element_Array (IDF.Sequence (IntDefSeq));
               begin
                  for I in Int_Array'Range loop
                     declare
                       Int : InterfaceDef.Impl.Object_Ptr
                         := InterfaceDef.Impl.To_Object (Int_Array (I));
                       Res : ContainedSeq;
                     begin
                        --  we will get all the definition of the inherited interface
                        Res := Lookup_Name (Object_Ptr (Int),
                                            Search_Name,
                                            -1,
                                            Limit_Type,
                                            Exclude_Inherited);
                        --  append the current result to the global one
                        Contained_For_Seq.Append
                          (Contained_For_Seq.Sequence (Result),
                           Contained_For_Seq.Sequence (Res));
                     end;
                  end loop;
               end;
               --  check the abstract_base_value
               declare
                  ValDefSeq : ValueDefSeq
                    := ValueDef.Impl.Get_Abstract_Base_Values
                    (ValueDef.Impl.Object_Ptr (Get_Real_Object (Self)));
                  --  create the array of the supported Values
                  Val_Array : VDF.Element_Array
                    := VDF.To_Element_Array (VDF.Sequence (ValDefSeq));
               begin
                  for I in Val_Array'Range loop
                     declare
                        Val : ValueDef.Impl.Object_Ptr
                          := ValueDef.Impl.To_Object (Val_Array (I));
                        Res : ContainedSeq;
                     begin
                        --  we will get all the definition of the inherited Value
                        Res := Lookup_Name (Object_Ptr (Val),
                                            Search_Name,
                                            -1,
                                            Limit_Type,
                                            Exclude_Inherited);
                        --  append the current result to the global one
                        Contained_For_Seq.Append
                          (Contained_For_Seq.Sequence (Result),
                           Contained_For_Seq.Sequence (Res));
                     end;
                  end loop;
               end;
               --  check the base_value
               declare
                  Obj : Portableserver.Servant;
                  Res : ContainedSeq;
               begin
                  Broca.Server_Tools.Reference_To_Servant
                    (ValueDef.Impl.Get_Base_Value
                     (ValueDef.Impl.Object_Ptr (Get_Real_Object (Self))),
                     Obj);
                  --  we will get all the definition of the inherited Value
                  Res := Lookup_Name (Object_Ptr (Obj),
                                      Search_Name,
                                      -1,
                                      Limit_Type,
                                      Exclude_Inherited);
                  --  append the current result to the global one
                  Contained_For_Seq.Append
                    (Contained_For_Seq.Sequence (Result),
                     Contained_For_Seq.Sequence (Res));
               end;
            when others =>
               null;
         end case;
      end if;

      --  check the different levels (if there is one)
      if (Levels_To_Search > 0) and
        Get_Def_Kind (Self) /= Dk_Repository then
         declare
            New_Level : Long;
            Parent : Object_Ptr
              := To_Object
              (Contained.Impl.Get_Defined_In
               (Contained.Impl.To_Contained
                (Get_Real_Object (Self))));
            Res : ContainedSeq;
         begin
            if Levels_To_Search = 1 then
               New_Level := -1;
            else
               New_Level := Levels_To_Search - 1;
            end if;
            Res := Lookup_Name (Parent,
                                Search_Name,
                                New_Level,
                                Limit_Type,
                                Exclude_Inherited);
            --  append the current result to the global one
            Contained_For_Seq.Append
              (Contained_For_Seq.Sequence (Result),
               Contained_For_Seq.Sequence (Res));
         end;
      end if;

      --  remove the twins (in case of a diamond inheritance)
      Contained.Impl.Simplify_ContainedSeq (Result);

      return Result;
   end lookup_name;


   -------------------------
   --  describe_contents  --
   -------------------------
   function describe_contents
     (Self : access Object;
      limit_type : in CORBA.Repository_Root.DefinitionKind;
      exclude_inherited : in CORBA.Boolean;
      max_returned_objs : in CORBA.Long)
     return CORBA.Repository_Root.Container.DescriptionSeq
   is
      Content : Contained.Impl.Contained_Seq.Sequence;
      package CD renames IDL_SEQUENCE_CORBA_Repository_Root_Container_Description;
      Result : DescriptionSeq := DescriptionSeq (CD.Null_Sequence);
      use Contained.Impl;
   begin
      --  get the contents of the container
      Content := Contained.Impl.To_Contained_Sequence
        (Contents (Self,
                   Limit_Type,
                   Exclude_Inherited));

      --  reduce it to max_returned_objs
      if Max_Returned_Objs > 0 then
         if CORBA.Long (Contained_Seq.Length (Content)) > Max_Returned_Objs then
            Contained_Seq.Head (Content,
                                Natural (Max_Returned_Objs),
                                null);
         end if;
      end if;

      --  get the description and populate the result.
      declare
         Cont_Array : Contained_Seq.Element_Array
           := Contained_Seq.To_Element_Array (Content);
         Des : Contained.Description;
         Ref : Contained.Ref;
         Res_Des : Description;
      begin
         for I in Cont_Array'Range loop
            Des := Contained.Impl.Describe (Cont_Array (I));

            --  get a reference of the object
            Ref := Contained.Convert_Forward.To_Ref
              (Contained.Impl.To_Forward (Cont_Array (I)));

            --  Create the container.description ...
            Res_Des := (Contained_Object => Ref,
                        Kind => Des.Kind,
                        Value => Des.Value);
            --  end add it to the result.
            CD.Append (CD.Sequence (Result), Res_Des);
         end loop;
      end;

      return Result;
   end describe_contents;


   ---------------------
   --  create_<node>  --
   ---------------------
   function create_module
     (Self : access Object;
      id : in CORBA.RepositoryId;
      name : in CORBA.Identifier;
      version : in CORBA.Repository_Root.VersionSpec)
     return CORBA.Repository_Root.ModuleDef_Forward.Ref
   is
      Result : ModuleDef_Forward.Ref;
   begin
      pragma Debug (O2 ("Create_Module (container)"));
      --  is the new structure allowed?
      if not Check_Structure (Self, Dk_Module) or
        not Check_Id (Self, Id) or
        not Check_Name (Self, Name) then
         return (CORBA.Object.Nil_Ref with null record);
      end if;
      declare
         Obj : ModuleDef.Impl.Object_Ptr := new ModuleDef.Impl.Object;
         Cont_Obj : Contained.Impl.Object_Ptr := new Contained.Impl.Object;
      begin

         --  initialization of the object
         pragma Debug (O ("before_init (create_module)"));
         ModuleDef.Impl.Init (Obj,
                              IRObject.Impl.Object_Ptr (Obj),
                              Dk_Module,
                              Id,
                              Name,
                              Version,
                              To_Forward (Object_Ptr (Self)),
                              Contained.Impl.Contained_Seq.Null_Sequence,
                              Cont_Obj);
         pragma Debug (O ("after_init (create_module)"));
         --  add it to the contents field of this container
         Append_To_Contents
           (Self,
            Contained.Impl.To_Contained (IRObject.Impl.Object_Ptr (Obj)));
         pragma Debug (O ("after append_to_contents (create_module)"));
         Result := ModuleDef.Impl.To_Forward (Obj);
         pragma Debug (O ("after to_forward (create_module)"));
         pragma Debug (O ("end (create_module)"));
         return Result;
      end;
   end create_module;


   function create_constant
     (Self : access Object;
      id : in CORBA.RepositoryId;
      name : in CORBA.Identifier;
      version : in CORBA.Repository_Root.VersionSpec;
      IDL_type : in CORBA.Repository_Root.IDLType_Forward.Ref;
      value : in CORBA.Any)
      return CORBA.Repository_Root.ConstantDef_Forward.Ref
   is
   begin
      --  is the new structure allowed?
      if not Check_Structure (Self, Dk_Constant) or
        not Check_Id (Self, Id) or
        not Check_Name (Self, Name) then
         return (CORBA.Object.Nil_Ref with null record);
      end if;
      declare
         Obj : ConstantDef.Impl.Object_Ptr := new ConstantDef.Impl.Object;
      begin
         --  initialization of the object
         ConstantDef.Impl.Init (Obj,
                                IRObject.Impl.Object_Ptr (Obj),
                                Dk_Constant,
                                Id,
                                Name,
                                Version,
                                To_Forward (Object_Ptr (Self)),
                                IDLType.Convert_Forward.To_Ref (IDL_Type),
                                Value);
         --  add it to the contents field of this container
         Append_To_Contents
           (Self,
            Contained.Impl.To_Contained (IRObject.Impl.Object_Ptr (Obj)));
         return ConstantDef.Impl.To_Forward (Obj);
      end;
   end create_constant;


   function create_struct
     (Self : access Object;
      id : in CORBA.RepositoryId;
      name : in CORBA.Identifier;
      version : in CORBA.Repository_Root.VersionSpec;
      members : in CORBA.Repository_Root.StructMemberSeq)
      return CORBA.Repository_Root.StructDef_Forward.Ref
   is
   begin
      --  is the new structure allowed?
      if not Check_Structure (Self, Dk_Struct) or
        not Check_Id (Self, Id) or
        not Check_Name (Self, Name) then
         return (CORBA.Object.Nil_Ref with null record);
      end if;
      declare
         Obj : StructDef.Impl.Object_Ptr := new StructDef.Impl.Object;
         Container_Obj : Object_Ptr := new Object;
         IDLType_Obj : IDLType.Impl.Object_Ptr := new IDLType.Impl.Object;
      begin
         --  initialization of the object
         StructDef.Impl.Init (Obj,
                              IRObject.Impl.Object_Ptr (Obj),
                              Dk_Struct,
                              Id,
                              Name,
                              Version,
                              To_Forward (Object_Ptr (Self)),
                              IDLType_Obj,
                              Contained.Impl.Contained_Seq.Null_Sequence,
                              Container_Obj,
                              Members);

         --  add it to the contents field of this container
         Append_To_Contents
           (Self,
            Contained.Impl.To_Contained (IRObject.Impl.Object_Ptr (Obj)));

         return StructDef.Impl.To_Forward (Obj);
      end;
   end create_struct;


   function create_union
     (Self : access Object;
      id : in CORBA.RepositoryId;
      name : in CORBA.Identifier;
      version : in CORBA.Repository_Root.VersionSpec;
      discriminator_type : in CORBA.Repository_Root.IDLType_Forward.Ref;
      members : in CORBA.Repository_Root.UnionMemberSeq)
     return CORBA.Repository_Root.UnionDef_Forward.Ref
   is
   begin
            --  is the new structure allowed?
      if not Check_Structure (Self, Dk_Union) or
        not Check_Id (Self, Id) or
        not Check_Name (Self, Name) then
         return (CORBA.Object.Nil_Ref with null record);
      end if;
      declare
         Obj : UnionDef.Impl.Object_Ptr := new UnionDef.Impl.Object;
         Container_Obj : Object_Ptr := new Object;
         IDLType_Obj : IDLType.Impl.Object_Ptr := new IDLType.Impl.Object;
      begin
         --  initialization of the object
         UnionDef.Impl.Init (Obj,
                             IRObject.Impl.Object_Ptr (Obj),
                             Dk_Union,
                             Id,
                             Name,
                             Version,
                             To_Forward (Object_Ptr (Self)),
                             IDLType_Obj,
                             Contained.Impl.Contained_Seq.Null_Sequence,
                             Container_Obj,
                             IDLType.Convert_Forward.To_Ref (Discriminator_Type),
                             Members);

         --  add it to the contents field of this container
         Append_To_Contents
           (Self,
            Contained.Impl.To_Contained (IRObject.Impl.Object_Ptr (Obj)));
         return UnionDef.Impl.To_Forward (Obj);
      end;
   end create_union;


   function create_enum
     (Self : access Object;
      id : in CORBA.RepositoryId;
      name : in CORBA.Identifier;
      version : in CORBA.Repository_Root.VersionSpec;
      members : in CORBA.Repository_Root.EnumMemberSeq)
     return CORBA.Repository_Root.EnumDef_Forward.Ref
   is
   begin
            --  is the new structure allowed?
      if not Check_Structure (Self, Dk_Enum) or
        not Check_Id (Self, Id) or
        not Check_Name (Self, Name) then
         return (CORBA.Object.Nil_Ref with null record);
      end if;
      declare
         Obj : EnumDef.Impl.Object_Ptr := new EnumDef.Impl.Object;
         IDLType_Obj : IDLType.Impl.Object_Ptr := new IDLType.Impl.Object;
      begin
         --  initialization of the object
         EnumDef.Impl.Init (Obj,
                            IRObject.Impl.Object_Ptr (Obj),
                            Dk_Enum,
                            Id,
                            Name,
                            Version,
                            To_Forward (Object_Ptr (Self)),
                            IDLType_Obj,
                            Members);

         --  add it to the contents field of this container
         Append_To_Contents
           (Self,
            Contained.Impl.To_Contained (IRObject.Impl.Object_Ptr (Obj)));
         return EnumDef.Impl.To_Forward (Obj);
      end;
   end create_enum;


   function create_alias
     (Self : access Object;
      id : in CORBA.RepositoryId;
      name : in CORBA.Identifier;
      version : in CORBA.Repository_Root.VersionSpec;
      original_type : in CORBA.Repository_Root.IDLType_Forward.Ref)
     return CORBA.Repository_Root.AliasDef_Forward.Ref
   is
   begin
            --  is the new structure allowed?
      if not Check_Structure (Self, Dk_Alias) or
        not Check_Id (Self, Id) or
        not Check_Name (Self, Name) then
         return (CORBA.Object.Nil_Ref with null record);
      end if;
      declare
         Obj : AliasDef.Impl.Object_Ptr := new AliasDef.Impl.Object;
         IDLType_Obj : IDLType.Impl.Object_Ptr := new IDLType.Impl.Object;
      begin
         --  initialization of the object
         AliasDef.Impl.Init (Obj,
                             IRObject.Impl.Object_Ptr (Obj),
                             Dk_Alias,
                             Id,
                             Name,
                             Version,
                             To_Forward (Object_Ptr (Self)),
                             IDLType_Obj,
                             IDLType.Convert_Forward.To_Ref (Original_Type));

         --  add it to the contents field of this container
         Append_To_Contents
           (Self,
            Contained.Impl.To_Contained (IRObject.Impl.Object_Ptr (Obj)));

         return AliasDef.Impl.To_Forward (Obj);
      end;
   end create_alias;


   function create_interface
     (Self : access Object;
      id : in CORBA.RepositoryId;
      name : in CORBA.Identifier;
      version : in CORBA.Repository_Root.VersionSpec;
      base_interfaces : in CORBA.Repository_Root.InterfaceDefSeq;
      is_abstract : in CORBA.Boolean)
     return CORBA.Repository_Root.InterfaceDef_Forward.Ref
   is
   begin
      pragma Debug (O2 ("Create_interface (container)"));
            --  is the new structure allowed?
      if not Check_Structure (Self, Dk_Interface) or
        not Check_Id (Self, Id) or
        not Check_Name (Self, Name) then
         return (CORBA.Object.Nil_Ref with null record);
      end if;
      declare
         Res : CORBA.Repository_Root.InterfaceDef_Forward.Ref;
         Obj : InterfaceDef.Impl.Object_Ptr := new InterfaceDef.Impl.Object;
         Cont_Obj : Contained.Impl.Object_Ptr := new Contained.Impl.Object;
         IDLType_Obj : IDLType.Impl.Object_Ptr := new IDLType.Impl.Object;
      begin
         pragma Debug (O ("Create_interface : before init"));
         --  initialization of the object
         InterfaceDef.Impl.Init (Obj,
                                 IRObject.Impl.Object_Ptr (Obj),
                                 Dk_Interface,
                                 Id,
                                 Name,
                                 Version,
                                 To_Forward (Object_Ptr (Self)),
                                 Contained.Impl.Contained_Seq.Null_Sequence,
                                 Cont_Obj,
                                 IDLType_Obj,
                                 Base_Interfaces,
                                 Is_Abstract);
         pragma Debug (O ("Create_interface : before append"));
         --  add it to the contents field of this container
         Append_To_Contents
           (Self,
            Contained.Impl.To_Contained (IRObject.Impl.Object_Ptr (Obj)));

         pragma Debug (O ("Create_interface : before to_forward"));
         Res := InterfaceDef.Impl.To_Forward (Obj);
         pragma Debug (O ("Create_interface : end"));
         return Res;
      end;
   end create_interface;


   function create_value
     (Self : access Object;
      id : in CORBA.RepositoryId;
      name : in CORBA.Identifier;
      version : in CORBA.Repository_Root.VersionSpec;
      is_custom : in CORBA.Boolean;
      is_abstract : in CORBA.Boolean;
      base_value : in CORBA.Repository_Root.ValueDef_Forward.Ref;
      is_truncatable : in CORBA.Boolean;
      abstract_base_values : in CORBA.Repository_Root.ValueDefSeq;
      supported_interfaces : in CORBA.Repository_Root.InterfaceDefSeq;
      initializers : in CORBA.Repository_Root.InitializerSeq)
     return CORBA.Repository_Root.ValueDef_Forward.Ref
   is
   begin
      --  is the new structure allowed?
      if not Check_Structure (Self, Dk_Value) or
        not Check_Id (Self, Id) or
        not Check_Name (Self, Name) then
         return (CORBA.Object.Nil_Ref with null record);
      end if;
      declare
         Obj : ValueDef.Impl.Object_Ptr := new ValueDef.Impl.Object;
         Cont_Obj : Contained.Impl.Object_Ptr := new Contained.Impl.Object;
         IDLType_Obj : IDLType.Impl.Object_Ptr := new IDLType.Impl.Object;
      begin
         --  there cannot be more then one "true" in those boolean!
         if (Is_Custom and Is_Abstract) or
           (Is_Custom and Is_Truncatable) or
           (Is_Abstract and Is_Truncatable) then
            --  Spec is not precise...
            Broca.Exceptions.Raise_Bad_Param(2);
         end if;

         --  initialization of the object
         ValueDef.Impl.Init (Obj,
                             IRObject.Impl.Object_Ptr (Obj),
                             Dk_Value,
                             Id,
                             Name,
                             Version,
                             To_Forward (Object_Ptr (Self)),
                             Contained.Impl.Contained_Seq.Null_Sequence,
                             Cont_Obj,
                             IDLType_Obj,
                             Supported_Interfaces,
                             Initializers,
                             ValueDef.Convert_Forward.To_Ref (Base_Value),
                             Abstract_Base_Values,
                             Is_Abstract,
                             Is_Custom,
                             Is_Truncatable);
         --  add it to the contents field of this container
         Append_To_Contents
           (Self,
            Contained.Impl.To_Contained (IRObject.Impl.Object_Ptr (Obj)));

         return ValueDef.Impl.To_Forward (Obj);
      end;
   end create_value;


   function create_value_box
     (Self : access Object;
      id : in CORBA.RepositoryId;
      name : in CORBA.Identifier;
      version : in CORBA.Repository_Root.VersionSpec;
      original_type_def : in CORBA.Repository_Root.IDLType_Forward.Ref)
     return CORBA.Repository_Root.ValueBoxDef_Forward.Ref
   is
   begin
      --  is the new structure allowed?
      if not Check_Structure (Self, Dk_ValueBox) or
        not Check_Id (Self, Id) or
        not Check_Name (Self, Name) then
         return (CORBA.Object.Nil_Ref with null record);
      end if;
      declare
         Obj : ValueBoxDef.Impl.Object_Ptr := new ValueBoxDef.Impl.Object;
         IDLType_Obj : IDLType.Impl.Object_Ptr := new IDLType.Impl.Object;
      begin
         --  initialization of the object
         ValueBoxDef.Impl.Init (Obj,
                                IRObject.Impl.Object_Ptr (Obj),
                                Dk_ValueBox,
                                Id,
                                Name,
                                Version,
                                To_Forward (Object_Ptr (Self)),
                                IDLType_Obj,
                                IDLType.Convert_Forward.To_Ref
                                (Original_Type_Def));

         --  add it to the contents field of this container
         Append_To_Contents
           (Self,
            Contained.Impl.To_Contained (IRObject.Impl.Object_Ptr (Obj)));

         return ValueBoxDef.Impl.To_Forward (Obj);
      end;
   end create_value_box;


   function create_exception
     (Self : access Object;
      id : in CORBA.RepositoryId;
      name : in CORBA.Identifier;
      version : in CORBA.Repository_Root.VersionSpec;
      members : in CORBA.Repository_Root.StructMemberSeq)
     return CORBA.Repository_Root.ExceptionDef_Forward.Ref
   is
   begin
            --  is the new structure allowed?
      if not Check_Structure (Self, Dk_Exception) or
        not Check_Id (Self, Id) or
        not Check_Name (Self, Name) then
         return (CORBA.Object.Nil_Ref with null record);
      end if;
      declare
         Obj : ExceptionDef.Impl.Object_Ptr := new ExceptionDef.Impl.Object;
         Cont_Obj : Contained.Impl.Object_Ptr := new Contained.Impl.Object;
      begin
        --  initialization of the object
         ExceptionDef.Impl.Init (Obj,
                                 IRObject.Impl.Object_Ptr (Obj),
                                 Dk_Exception,
                                 Id,
                                 Name,
                                 Version,
                                 To_Forward (Object_Ptr (Self)),
                                 Contained.Impl.Contained_Seq.Null_Sequence,
                                 Cont_Obj,
                                 Members);
         --  add it to the contents field of this container
         Append_To_Contents
           (Self,
            Contained.Impl.To_Contained (IRObject.Impl.Object_Ptr (Obj)));

         return ExceptionDef.Impl.To_Forward (Obj);
      end;
   end create_exception;


   function create_native
     (Self : access Object;
      id : in CORBA.RepositoryId;
      name : in CORBA.Identifier;
      version : in CORBA.Repository_Root.VersionSpec)
     return CORBA.Repository_Root.NativeDef_Forward.Ref
   is
   begin
      --  is the new structure allowed?
      if not Check_Structure (Self, Dk_Native) or
        not Check_Id (Self, Id) or
        not Check_Name (Self, Name) then
         return (CORBA.Object.Nil_Ref with null record);
      end if;
      declare
         Obj : NativeDef.Impl.Object_Ptr := new NativeDef.Impl.Object;
         IDLType_Obj : IDLType.Impl.Object_Ptr := new IDLType.Impl.Object;
      begin
        --  initialization of the object
         NativeDef.Impl.Init (Obj,
                              IRObject.Impl.Object_Ptr (Obj),
                              Dk_Native,
                              Id,
                              Name,
                              Version,
                              To_Forward (Object_Ptr (Self)),
                              IDLType_Obj);

         --  add it to the contents field of this container
         Append_To_Contents
           (Self,
            Contained.Impl.To_Contained (IRObject.Impl.Object_Ptr (Obj)));

         return NativeDef.Impl.To_Forward (Obj);
      end;
   end create_native;

end CORBA.Repository_Root.Container.Impl;


