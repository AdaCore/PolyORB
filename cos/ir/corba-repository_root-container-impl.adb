------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                  CORBA.REPOSITORY_ROOT.CONTAINER.IMPL                    --
--                                                                          --
--                                 B o d y                                  --
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

pragma Style_Checks (Off);

with Ada.Strings.Unbounded;

with CORBA.Repository_Root.AbstractInterfaceDef;
with CORBA.Repository_Root.AliasDef.Impl;
with CORBA.Repository_Root.ConstantDef.Impl;
with CORBA.Repository_Root.Container.Helper;
with CORBA.Repository_Root.Container.Skel;
pragma Warnings (Off, CORBA.Repository_Root.Container.Skel);
with CORBA.Repository_Root.EnumDef.Impl;
with CORBA.Repository_Root.ExceptionDef.Impl;
with CORBA.Repository_Root.ExtAbstractInterfaceDef.Impl;
with CORBA.Repository_Root.ExtInterfaceDef.Impl;
with CORBA.Repository_Root.ExtLocalInterfaceDef.Impl;
with CORBA.Repository_Root.IDLType.Impl;
with CORBA.Repository_Root.InterfaceAttrExtension.Impl;
with CORBA.Repository_Root.InterfaceDef.Impl;
with CORBA.Repository_Root.LocalInterfaceDef;
with CORBA.Repository_Root.ModuleDef.Impl;
with CORBA.Repository_Root.NativeDef.Impl;
with CORBA.Repository_Root.Repository.Impl;
with CORBA.Repository_Root.StructDef.Impl;
with CORBA.Repository_Root.UnionDef.Impl;
with CORBA.Repository_Root.ValueBoxDef.Impl;
with CORBA.Repository_Root.ValueDef.Impl;

with PolyORB.Log;
pragma Elaborate_All (PolyORB.Log);
with PolyORB.CORBA_P.Server_Tools;
with PortableServer;

package body CORBA.Repository_Root.Container.Impl is

   -----------
   -- Debug --
   -----------

   use PolyORB.Log;

   package L is new PolyORB.Log.Facility_Log ("container.impl");
   procedure O (Message : Standard.String; Level : Log_Level := Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
     renames L.Enabled;
   pragma Unreferenced (C); --  For conditional pragma Debug

   package L2 is new PolyORB.Log.Facility_Log ("container.impl_method_trace");
   procedure O2 (Message : Standard.String; Level : Log_Level := Debug)
     renames L2.Output;
   function C2 (Level : Log_Level := Debug) return Boolean
     renames L2.Enabled;
   pragma Unreferenced (C2); --  For conditional pragma Debug

   ----------------------
   --  Procedure init  --
   ----------------------
   procedure Init (Self : access Object;
                   Real_Object : IRObject.Impl.Object_Ptr;
                   Def_Kind : CORBA.Repository_Root.DefinitionKind;
                   Contents :
                     CORBA.Repository_Root.Contained.Impl.Contained_Seq.Sequence) is
   begin
      pragma Debug (O2 ("init (container)"));
      IRObject.Impl.Init (IRObject.Impl.Object_Ptr (Self), Real_Object, Def_kind);
      Self.Contents := Contents;
   end Init;

   -----------------
   --  To_Object  --
   -----------------
   function To_Object (Fw_Ref : Container_Forward.Ref)
                       return Object_Ptr is
--      Result : IRObject.Impl.Object_Ptr;
      The_Ref : Container.Ref;
      Obj : PortableServer.Servant;
   begin
      pragma Debug (O2 ("to_object (container)"));
      The_Ref := Container.Convert_Forward.To_Ref (Fw_Ref);
      pragma Debug (O2 ("to_object, before object_of"));
      PolyORB.CORBA_P.Server_Tools.Reference_To_Servant (The_Ref,
                                               Obj);
      return To_Container (IRObject.Impl.Object_Ptr (Obj));
   end To_Object;

   ------------------
   --  To_Forward  --
   ------------------
   function To_Forward (Obj : Object_Ptr)
                        return Container_Forward.Ref is
      Result : Container_Forward.Ref;
   begin
      case get_def_kind (Obj) is
         when
           dk_Primitive  |
           dk_String     |
           dk_Sequence   |
           dk_Array      |
           dk_Wstring    |
           dk_Fixed      |
           dk_all        |
           dk_Typedef    |
           dk_Enum       |
           dk_Alias      |
           dk_Native     |
           dk_ValueBox   |
           dk_Attribute  |
           dk_Constant   |
           dk_Operation  |
           dk_ValueMember|
           dk_none       =>
            CORBA.Raise_Internal (CORBA.Default_Sys_Member);
            return Result;
         when dk_Interface  =>
            declare
               The_Ref : InterfaceDef.Ref;
            begin
               PolyORB.CORBA_P.Server_Tools.Initiate_Servant (PortableServer.Servant
                                                    (Get_Real_Object (Obj)),
                                                    The_Ref);
               return Container.Convert_Forward.To_Forward
                 (Container.Helper.To_Ref (The_Ref));
            end;
         when dk_Value      =>
            declare
               The_Ref : ValueDef.Ref;
            begin
               PolyORB.CORBA_P.Server_Tools.Initiate_Servant (PortableServer.Servant
                                                    (Get_Real_Object (Obj)),
                                                    The_Ref);
               return Container.Convert_Forward.To_Forward
                 (Container.Helper.To_Ref (The_Ref));
            end;
         when  dk_Struct     =>
            declare
               The_Ref : StructDef.Ref;
            begin
               PolyORB.CORBA_P.Server_Tools.Initiate_Servant (PortableServer.Servant
                                                    (Get_Real_Object (Obj)),
                                                    The_Ref);
               return Container.Convert_Forward.To_Forward
                 (Container.Helper.To_Ref (The_Ref));
            end;
         when  dk_Union      =>
            declare
               The_Ref : UnionDef.Ref;
            begin
               PolyORB.CORBA_P.Server_Tools.Initiate_Servant (PortableServer.Servant
                                                    (Get_Real_Object (Obj)),
                                                    The_Ref);
               return Container.Convert_Forward.To_Forward
                 (Container.Helper.To_Ref (The_Ref));
            end;
         when dk_Exception  =>
            declare
               The_Ref : ExceptionDef.Ref;
            begin
               PolyORB.CORBA_P.Server_Tools.Initiate_Servant (PortableServer.Servant
                                                    (Get_Real_Object (Obj)),
                                                    The_Ref);
               return Container.Convert_Forward.To_Forward
                 (Container.Helper.To_Ref (The_Ref));
            end;
         when dk_Module     =>
            declare
               The_Ref : ModuleDef.Ref;
            begin
               PolyORB.CORBA_P.Server_Tools.Initiate_Servant (PortableServer.Servant
                                                    (Get_Real_Object (Obj)),
                                                    The_Ref);
               return Container.Convert_Forward.To_Forward
                 (Container.Helper.To_Ref (The_Ref));
            end;
         when dk_Repository     =>
            declare
               The_Ref : Repository.Ref;
            begin
               PolyORB.CORBA_P.Server_Tools.Initiate_Servant (PortableServer.Servant
                                                    (Get_Real_Object (Obj)),
                                                    The_Ref);
               return Container.Convert_Forward.To_Forward
                 (Container.Helper.To_Ref (The_Ref));
            end;
         when dk_AbstractInterface .. dk_Event =>
            raise Program_Error;
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
      New_List : CORBA.Repository_Root.Contained.Impl.Contained_Seq.Sequence) is
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

   ------------------
   -- To_Container --
   ------------------

   procedure To_Container
     (Self : IRObject.Impl.Object_Ptr;
      Success : out Boolean;
      Result : out Object_Ptr)
   is
   begin
      pragma Debug (O2 ("to_container (container)"));
      Success := True;
      case IRObject.Impl.get_def_kind
        (Self) is
         when
           dk_Attribute  |
           dk_Constant   |
           dk_Operation  |
           dk_Typedef    |
           dk_Alias      |
           dk_Primitive  |
           dk_String     |
           dk_Sequence   |
           dk_Array      |
           dk_Wstring    |
           dk_Fixed      |
           dk_Enum       |
           dk_ValueBox   |
           dk_ValueMember|
           dk_Native     |
           dk_all        |
           dk_none       =>
            Success := False;
            Result := null;
         when
           --  inherited types
           dk_Repository |
           dk_Value      |
           dk_Module     |
           dk_Exception  |
           dk_Interface  =>
            Result := Object_Ptr (Self);
         when
           -- types containing a "container_view" field
           dk_Struct     =>
            declare
               Interm : constant StructDef.Impl.Object_Ptr :=
                 StructDef.Impl.Object_Ptr (Self);
            begin
               Result := StructDef.Impl.Get_Container_View (Interm);
            end;
         when
           -- types containing a "container_view" field
           dk_Union      =>
            declare
               Interm : constant UnionDef.Impl.Object_Ptr :=
                 UnionDef.Impl.Object_Ptr (Self);
            begin
               Result := UnionDef.Impl.Get_Container_View (Interm);
            end;
         when dk_AbstractInterface .. dk_Event =>
            raise Program_Error;
      end case;
      return;
   end To_Container;

   function To_Container
     (Self : IRObject.Impl.Object_Ptr)
      return Object_Ptr
   is
   begin
      pragma Debug (O2 ("to_container (container)"));
      case IRObject.Impl.get_def_kind
        (Self) is
         when
           dk_Attribute  |
           dk_Constant   |
           dk_Operation  |
           dk_Typedef    |
           dk_Alias      |
           dk_Primitive  |
           dk_String     |
           dk_Sequence   |
           dk_Array      |
           dk_Wstring    |
           dk_Fixed      |
           dk_Enum       |
           dk_ValueBox   |
           dk_ValueMember|
           dk_Native     |
           dk_all        |
           dk_none       =>
            CORBA.Raise_Internal (CORBA.Default_Sys_Member);
            return null;
         when
           --  inherited types
           dk_Repository |
           dk_Value      |
           dk_Module     |
           dk_Exception  |
           dk_Interface  =>
            return Object_Ptr (Self);
         when
           -- types containing a "container_view" field
           dk_Struct     =>
            declare
               Interm : constant StructDef.Impl.Object_Ptr :=
                 StructDef.Impl.Object_Ptr (Self);
            begin
               return StructDef.Impl.Get_Container_View (Interm);
            end;
         when
           -- types containing a "container_view" field
           dk_Union      =>
            declare
               Interm : constant UnionDef.Impl.Object_Ptr :=
                 UnionDef.Impl.Object_Ptr (Self);
            begin
               return UnionDef.Impl.Get_Container_View (Interm);
            end;
         when
           dk_AbstractInterface .. dk_Event =>
            raise Program_Error;
      end case;
   end To_Container;

   --------------
   -- Check_Id --
   --------------

   procedure Check_Id
     (Self : access Object;
      Id   : RepositoryId)
   is
      Rep : Repository.Impl.Object_Ptr;
      use Contained.Impl;
   begin
      pragma Debug (O2 ("Check_Id (container)"));
      if get_def_kind (Self) = dk_Repository then
         Rep := Repository.Impl.Object_Ptr (Object_Ptr (Self));

      else
         Rep := Repository.Impl.To_Object
           (get_containing_repository
            (To_Contained (Get_Real_Object (Self))));
      end if;

      if not Contained.Is_Nil (Repository.Impl.lookup_Id (Rep, id)) then
         --  The same Id already exists in this repository
         CORBA.Raise_Bad_Param (CORBA.System_Exception_Members'
                                (Minor     => 2,
                                 Completed => CORBA.Completed_No));
      end if;
   end Check_Id;

   ----------------
   -- Check_Name --
   ----------------

   procedure Check_Name
     (Self : access Object;
      Name : Identifier)
   is
      package Contained_For_Seq renames IDL_SEQUENCE_CORBA_Contained_Forward;
      use Contained_For_Seq;
   begin
      pragma Debug (O2 ("Check_Name (container)"));
      if Contained_For_Seq.Sequence
        (Lookup_Name (Self, name, -1, dk_all, True)) /=
        Contained_For_Seq.Null_Sequence
      then
         --  There is already a node using this Name in this scope
         CORBA.Raise_Bad_Param (CORBA.System_Exception_Members'
                                (Minor     => 3,
                                 Completed => CORBA.Completed_No));
      end if;
   end Check_Name;

   ---------------------
   -- Check_Structure --
   ---------------------

   procedure Check_Structure
    (Self : access Object;
     Kind : DefinitionKind)
   is
      Self_DK     : constant DefinitionKind := get_def_kind (Self);
      Not_Allowed : Boolean                 := False;
   begin
      pragma Debug (O2 ("Check_Structure (container)"));

      --  The move or creation should comply with CORBA 3.0 10.4.4
      --  (Structure and Navigation of the Interface Repository)

      case Kind is
         when
           dk_Operation |
           dk_Attribute
         =>
            if Self_DK = dk_Repository
              or else Self_DK = dk_Module
            then
               Not_Allowed := True;
            end if;

         when dk_ValueMember =>
            if Self_DK /= dk_value then
               Not_Allowed := True;
            end if;

         when
           dk_Module    |
           dk_Interface |
           dk_Value
         =>
            if Self_DK = dk_Interface
              or else Self_DK = dk_Value
            then
               Not_Allowed := True;
            end if;

         when others =>
            null;
      end case;

      --  Exception, union or struct can only contain union, struct or enum

      if Self_DK = dk_Struct
        or else Self_DK = dk_Union
        or else Self_DK = dk_Exception
      then
         if kind /= dk_Struct
           and then kind /= dk_Union
           and then kind /= dk_Enum
         then
            Not_Allowed := True;
         end if;
      end if;

      if Not_Allowed then
         CORBA.Raise_Bad_Param (CORBA.System_Exception_Members'
                                (Minor     => 4,
                                 Completed => CORBA.Completed_No));
      end if;
   end Check_Structure;

   -------------
   -- IR spec --
   -------------

   --------------
   --  Lookup  --
   --------------
   function lookup
     (Self : access Object;
      search_name : CORBA.ScopedName)
      return CORBA.Repository_Root.Contained.Ref
   is
      Result_Obj : Contained.Impl.Object_Ptr := null;
      Nil_Ref : CORBA.Repository_Root.Contained.Ref;
      use Contained.Impl;
      use Ada.Strings.Unbounded;
   begin
      --  if it begins with :: then lookup in all the repository
      if Head (Unbounded_String (search_name), 2) = "::" then
         declare
            New_Search : ScopedName
              := ScopedName (Tail (Unbounded_String (search_name),
                                   Length (Unbounded_String (search_name)) - 2));
         begin
            if get_def_kind (Self) = dk_Repository then
               Result_Obj := Lookup_ScopedName (Self.Contents,
                                                New_Search);
            else
               Result_Obj := Lookup_ScopedName
                 (Repository.Impl.Get_Contents
                  (Repository.Impl.To_Object
                   (get_containing_repository
                    (To_Contained (Get_Real_Object (Self))))),
                  New_Search);
            end if;
         end;
      else
         Result_Obj := Lookup_ScopedName (Self.Contents,
                                          search_name);
      end if;

      --  return a nil_ref if not found
      if Result_Obj = null then
         return Nil_Ref;
      end if;

      return Contained.Convert_Forward.To_Ref
        (Contained.Impl.To_Forward (Result_Obj));

   end lookup;

   ----------------
   --  contents  --
   ----------------
   function contents
     (Self : access Object;
      limit_type : CORBA.Repository_Root.DefinitionKind;
      exclude_inherited : CORBA.Boolean)
      return CORBA.Repository_Root.ContainedSeq
   is
      Result : CORBA.Repository_Root.ContainedSeq;
      package Contained_For_Seq renames IDL_SEQUENCE_CORBA_Contained_Forward;
      package IdF renames IDL_SEQUENCE_CORBA_InterfaceDef_Forward;
      package VDF renames IDL_SEQUENCE_CORBA_ValueDef_Forward;
   begin
      --  Get the direct contained
      Result := Contained.Impl.contents (Self.Contents,
                                         limit_type);

      --  Do we look into the inherited
      if not exclude_inherited then
         case get_def_kind (Self) is
            when dk_Interface =>
               declare
                  IntDefSeq : InterfaceDefSeq
                    := InterfaceDef.Impl.get_base_interfaces
                    (InterfaceDef.Impl.Object_Ptr (Get_Real_Object (Self)));
                  --  create the array of the parent interfaces
                  Int_Array : IdF.Element_Array
                    := IdF.To_Element_Array (IdF.Sequence (IntDefSeq));
               begin
                  for I in Int_Array'Range loop
                     declare
                        Int : constant InterfaceDef.Impl.Object_Ptr
                          := InterfaceDef.Impl.To_Object (Int_Array (I));
                        Res : ContainedSeq;
                     begin
                        --  we will get all the contained of the inherited interface
                        Res := contents (Object_Ptr (Int),
                                         limit_type,
                                         exclude_inherited);
                        --  append the current result to the global one
                        Contained_For_Seq.Append
                          (Contained_For_Seq.Sequence (Result),
                           Contained_For_Seq.Sequence (Res));
                     end;
                  end loop;
               end;
            when dk_Value =>
               --  check the supported interfaces
               declare
                  IntDefSeq : InterfaceDefSeq
                    := ValueDef.Impl.get_supported_interfaces
                    (ValueDef.Impl.Object_Ptr (Get_Real_Object (Self)));
                  --  create the array of the supported interfaces
                  Int_Array : IdF.Element_Array
                    := IdF.To_Element_Array (IdF.Sequence (IntDefSeq));
               begin
                  for I in Int_Array'Range loop
                     declare
                       Int : constant InterfaceDef.Impl.Object_Ptr
                         := InterfaceDef.Impl.To_Object (Int_Array (I));
                       Res : ContainedSeq;
                     begin
                        --  we will get all the definition of the inherited interface
                        Res := contents (Object_Ptr (Int),
                                         limit_type,
                                         exclude_inherited);
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
                    := ValueDef.Impl.get_abstract_base_values
                    (ValueDef.Impl.Object_Ptr (Get_Real_Object (Self)));
                  --  create the array of the supported values
                  Val_Array : VDF.Element_Array
                    := VDF.To_Element_Array (VDF.Sequence (ValDefSeq));
               begin
                  for I in Val_Array'Range loop
                     declare
                        Val : constant ValueDef.Impl.Object_Ptr
                          := ValueDef.Impl.To_Object (Val_Array (I));
                        Res : ContainedSeq;
                     begin
                        --  we will get all the definition of the inherited value
                        Res := contents (Object_Ptr (Val),
                                         limit_type,
                                         exclude_inherited);
                        --  append the current result to the global one
                        Contained_For_Seq.Append
                          (Contained_For_Seq.Sequence (Result),
                           Contained_For_Seq.Sequence (Res));
                     end;
                  end loop;
               end;
               --  check the base_value
               declare
                  Obj : PortableServer.Servant;
                  Res : ContainedSeq;
               begin
                  PolyORB.CORBA_P.Server_Tools.Reference_To_Servant
                    (ValueDef.Impl.get_base_value
                     (ValueDef.Impl.Object_Ptr (Get_Real_Object (Self))),
                     Obj);

                  --  we will get all the definition of the inherited value
                  Res := contents (Object_Ptr (Obj),
                                   limit_type,
                                   exclude_inherited);
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
   function lookup_name
     (Self : access Object;
      search_name : CORBA.Identifier;
      levels_to_search : CORBA.Long;
      limit_type : CORBA.Repository_Root.DefinitionKind;
      exclude_inherited : CORBA.Boolean)
      return CORBA.Repository_Root.ContainedSeq
   is
      package Contained_For_Seq renames IDL_SEQUENCE_CORBA_Contained_Forward;
      package IdF renames IDL_SEQUENCE_CORBA_InterfaceDef_Forward;
      package VDF renames IDL_SEQUENCE_CORBA_ValueDef_Forward;
      Result : CORBA.Repository_Root.ContainedSeq;
   begin
      Result := Contained.Impl.Lookup_Name (Self.Contents,
                                            search_name,
                                            limit_type);

      --  Do we look into the inherited
      if not exclude_inherited then
         case get_def_kind (Self) is
            when dk_Interface =>
               declare
                  IntDefSeq : InterfaceDefSeq
                    := InterfaceDef.Impl.get_base_interfaces
                    (InterfaceDef.Impl.Object_Ptr (Get_Real_Object (Self)));
                  --  create the array of the parent interfaces
                  Int_Array : IdF.Element_Array
                    := IdF.To_Element_Array (IdF.Sequence (IntDefSeq));
               begin
                  for I in Int_Array'Range loop
                     declare
                        Int : constant InterfaceDef.Impl.Object_Ptr
                          := InterfaceDef.Impl.To_Object (Int_Array (I));
                        Res : ContainedSeq;
                     begin
                        --  we will get all the definition of the inherited interface
                        Res := lookup_name (Object_Ptr (Int),
                                            search_name,
                                            -1,
                                            limit_type,
                                            exclude_inherited);
                        --  append the current result to the global one
                        Contained_For_Seq.Append
                          (Contained_For_Seq.Sequence (Result),
                           Contained_For_Seq.Sequence (Res));
                     end;
                  end loop;
               end;
            when dk_Value =>
               --  check the supported interfaces
               declare
                  IntDefSeq : InterfaceDefSeq
                    := ValueDef.Impl.get_supported_interfaces
                    (ValueDef.Impl.Object_Ptr (Get_Real_Object (Self)));
                  --  create the array of the supported interfaces
                  Int_Array : IdF.Element_Array
                    := IdF.To_Element_Array (IdF.Sequence (IntDefSeq));
               begin
                  for I in Int_Array'Range loop
                     declare
                       Int : constant InterfaceDef.Impl.Object_Ptr
                         := InterfaceDef.Impl.To_Object (Int_Array (I));
                       Res : ContainedSeq;
                     begin
                        --  we will get all the definition of the inherited interface
                        Res := lookup_name (Object_Ptr (Int),
                                            search_name,
                                            -1,
                                            limit_type,
                                            exclude_inherited);
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
                    := ValueDef.Impl.get_abstract_base_values
                    (ValueDef.Impl.Object_Ptr (Get_Real_Object (Self)));
                  --  create the array of the supported values
                  Val_Array : VDF.Element_Array
                    := VDF.To_Element_Array (VDF.Sequence (ValDefSeq));
               begin
                  for I in Val_Array'Range loop
                     declare
                        Val : constant ValueDef.Impl.Object_Ptr
                          := ValueDef.Impl.To_Object (Val_Array (I));
                        Res : ContainedSeq;
                     begin
                        --  we will get all the definition of the inherited value
                        Res := lookup_name (Object_Ptr (Val),
                                            search_name,
                                            -1,
                                            limit_type,
                                            exclude_inherited);
                        --  append the current result to the global one
                        Contained_For_Seq.Append
                          (Contained_For_Seq.Sequence (Result),
                           Contained_For_Seq.Sequence (Res));
                     end;
                  end loop;
               end;
               --  check the base_value
               declare
                  Obj : PortableServer.Servant;
                  Res : ContainedSeq;
               begin
                  PolyORB.CORBA_P.Server_Tools.Reference_To_Servant
                    (ValueDef.Impl.get_base_value
                     (ValueDef.Impl.Object_Ptr (Get_Real_Object (Self))),
                     Obj);
                  --  we will get all the definition of the inherited value
                  Res := lookup_name (Object_Ptr (Obj),
                                      search_name,
                                      -1,
                                      limit_type,
                                      exclude_inherited);
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
      if (levels_to_search > 0) and
        get_def_kind (Self) /= dk_Repository then
         declare
            New_Level : Long;
            Parent : constant Object_Ptr
              := To_Object
              (Contained.Impl.get_defined_in
               (Contained.Impl.To_Contained
                (Get_Real_Object (Self))));
            Res : ContainedSeq;
         begin
            if levels_to_search = 1 then
               New_Level := -1;
            else
               New_Level := levels_to_search - 1;
            end if;
            Res := lookup_name (Parent,
                                search_name,
                                New_Level,
                                limit_type,
                                exclude_inherited);
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
      limit_type : CORBA.Repository_Root.DefinitionKind;
      exclude_inherited : CORBA.Boolean;
      max_returned_objs : CORBA.Long)
     return CORBA.Repository_Root.Container.DescriptionSeq
   is
      Content : Contained.Impl.Contained_Seq.Sequence;
      package CD renames IDL_SEQUENCE_CORBA_Container_Description;
      Result : DescriptionSeq := DescriptionSeq (CD.Null_Sequence);
      use Contained.Impl;
   begin
      --  get the contents of the container
      Content := Contained.Impl.To_Contained_Sequence
        (contents (Self,
                   limit_type,
                   exclude_inherited));

      --  reduce it to max_returned_objs
      if max_returned_objs > 0 then
         if CORBA.Long (Contained_Seq.Length (Content)) > max_returned_objs then
            Contained_Seq.Head (Content,
                                Natural (max_returned_objs),
                                null);
         end if;
      end if;

      --  get the description and populate the result.
      declare
         Cont_Array : constant Contained_Seq.Element_Array
           := Contained_Seq.To_Element_Array (Content);
         Des : Contained.Description;
         Ref : Contained.Ref;
         Res_Des : Description;
      begin
         for I in Cont_Array'Range loop
            Des := Contained.Impl.describe (Cont_Array (I));

            --  get a reference of the object
            Ref := Contained.Convert_Forward.To_Ref
              (Contained.Impl.To_Forward (Cont_Array (I)));

            --  Create the container.description ...
            Res_Des := (Contained_Object => Ref,
                        kind => Des.kind,
                        value => Des.value);
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
      id : CORBA.RepositoryId;
      name : CORBA.Identifier;
      version : CORBA.Repository_Root.VersionSpec)
     return CORBA.Repository_Root.ModuleDef_Forward.Ref
   is
   begin
      pragma Debug (O2 ("Create_Module (container)"));
      --  is the new structure allowed?
      Check_Structure (Self, dk_Module);
      Check_Id (Self, id);
      Check_Name (Self, name);

      declare
         Result   : ModuleDef_Forward.Ref;
         Obj      : constant ModuleDef.Impl.Object_Ptr
           := new ModuleDef.Impl.Object;
         Cont_Obj : constant Contained.Impl.Object_Ptr
           := new Contained.Impl.Object;
      begin

         --  initialization of the object
         pragma Debug (O ("before_init (create_module)"));
         ModuleDef.Impl.Init (Obj,
                              IRObject.Impl.Object_Ptr (Obj),
                              dk_Module,
                              id,
                              name,
                              version,
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
      id : CORBA.RepositoryId;
      name : CORBA.Identifier;
      version : CORBA.Repository_Root.VersionSpec;
      IDL_type : CORBA.Repository_Root.IDLType_Forward.Ref;
      value : CORBA.Any)
      return CORBA.Repository_Root.ConstantDef_Forward.Ref
   is
   begin
      --  is the new structure allowed?
      Check_Structure (Self, dk_Constant);
      Check_Id (Self, id);
      Check_Name (Self, name);

      declare
         Obj : constant ConstantDef.Impl.Object_Ptr := new ConstantDef.Impl.Object;
      begin
         --  initialization of the object
         ConstantDef.Impl.Init (Obj,
                                IRObject.Impl.Object_Ptr (Obj),
                                dk_Constant,
                                id,
                                name,
                                version,
                                To_Forward (Object_Ptr (Self)),
                                IDLType.Convert_Forward.To_Ref (IDL_type),
                                value);
         --  add it to the contents field of this container
         Append_To_Contents
           (Self,
            Contained.Impl.To_Contained (IRObject.Impl.Object_Ptr (Obj)));
         return ConstantDef.Impl.To_Forward (Obj);
      end;
   end create_constant;

   function create_struct
     (Self : access Object;
      id : CORBA.RepositoryId;
      name : CORBA.Identifier;
      version : CORBA.Repository_Root.VersionSpec;
      members : CORBA.Repository_Root.StructMemberSeq)
      return CORBA.Repository_Root.StructDef_Forward.Ref
   is
   begin
      --  is the new structure allowed?
      Check_Structure (Self, dk_Struct);
      Check_Id (Self, id);
      Check_Name (Self, name);

      declare
         Obj : constant StructDef.Impl.Object_Ptr := new StructDef.Impl.Object;
         Container_Obj : constant Object_Ptr := new Object;
         IDLType_Obj : constant IDLType.Impl.Object_Ptr := new IDLType.Impl.Object;
      begin
         --  initialization of the object
         StructDef.Impl.Init (Obj,
                              IRObject.Impl.Object_Ptr (Obj),
                              dk_Struct,
                              id,
                              name,
                              version,
                              To_Forward (Object_Ptr (Self)),
                              IDLType_Obj,
                              Contained.Impl.Contained_Seq.Null_Sequence,
                              Container_Obj,
                              members);

         --  add it to the contents field of this container
         Append_To_Contents
           (Self,
            Contained.Impl.To_Contained (IRObject.Impl.Object_Ptr (Obj)));

         return StructDef.Impl.To_Forward (Obj);
      end;
   end create_struct;

   function create_union
     (Self : access Object;
      id : CORBA.RepositoryId;
      name : CORBA.Identifier;
      version : CORBA.Repository_Root.VersionSpec;
      discriminator_type : CORBA.Repository_Root.IDLType_Forward.Ref;
      members : CORBA.Repository_Root.UnionMemberSeq)
     return CORBA.Repository_Root.UnionDef_Forward.Ref
   is
   begin
      --  is the new structure allowed?
      Check_Structure (Self, dk_Union);
      Check_Id (Self, id);
      Check_Name (Self, name);

      declare
         Obj : constant UnionDef.Impl.Object_Ptr := new UnionDef.Impl.Object;
         Container_Obj : constant Object_Ptr := new Object;
         IDLType_Obj : constant IDLType.Impl.Object_Ptr := new IDLType.Impl.Object;
      begin
         --  initialization of the object
         UnionDef.Impl.Init (Obj,
                             IRObject.Impl.Object_Ptr (Obj),
                             dk_Union,
                             id,
                             name,
                             version,
                             To_Forward (Object_Ptr (Self)),
                             IDLType_Obj,
                             Contained.Impl.Contained_Seq.Null_Sequence,
                             Container_Obj,
                             IDLType.Convert_Forward.To_Ref
                               (discriminator_type),
                             members);

         --  add it to the contents field of this container
         Append_To_Contents
           (Self,
            Contained.Impl.To_Contained (IRObject.Impl.Object_Ptr (Obj)));
         return UnionDef.Impl.To_Forward (Obj);
      end;
   end create_union;

   function create_enum
     (Self : access Object;
      id : CORBA.RepositoryId;
      name : CORBA.Identifier;
      version : CORBA.Repository_Root.VersionSpec;
      members : CORBA.Repository_Root.EnumMemberSeq)
     return CORBA.Repository_Root.EnumDef_Forward.Ref
   is
   begin
      --  is the new structure allowed?
      Check_Structure (Self, dk_Enum);
      Check_Id (Self, id);
      Check_Name (Self, name);

      declare
         Obj : constant EnumDef.Impl.Object_Ptr := new EnumDef.Impl.Object;
         IDLType_Obj : constant IDLType.Impl.Object_Ptr := new IDLType.Impl.Object;
      begin
         --  initialization of the object
         EnumDef.Impl.Init (Obj,
                            IRObject.Impl.Object_Ptr (Obj),
                            dk_Enum,
                            id,
                            name,
                            version,
                            To_Forward (Object_Ptr (Self)),
                            IDLType_Obj,
                            members);

         --  add it to the contents field of this container
         Append_To_Contents
           (Self,
            Contained.Impl.To_Contained (IRObject.Impl.Object_Ptr (Obj)));
         return EnumDef.Impl.To_Forward (Obj);
      end;
   end create_enum;

   function create_alias
     (Self : access Object;
      id : CORBA.RepositoryId;
      name : CORBA.Identifier;
      version : CORBA.Repository_Root.VersionSpec;
      original_type : CORBA.Repository_Root.IDLType_Forward.Ref)
     return CORBA.Repository_Root.AliasDef_Forward.Ref
   is
   begin
      --  is the new structure allowed?
      Check_Structure (Self, dk_Alias);
      Check_Id (Self, id);
      Check_Name (Self, name);

      declare
         Obj : constant AliasDef.Impl.Object_Ptr := new AliasDef.Impl.Object;
         IDLType_Obj : constant IDLType.Impl.Object_Ptr := new IDLType.Impl.Object;
      begin
         --  initialization of the object
         AliasDef.Impl.Init (Obj,
                             IRObject.Impl.Object_Ptr (Obj),
                             dk_Alias,
                             id,
                             name,
                             version,                             To_Forward (Object_Ptr (Self)),
                             IDLType_Obj,
                             IDLType.Convert_Forward.To_Ref (original_type));

         --  add it to the contents field of this container
         Append_To_Contents
           (Self,
            Contained.Impl.To_Contained (IRObject.Impl.Object_Ptr (Obj)));

         return AliasDef.Impl.To_Forward (Obj);
      end;
   end create_alias;

   ----------------------
   -- create_interface --
   ----------------------

   function create_interface
     (Self            : access Object;
      id              : RepositoryId;
      name            : Identifier;
      version         : VersionSpec;
      base_interfaces : InterfaceDefSeq;
      is_abstract     : CORBA.Boolean)
      return InterfaceDef_Forward.Ref
   is
   begin
      pragma Debug (O2 ("Create_interface (container)"));

      --  Is the new structure allowed?

      Check_Structure (Self, dk_Interface);
      Check_Id (Self, id);
      Check_Name (Self, name);

      declare
         Obj         : constant ExtInterfaceDef.Impl.Object_Ptr
           := new ExtInterfaceDef.Impl.Object;
         Cont_Obj    : constant Contained.Impl.Object_Ptr
           := new Contained.Impl.Object;
         IDLType_Obj : constant IDLType.Impl.Object_Ptr
           := new IDLType.Impl.Object;
         InterfaceAttrExtension_Obj : constant
           InterfaceAttrExtension.Impl.Object_Ptr
           := new InterfaceAttrExtension.Impl.Object;
      begin
         pragma Debug (O ("Create_interface : before init"));
         --  initialization of the object
         ExtInterfaceDef.Impl.Internals.Init
           (Obj,
            IRObject.Impl.Object_Ptr (Obj),
            dk_Interface,
            id,
            name,
            version,
            To_Forward (Object_Ptr (Self)),
            Contained.Impl.Contained_Seq.Null_Sequence,
            Cont_Obj,
            IDLType_Obj,
            base_interfaces,
            is_abstract,
            InterfaceAttrExtension_Obj);
         pragma Debug (O ("Create_interface : before append"));
         --  add it to the contents field of this container
         Append_To_Contents
           (Self,
            Contained.Impl.To_Contained (IRObject.Impl.Object_Ptr (Obj)));

         return
           InterfaceDef.Impl.To_Forward (InterfaceDef.Impl.Object_Ptr (Obj));
      end;
   end create_interface;

   function create_value
     (Self : access Object;
      id : CORBA.RepositoryId;
      name : CORBA.Identifier;
      version : CORBA.Repository_Root.VersionSpec;
      is_custom : CORBA.Boolean;
      is_abstract : CORBA.Boolean;
      base_value : CORBA.Repository_Root.ValueDef_Forward.Ref;
      is_truncatable : CORBA.Boolean;
      abstract_base_values : CORBA.Repository_Root.ValueDefSeq;
      supported_interfaces : CORBA.Repository_Root.InterfaceDefSeq;
      initializers : CORBA.Repository_Root.InitializerSeq)
     return CORBA.Repository_Root.ValueDef_Forward.Ref
   is
   begin
      --  is the new structure allowed?
      Check_Structure (Self, dk_Value);
      Check_Id (Self, id);
      Check_Name (Self, name);

      declare
         Obj : constant ValueDef.Impl.Object_Ptr := new ValueDef.Impl.Object;
         Cont_Obj : constant Contained.Impl.Object_Ptr := new Contained.Impl.Object;
         IDLType_Obj : constant IDLType.Impl.Object_Ptr := new IDLType.Impl.Object;
      begin
         --  there cannot be more then one "true" in those boolean!
         if (is_custom and is_abstract) or
           (is_custom and is_truncatable) or
           (is_abstract and is_truncatable) then
            --  Spec is not precise...
            CORBA.Raise_Bad_Param (CORBA.System_Exception_Members'
                                   (Minor => 2,
                                    Completed => CORBA.Completed_No));
         end if;

         --  initialization of the object
         ValueDef.Impl.Init (Obj,
                             IRObject.Impl.Object_Ptr (Obj),
                             dk_Value,
                             id,
                             name,
                             version,
                             To_Forward (Object_Ptr (Self)),
                             Contained.Impl.Contained_Seq.Null_Sequence,
                             Cont_Obj,
                             IDLType_Obj,
                             supported_interfaces,
                             initializers,
                             ValueDef.Convert_Forward.To_Ref (base_value),
                             abstract_base_values,
                             is_abstract,
                             is_custom,
                             is_truncatable);
         --  add it to the contents field of this container
         Append_To_Contents
           (Self,
            Contained.Impl.To_Contained (IRObject.Impl.Object_Ptr (Obj)));

         return ValueDef.Impl.To_Forward (Obj);
      end;
   end create_value;

   function create_value_box
     (Self : access Object;
      id : CORBA.RepositoryId;
      name : CORBA.Identifier;
      version : CORBA.Repository_Root.VersionSpec;
      original_type_def : CORBA.Repository_Root.IDLType_Forward.Ref)
     return CORBA.Repository_Root.ValueBoxDef_Forward.Ref
   is
   begin
      --  is the new structure allowed?
      Check_Structure (Self, dk_ValueBox);
      Check_Id (Self, id);
      Check_Name (Self, name);

      declare
         Obj : constant ValueBoxDef.Impl.Object_Ptr := new ValueBoxDef.Impl.Object;
         IDLType_Obj : constant IDLType.Impl.Object_Ptr := new IDLType.Impl.Object;
      begin
         --  initialization of the object
         ValueBoxDef.Impl.Init (Obj,
                                IRObject.Impl.Object_Ptr (Obj),
                                dk_ValueBox,
                                id,
                                name,
                                version,
                                To_Forward (Object_Ptr (Self)),
                                IDLType_Obj,
                                IDLType.Convert_Forward.To_Ref
                                  (original_type_def));

         --  add it to the contents field of this container
         Append_To_Contents
           (Self,
            Contained.Impl.To_Contained (IRObject.Impl.Object_Ptr (Obj)));

         return ValueBoxDef.Impl.To_Forward (Obj);
      end;
   end create_value_box;

   function create_exception
     (Self : access Object;
      id : CORBA.RepositoryId;
      name : CORBA.Identifier;
      version : CORBA.Repository_Root.VersionSpec;
      members : CORBA.Repository_Root.StructMemberSeq)
     return CORBA.Repository_Root.ExceptionDef_Forward.Ref
   is
   begin
      --  is the new structure allowed?
      Check_Structure (Self, dk_Exception);
      Check_Id (Self, id);
      Check_Name (Self, name);

      declare
         Obj : constant ExceptionDef.Impl.Object_Ptr := new ExceptionDef.Impl.Object;
         Cont_Obj : constant Contained.Impl.Object_Ptr := new Contained.Impl.Object;
      begin
         --  initialization of the object
         ExceptionDef.Impl.Init (Obj,
                                 IRObject.Impl.Object_Ptr (Obj),
                                 dk_Exception,
                                 id,
                                 name,
                                 version,
                                 To_Forward (Object_Ptr (Self)),
                                 Contained.Impl.Contained_Seq.Null_Sequence,
                                 Cont_Obj,
                                 members);
         --  add it to the contents field of this container
         Append_To_Contents
           (Self,
            Contained.Impl.To_Contained (IRObject.Impl.Object_Ptr (Obj)));

         return ExceptionDef.Impl.To_Forward (Obj);
      end;
   end create_exception;

   function create_native
     (Self : access Object;
      id : CORBA.RepositoryId;
      name : CORBA.Identifier;
      version : CORBA.Repository_Root.VersionSpec)
     return CORBA.Repository_Root.NativeDef_Forward.Ref
   is
   begin
      --  is the new structure allowed?
      Check_Structure (Self, dk_Native);
      Check_Id (Self, id);
      Check_Name (Self, name);

      declare
         Obj : constant NativeDef.Impl.Object_Ptr := new NativeDef.Impl.Object;
         IDLType_Obj : constant IDLType.Impl.Object_Ptr := new IDLType.Impl.Object;
      begin
         --  initialization of the object
         NativeDef.Impl.Init (Obj,
                              IRObject.Impl.Object_Ptr (Obj),
                              dk_Native,
                              id,
                              name,
                              version,
                              To_Forward (Object_Ptr (Self)),
                              IDLType_Obj);

         --  add it to the contents field of this container
         Append_To_Contents
           (Self,
            Contained.Impl.To_Contained (IRObject.Impl.Object_Ptr (Obj)));

         return NativeDef.Impl.To_Forward (Obj);
      end;
   end create_native;

   -------------------------------
   -- create_abstract_interface --
   -------------------------------

   function create_abstract_interface
     (Self            : access Object;
      id              : RepositoryId;
      name            : Identifier;
      version         : VersionSpec;
      base_interfaces : AbstractInterfaceDefSeq)
      return AbstractInterfaceDef_Forward.Ref
   is
   begin
      --  Is the new structure allowed?

      Check_Structure (Self, dk_AbstractInterface);
      Check_Id (Self, id);
      Check_Name (Self, name);

      declare
         Ref            : AbstractInterfaceDef.Ref;
         Obj            : constant ExtAbstractInterfaceDef.Impl.Object_Ptr
           := new ExtAbstractInterfaceDef.Impl.Object;
         Cont_Obj       : constant Contained.Impl.Object_Ptr
           := new Contained.Impl.Object;
         IDLType_Obj    : constant IDLType.Impl.Object_Ptr
           := new IDLType.Impl.Object;
         IntAttrExt_Obj : constant InterfaceAttrExtension.Impl.Object_Ptr
           := new InterfaceAttrExtension.Impl.Object;
      begin
         --  Initialization and activation of the servant

         ExtAbstractInterfaceDef.Impl.Internals.Init
           (Obj,
            IRObject.Impl.Object_Ptr (Obj),
            dk_Interface,
            id,
            name,
            version,
            To_Forward (Object_Ptr (Self)),
            Contained.Impl.Contained_Seq.Null_Sequence,
            Cont_Obj,
            IDLType_Obj,
            base_interfaces,
            IntAttrExt_Obj);
         PolyORB.CORBA_P.Server_Tools.Initiate_Servant
           (PortableServer.Servant (Obj), Ref);

         --  Add it to the contents field of this container

         Append_To_Contents
           (Self,
            Contained.Impl.To_Contained (IRObject.Impl.Object_Ptr (Obj)));

         return AbstractInterfaceDef.Convert_Forward.To_Forward (Ref);
      end;
   end create_abstract_interface;

   ----------------------------
   -- create_local_interface --
   ----------------------------

   function create_local_interface
     (Self            : access Object;
      id              : RepositoryId;
      name            : Identifier;
      version         : VersionSpec;
      base_interfaces : InterfaceDefSeq)
      return LocalInterfaceDef_Forward.Ref
   is
   begin
      --  Is the new structure allowed?

      Check_Structure (Self, dk_LocalInterface);
      Check_Id (Self, id);
      Check_Name (Self, name);

      declare
         Ref            : LocalInterfaceDef.Ref;
         Obj            : constant ExtLocalInterfaceDef.Impl.Object_Ptr
           := new ExtLocalInterfaceDef.Impl.Object;
         Cont_Obj       : constant Contained.Impl.Object_Ptr
           := new Contained.Impl.Object;
         IDLType_Obj    : constant IDLType.Impl.Object_Ptr
           := new IDLType.Impl.Object;
         IntAttrExt_Obj : constant InterfaceAttrExtension.Impl.Object_Ptr
           := new InterfaceAttrExtension.Impl.Object;
      begin
         --  Initialization and activation of the servant

         ExtLocalInterfaceDef.Impl.Internals.Init
           (Obj,
            IRObject.Impl.Object_Ptr (Obj),
            dk_Interface,
            id,
            name,
            version,
            To_Forward (Object_Ptr (Self)),
            Contained.Impl.Contained_Seq.Null_Sequence,
            Cont_Obj,
            IDLType_Obj,
            base_interfaces,
            IntAttrExt_Obj);
         PolyORB.CORBA_P.Server_Tools.Initiate_Servant
           (PortableServer.Servant (Obj), Ref);

         --  Add it to the contents field of this container

         Append_To_Contents
           (Self,
            Contained.Impl.To_Contained (IRObject.Impl.Object_Ptr (Obj)));

         return LocalInterfaceDef.Convert_Forward.To_Forward (Ref);
      end;
   end create_local_interface;

end CORBA.Repository_Root.Container.Impl;
