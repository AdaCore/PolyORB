----------------------------------------------
--  This file has been generated automatically
--  by AdaBroker (http://adabroker.eu.org/)
----------------------------------------------

with Ada.Text_IO;
with Ada.Unchecked_Deallocation;
with Ada.Strings.Unbounded;

with Corba.Repository_Root; use Corba.Repository_Root;
with CORBA.Repository_Root.Contained.Skel;
with CORBA.Repository_Root.Contained.Helper;
with CORBA.Repository_Root.Container.Impl;
with CORBA.Repository_Root.Exceptiondef.Impl;
with CORBA.Repository_Root.Interfacedef.Impl;
with CORBA.Repository_Root.Valuedef.Impl;
with CORBA.Repository_Root.Moduledef.Impl;
with CORBA.Repository_Root.UnionDef.Impl;
with CORBA.Repository_Root.StructDef.Impl;
with CORBA.Repository_Root.IRObject.Impl;
with CORBA.Repository_Root.Repository.Impl;

with CORBA.Repository_Root.ConstantDef;
with CORBA.Repository_Root.AttributeDef;
with CORBA.Repository_Root.OperationDef;
with CORBA.Repository_Root.ValueMemberDef;
with CORBA.Repository_Root.EnumDef;
with CORBA.Repository_Root.AliasDef;
with CORBA.Repository_Root.NativeDef;
with CORBA.Repository_Root.ValueBoxDef;
with CORBA.Repository_Root.ModuleDef;
with CORBA.Repository_Root.ExceptionDef;
with CORBA.Repository_Root.InterfaceDef;
with CORBA.Repository_Root.ValueDef;
with CORBA.Repository_Root.StructDef;
with CORBA.Repository_Root.UnionDef;

with Broca.Exceptions;
with Broca.Debug;
with Broca.Repository;
with Broca.Server_Tools;
with PortableServer;
with Sequences;


package body CORBA.Repository_Root.Contained.Impl is

   Package Contained_For_Seq renames IDL_SEQUENCE_CORBA_Repository_Root_Contained_Forward;

   -----------
   -- Debug --
   -----------

   Flag : constant Natural
     := Broca.Debug.Is_Active ("contained.impl");
   procedure O is new Broca.Debug.Output (Flag);

   Flag2 : constant Natural
     := Broca.Debug.Is_Active ("contained.impl_method_trace");
   procedure O2 is new Broca.Debug.Output (Flag2);

   ----------------------
   --  Procedure init  --
   ----------------------
   procedure Init (Self : access Object;
                   Real_Object : IRObject.Impl.Object_Ptr;
                   Def_Kind : Corba.Repository_Root.DefinitionKind;
                   Id : CORBA.RepositoryId;
                   Name : CORBA.Identifier;
                   Version : CORBA.Repository_Root.VersionSpec;
                   Defined_In : CORBA.Repository_Root.Container_Forward.Ref) is
   begin
      pragma Debug (O2 ("init (contained) enter"));
      IRObject.Impl.Init (IRObject.Impl.Object_Ptr (Self), Real_Object, Def_Kind);
      Self.Id := Id;
      Self.Name := Name;
      Self.Version := Version;
      Self.Defined_In := Defined_In;
      pragma Debug (O2 ("init (contained) end"));
   end Init;


   -----------------
   --  To_Object  --
   -----------------
   function To_Object (Fw_Ref : Contained_Forward.Ref)
                       return Object_Ptr is
      Result : Portableserver.Servant;
      The_Ref : Contained.Ref;
   begin
      pragma Debug (O2 ("to_object (contained)"));
      The_Ref := Contained.Convert_Forward.To_Ref (Fw_Ref);
      pragma Debug (O2 ("to_object, before object_of"));
      Broca.Server_Tools.Reference_To_Servant (The_Ref,
                                               Result);
      pragma Debug (O ("to_object end;"));
      return To_Contained (IROBject.Impl.Object_Ptr (Result));
   end To_Object;

   ------------------
   --  To_Forward  --
   ------------------
   function To_Forward (Obj : Object_Ptr)
                        return Contained_Forward.Ref is
      Result : Contained_Forward.Ref;
   begin
      case Get_Def_Kind (Obj) is
         when
           Dk_Primitive  |
           Dk_String     |
           Dk_Sequence   |
           Dk_Array      |
           Dk_Wstring    |
           Dk_Fixed      |
           Dk_Repository |
           Dk_All        |
           Dk_Typedef    |
           Dk_None       =>
            Broca.Exceptions.Raise_Internal;
            return Result;
         when
           Dk_Interface  =>
            declare
               The_Ref : InterfaceDef.Ref;
            begin
               Broca.Server_Tools.Initiate_Servant (PortableServer.Servant
                                                    (Get_Real_Object (Obj)),
                                                    The_Ref);
               return Contained.Convert_Forward.To_Forward
                 (Contained.Helper.To_Ref (The_Ref));
            end;
         when
           Dk_Value      =>
            declare
               The_Ref : ValueDef.Ref;
            begin
               Broca.Server_Tools.Initiate_Servant (PortableServer.Servant
                                                    (Get_Real_Object (Obj)),
                                                    The_Ref);
               return Contained.Convert_Forward.To_Forward
                 (Contained.Helper.To_Ref (The_Ref));
            end;
         when  Dk_Struct     =>
            declare
               The_Ref : StructDef.Ref;
            begin
               Broca.Server_Tools.Initiate_Servant (PortableServer.Servant
                                                    (Get_Real_Object (Obj)),
                                                    The_Ref);
               return Contained.Convert_Forward.To_Forward
                 (Contained.Helper.To_Ref (The_Ref));
            end;
         when  Dk_Union      =>
            declare
               The_Ref : UnionDef.Ref;
            begin
               Broca.Server_Tools.Initiate_Servant (PortableServer.Servant
                                                    (Get_Real_Object (Obj)),
                                                    The_Ref);
               return Contained.Convert_Forward.To_Forward
                 (Contained.Helper.To_Ref (The_Ref));
            end;
         when  Dk_Enum      =>
            declare
               The_Ref : EnumDef.Ref;
            begin
               Broca.Server_Tools.Initiate_Servant (PortableServer.Servant
                                                    (Get_Real_Object (Obj)),
                                                    The_Ref);
               return Contained.Convert_Forward.To_Forward
                 (Contained.Helper.To_Ref (The_Ref));
            end;
         when  Dk_Alias      =>
            declare
               The_Ref : AliasDef.Ref;
            begin
               Broca.Server_Tools.Initiate_Servant (PortableServer.Servant
                                                    (Get_Real_Object (Obj)),
                                                    The_Ref);
               return Contained.Convert_Forward.To_Forward
                 (Contained.Helper.To_Ref (The_Ref));
            end;
         when  Dk_Native      =>
            declare
               The_Ref : NativeDef.Ref;
            begin
               Broca.Server_Tools.Initiate_Servant (PortableServer.Servant
                                                    (Get_Real_Object (Obj)),
                                                    The_Ref);
               return Contained.Convert_Forward.To_Forward
                 (Contained.Helper.To_Ref (The_Ref));
            end;
         when  Dk_ValueBox      =>
            declare
               The_Ref : ValueBoxDef.Ref;
            begin
               Broca.Server_Tools.Initiate_Servant (PortableServer.Servant
                                                    (Get_Real_Object (Obj)),
                                                    The_Ref);
               return Contained.Convert_Forward.To_Forward
                 (Contained.Helper.To_Ref (The_Ref));
            end;
         when Dk_Exception  =>
            declare
               The_Ref : ExceptionDef.Ref;
            begin
               Broca.Server_Tools.Initiate_Servant (PortableServer.Servant
                                                    (Get_Real_Object (Obj)),
                                                    The_Ref);
               return Contained.Convert_Forward.To_Forward
                 (Contained.Helper.To_Ref (The_Ref));
            end;
         when
           Dk_Module     =>
            declare
               The_Ref : ModuleDef.Ref;
            begin
               Broca.Server_Tools.Initiate_Servant (PortableServer.Servant
                                                    (Get_Real_Object (Obj)),
                                                    The_Ref);
               return Contained.Convert_Forward.To_Forward
                 (Contained.Helper.To_Ref (The_Ref));
            end;
         when
           Dk_Attribute     =>
            declare
               The_Ref : AttributeDef.Ref;
            begin
               Broca.Server_Tools.Initiate_Servant (PortableServer.Servant
                                                    (Get_Real_Object (Obj)),
                                                    The_Ref);
               return Contained.Convert_Forward.To_Forward
                 (Contained.Helper.To_Ref (The_Ref));
            end;
         when
           Dk_Constant     =>
            declare
               The_Ref : ConstantDef.Ref;
            begin
               Broca.Server_Tools.Initiate_Servant (PortableServer.Servant
                                                    (Get_Real_Object (Obj)),
                                                    The_Ref);
               return Contained.Convert_Forward.To_Forward
                 (Contained.Helper.To_Ref (The_Ref));
            end;
         when
           Dk_Operation     =>
            declare
               The_Ref : OperationDef.Ref;
            begin
               Broca.Server_Tools.Initiate_Servant (PortableServer.Servant
                                                    (Get_Real_Object (Obj)),
                                                    The_Ref);
               return Contained.Convert_Forward.To_Forward
                 (Contained.Helper.To_Ref (The_Ref));
            end;
         when
           Dk_ValueMember     =>
            declare
               The_Ref : ValueMemberDef.Ref;
            begin
               Broca.Server_Tools.Initiate_Servant (PortableServer.Servant
                                                    (Get_Real_Object (Obj)),
                                                    The_Ref);
               return Contained.Convert_Forward.To_Forward
                 (Contained.Helper.To_Ref (The_Ref));
            end;

      end case;
   end To_Forward;

   --------------------
   --  To_Contained  --
   --------------------
   procedure To_Contained
     (Self : IRObject.Impl.Object_Ptr;
      Success : out Boolean;
      Result : out Object_ptr)
   is
   begin
      pragma Debug (O2 ("to_contained (contained)"));
      Success := True;
      case IRObject.Impl.Get_Def_Kind
        (Self) is
         when
           Dk_Repository |
           Dk_Primitive  |
           Dk_String     |
           Dk_Sequence   |
           Dk_Array      |
           Dk_Wstring    |
           Dk_Fixed      |
           Dk_All        |
           Dk_None       =>
            Success := False;
            Result := null;
         when
           --  inherited types
           Dk_Attribute  |
           Dk_Constant   |
           Dk_Operation  |
           Dk_Typedef    |
           Dk_Alias      |
           Dk_Struct     |
           Dk_Union      |
           Dk_Enum       |
           Dk_ValueBox   |
           dk_ValueMember|
           dk_Native =>
            Result := Object_Ptr (Self);
              -- types containing a "contained_view" field
         when
           Dk_Exception  =>
            declare
               Interm : Exceptiondef.Impl.Object_Ptr :=
                 Exceptiondef.Impl.Object_Ptr (Self);
            begin
               Result := Exceptiondef.Impl.Get_Contained_View (Interm);
            end;
         when
           Dk_Module     =>
            declare
               Interm : Moduledef.Impl.Object_Ptr :=
                 Moduledef.Impl.Object_Ptr (Self);
            begin
               Result := Moduledef.Impl.Get_Contained_View (Interm);
            end;
         when
           Dk_Value      =>
            declare
               Interm : Valuedef.Impl.Object_Ptr :=
                 Valuedef.Impl.Object_Ptr (Self);
            begin
               Result := Valuedef.Impl.Get_Contained_View (Interm);
            end;
         when
           Dk_Interface  =>
            declare
               Interm : Interfacedef.Impl.Object_Ptr :=
                 Interfacedef.Impl.Object_Ptr (Self);
            begin
               Result := Interfacedef.Impl.Get_Contained_View (Interm);
            end;
      end case;
      return;
   end To_Contained;


   function To_Contained
     (Self : IRObject.Impl.Object_Ptr)
     return  Object_ptr
   is
   begin
      pragma Debug (O2 ("to_contained (contained)"));
      case IRObject.Impl.Get_Def_Kind
        (Self) is
         when
           Dk_Repository |
           Dk_Primitive  |
           Dk_String     |
           Dk_Sequence   |
           Dk_Array      |
           Dk_Wstring    |
           Dk_Fixed      |
           Dk_All        |
           Dk_None       =>
            Broca.Exceptions.Raise_Internal;
            return null;
         when
           --  inherited types
           Dk_Attribute  |
           Dk_Constant   |
           Dk_Operation  |
           Dk_Typedef    |
           Dk_Alias      |
           Dk_Struct     |
           Dk_Union      |
           Dk_Enum       |
           Dk_ValueBox   |
           dk_ValueMember|
           dk_Native =>
            return Object_Ptr (Self);
              -- types containing a "contained_view" field
         when
           Dk_Exception  =>
            declare
               Interm : Exceptiondef.Impl.Object_Ptr :=
                 Exceptiondef.Impl.Object_Ptr (Self);
            begin
               return Exceptiondef.Impl.Get_Contained_View (Interm);
            end;
         when
           Dk_Module     =>
            declare
               Interm : Moduledef.Impl.Object_Ptr :=
                 Moduledef.Impl.Object_Ptr (Self);
            begin
               return Moduledef.Impl.Get_Contained_View (Interm);
            end;
         when
           Dk_Value      =>
            declare
               Interm : Valuedef.Impl.Object_Ptr :=
                 Valuedef.Impl.Object_Ptr (Self);
            begin
               return Valuedef.Impl.Get_Contained_View (Interm);
            end;
         when
           Dk_Interface  =>
            declare
               Interm : Interfacedef.Impl.Object_Ptr :=
                 Interfacedef.Impl.Object_Ptr (Self);
            begin
               return Interfacedef.Impl.Get_Contained_View (Interm);
            end;
      end case;
   end To_Contained;


   -----------------------
   -- IR implementation --
   -----------------------

   function get_id
     (Self : access Object)
     return CORBA.RepositoryId
   is
   begin
      return Self.Id;
   end get_id;


   procedure set_id
     (Self : access Object;
      To : in CORBA.RepositoryId) is
   begin
      --  If the Id is already used, raise an exception.
      if Contained.Is_Nil
        (Repository.Impl.Lookup_Id
         --  Convert the ref to object (must cast).
         (Repository.Impl.To_Object (Get_Containing_Repository (Self)),
          To)) then
         Self.Id := To;
      else
         Broca.Exceptions.Raise_Bad_Param(2);
      end if;
   end set_id;


   function get_name
     (Self : access Object)
     return CORBA.Identifier
   is
   begin
      return Self.Name;
   end get_name;

   procedure set_name
     (Self : access Object;
      To : in CORBA.Identifier)
   is
      Other : ContainedSeq;
      use Contained_For_Seq;
   begin
      --  Must check if the name is not already used in this scope
      --  So we check all the nodes in this container with the same name
      Other := Container.Impl.Lookup_Name
        (Container.Impl.To_Object (Self.Defined_In),
         To,
         1,
         Dk_All,
         False);
      if Contained_For_Seq.Null_Sequence = (Contained_For_Seq.Sequence (Other))
      then
         Self.Name := To;
      else
         Broca.Exceptions.Raise_Bad_Param(1);
      end if;
   end set_name;


   function get_version
     (Self : access Object)
     return VersionSpec
   is
   begin
      return Self.Version;
   end get_version;


   procedure set_version
     (Self : access Object;
      To : in VersionSpec) is
   begin
      Self.Version := To;
   end set_version;


   ----------------------
   --  get_defined_in  --
   ----------------------
   function get_defined_in
     (Self : access Object)
     return Container_Forward.Ref
   is
   begin
      return Self.Defined_In;
   end get_defined_in;


   function get_defined_in
     (Self : access Object)
     return CORBA.RepositoryId
   is
      Cont : Container.Impl.Object_Ptr
        := Container.Impl.To_Object (Self.Defined_In);
      use Container.Impl;
   begin

      if Get_Def_Kind (Cont) =  Dk_Repository then
         return  CORBA.Null_RepositoryId;
      end if;

      return
        Get_Id (To_Contained (Get_Real_Object (Cont)));
   end Get_Defined_In;


   function get_absolute_name
     (Self : access Object)
      return CORBA.ScopedName
   is
      use Container.Impl;
   begin
      pragma Debug (O2 ("get_absolute_name enter"));
      if Container_Forward.Is_Nil (Self.Defined_In) then
         pragma Debug (O ("get_absolute_name defined_in is null;"));
         null;
      end if;
      if Get_Def_Kind (To_Object (Self.Defined_In)) = Dk_Repository then
         --  if we are in the repository, then just append "::" to the name...
         pragma Debug (O ("get_absolute_name : it's a repository"));
         return CORBA.ScopedName (CORBA.To_CORBA_String ("::")
                                  & CORBA.String (Self.Name));
      else
         declare
            Scope : Object_Ptr
              := To_Contained (Get_Real_Object (To_Object (Self.Defined_In)));
         begin
            pragma Debug (O2 ("get_absolute_name : it is not a repository"));
            -- ... else append "::" and the name to the previous absolute_name.
            return CORBA.ScopedName (CORBA.String (Get_Absolute_Name (Scope))
                                     & CORBA.To_CORBA_String ("::")
                                     & CORBA.String (Self.Name));
         end;
      end if;
   end get_absolute_name;

   function get_containing_repository
     (Self : access Object)
     return Repository_Forward.Ref
   is
      use Container.Impl;
   begin
      if Get_Def_Kind (To_Object (Self.Defined_In)) = Dk_Repository then
         --  the define_in is the repository
         return Repository.Impl.To_Forward
           (Repository.Impl.Object_Ptr (To_Object (Self.Defined_In)));
      else
         -- returns the repository of the parent
         return Get_Containing_Repository
           (To_Contained (Get_Real_Object (To_Object (Self.Defined_In))));
      end if;
   end get_containing_repository;


   ----------------
   --  describe  --
   ----------------
   function describe
     (Self : access Object)
     return Contained.Description
   is
      --  only returned when an exception occured
      Result : Contained.Description;
   begin
      case Get_Def_Kind (Self) is
         when
           --  not contained types
           Dk_Repository |
           Dk_Primitive  |
           Dk_String     |
           Dk_Sequence   |
           Dk_Array      |
           Dk_Wstring    |
           Dk_Fixed      |
           Dk_All        |
           Dk_None       =>
            Broca.Exceptions.Raise_Internal;
            return Result;
         when
           --  child objects
           Dk_Attribute  |
           Dk_Constant   |
           Dk_Operation  |
           Dk_Typedef    |
           Dk_Alias      |
           Dk_Struct     |
           Dk_Union      |
           Dk_Enum       |
           Dk_ValueBox   |
           Dk_ValueMember|
           dk_Native =>
            --  dispatching call...
            return Describe (Object_Ptr (Self));
         -- types containing a "contained_view" field
         when
           Dk_Exception  =>
            declare
               Interm : Exceptiondef.Impl.Object_Ptr :=
                 Exceptiondef.Impl.Object_Ptr (Get_Real_Object (Self));
            begin
               return Exceptiondef.Impl.Describe (Interm);
            end;
         when
           Dk_Module     =>
            declare
               Interm : Moduledef.Impl.Object_Ptr :=
                 Moduledef.Impl.Object_Ptr (Get_Real_Object (Self));
            begin
               return Moduledef.Impl.Describe (Interm);
            end;
         when
           Dk_Value      =>
            declare
               Interm : Valuedef.Impl.Object_Ptr :=
                 Valuedef.Impl.Object_Ptr (Get_Real_Object (Self));
            begin
               return Valuedef.Impl.Describe (Interm);
            end;
         when
           Dk_Interface  =>
            declare
               Interm : Interfacedef.Impl.Object_Ptr :=
                 Interfacedef.Impl.Object_Ptr (Get_Real_Object (Self));
            begin
               return Interfacedef.Impl.Describe (Interm);
            end;
      end case;
   end describe;

   ------------
   --  move  --
   ------------
   procedure move
     (Self : access Object;
      new_container : in Container_Forward.Ref;
      new_name : in CORBA.Identifier;
      new_version : in VersionSpec) is

      For_Container_Ptr : Container.Impl.Object_Ptr
        := Container.Impl.To_Object (Self.Defined_In);
      New_Container_Ptr : Container.Impl.Object_Ptr
        := Container.Impl.To_Object (New_Container);
      Rep1 : Repository.Impl.Object_Ptr
        := Repository.Impl.To_Object (Get_Containing_Repository (Self));
      Rep2 : Repository.Impl.Object_Ptr;
      Not_Allowed : Boolean := False;
      use Repository.Impl;
   begin
      if Container.Impl.Get_Def_Kind (New_Container_Ptr) = Dk_Repository then
         Rep2 := Repository.Impl.Object_Ptr (New_Container_Ptr);
      else
         Rep2 := Repository.Impl.To_Object
           (Get_Containing_Repository
            (To_Contained
             (Container.Impl.Get_Real_Object (New_Container_Ptr))));
      end if;
      -- It must be in the same Repository
      if Rep1 /= Rep2 then
         Broca.Exceptions.Raise_Bad_Param (Minor => 4);
         Not_Allowed := True;
      else

         --  the move should comply with p10-8 of the IR spec.
         --  (structure and navigation in the IR)
         --  it raises the bad_param if the structure is not correct
         if Container.Impl.Check_Structure (New_Container_Ptr,
                                            Get_Def_Kind (Self)) then
            --  check if the name is not already used in this scope.
            if Container.Impl.Check_Name (New_Container_Ptr,
                                          New_Name) then
               --  remove the contained from the previous container
               Container.Impl.Delete_From_Contents (For_Container_Ptr,
                                                    Object_Ptr (Self));
               --  we can move this contained to this container
               Self.Defined_In := New_Container;
               Self.Name := New_Name;
               Self.Version := New_Version;
               --  add the contained to the new container
               Container.Impl.Append_To_Contents (New_Container_Ptr,
                                                  Object_Ptr (Self));
            end if;
         end if;
      end if;
   end move;

   ------------------------
   -- A Seq of contained --
   ------------------------


   ------------------------------
   --  Simplify_Contained_Seq  --
   ------------------------------
   procedure Simplify_ContainedSeq (In_Seq : in out ContainedSeq)
   is
      Cont_Array : Contained_For_Seq.Element_Array
        := Contained_For_Seq.To_Element_Array
        (Contained_For_Seq.Sequence (In_Seq));
   begin
      for I in Cont_Array'Range loop
         for J in (I + 1) .. (Cont_Array'Last) loop
            if To_Object (Cont_Array (I)) = To_Object (Cont_Array (J)) then
               declare
                  Ind : Natural;
                  Del_Array : Contained_For_Seq.Element_Array (1 .. 1);
               begin
                  Del_Array (1) := Cont_Array (J);
                  Ind := Contained_For_Seq.Index
                    (Contained_For_Seq.Sequence (In_Seq),
                     Del_Array,
                     Sequences.Backward);
                  Contained_For_Seq.Delete (Contained_For_Seq.Sequence (In_Seq),
                                            Ind,
                                            Ind);
               end;
            end if;
         end loop;
      end loop;
   end;

   -----------------
   --  Lookup_id  --
   -----------------
   function Lookup_Id (In_Seq : Contained_Seq.Sequence;
                       Search_Id : CORBA.RepositoryId)
                       return Object_Ptr is
      Result : Object_Ptr := null;
      Success : Boolean;
      Container_Object : Container.Impl.Object_Ptr;
      Cont_Array : Contained_Seq.Element_Array
        := Contained_Seq.To_Element_Array (In_Seq);
      use Broca.Repository;
   begin

      for I in Cont_Array'Range loop
         exit when Result /= null;
         if Is_Equivalent (Cont_Array (I).Id, Search_Id) then
            Result := Cont_Array (I);
         else
            Container.Impl.To_Container (Get_Real_Object (Cont_Array (I)),
                                         Success,
                                         Container_Object);
            if Success then
               Result := Lookup_Id (Container.Impl.Get_Contents
                                    (Container_Object),
                                    Search_Id);
            end if;
         end if;
      end loop;
      return Result;
   end Lookup_Id;

   -------------------------
   -- Lookup_Scoped_Name --
   -------------------------
   function Lookup_ScopedName (In_Seq : Contained_Seq.Sequence;
                               Name : ScopedName) return Object_Ptr is
      use Ada.Strings.Unbounded;

      Result : Object_Ptr := null;
      Search : Unbounded_String := Unbounded_String (Name);
      Look : Unbounded_String;
      Ind : Natural;
   begin
      --  Should not begin with ::
      if Head (Search, 2) = "::" then
         Broca.Exceptions.Raise_Internal;
      end if;

      --  Calculate the Index of "::"
      Ind := Index (Search, "::");
      if Ind /= 0 then
         --  create the name to look at in the In_Seq
         Look := Head (Search, Ind - To_String (Search)'First);
         --  create the new search
         Tail (Search, Length (Search) - Length (Look) - 2);
      else
         --  create the name to look at in the In_Seq
         Look := Search;
      end if;

      declare
         Cont_Array : Contained_Seq.Element_Array
           := Contained_Seq.To_Element_Array (In_Seq);
      begin
         for I in Cont_Array'Range loop
            if Cont_Array (I).Name = Identifier (Look) then
               Result := Cont_Array (I);
               exit;
            end if;
         end loop;
      end;

      if Result = null then
         return null;
      end if;

      if Ind = 0 then
         --  we finally found the right object
         return Result;
      else
         --  the scopedName is no empty, we have to continue the query
         declare
            Success : Boolean;
            Obj : Container.Impl.Object_Ptr;
         begin
            --  What we found should be a container...
            Container.Impl.To_Container (Get_Real_Object (Result),
                                         Success,
                                         Obj);
            if not Success then
               return null;
            else
               return Lookup_ScopedName (Container.Impl.Get_Contents (Obj),
                                         ScopedName (Search));
            end if;
         end;
      end if;
   end Lookup_ScopedName;

   -----------------
   -- Lookup_Name --
   -----------------
   function Lookup_Name (In_Seq : Contained_Seq.Sequence;
                         Name : Identifier;
                         Limit_Type : DefinitionKind) return ContainedSeq is
      Result : Contained_Seq.Sequence := Contained_Seq.Null_Sequence;
      Cont_Array : Contained_Seq.Element_Array
        := Contained_Seq.To_Element_Array (In_Seq);
   begin
      for I in Cont_Array'Range loop
         if Cont_Array (I).Name = Name then
            --  if limit_type is dk_all or if we get the right limit_type...
            if (Limit_Type = Dk_All) or
              (Limit_Type = Get_Def_Kind (Cont_Array (I))) then
               --  ...we should append this contained to the list
               Contained_Seq.Append (Result, Cont_Array (I));
            end if;
         end if;
      end loop;
      return To_ContainedSeq (Result);
   end;

   ----------------
   --  Contents  --
   ----------------
   function Contents (In_Seq : Contained_Seq.Sequence;
                      Limit_Type : DefinitionKind) return ContainedSeq is
   begin
      if Limit_Type = Dk_All then
         return To_ContainedSeq (In_Seq);
      else
         --  we can only select the containeds with the right def_kind
         declare
            Result : Contained_Seq.Sequence := Contained_Seq.Null_Sequence;
            Cont_Array : Contained_Seq.Element_Array
              := Contained_Seq.To_Element_Array (In_Seq);
         begin
            for I in Cont_Array'Range loop
               --  if we get the right limit_type...
               if Limit_Type = Get_Def_Kind (Cont_Array (I)) then
                  --  ...we should append this contained to the list
                  Contained_Seq.Append (Result, Cont_Array (I));
               end if;
            end loop;
            return To_ContainedSeq (Result);
         end;
      end if;
   end;

   -----------------------
   --  To_ContainedSeq  --
   -----------------------
   function To_ContainedSeq (In_Seq : Contained_Seq.Sequence)
                             return  CORBA.Repository_Root.ContainedSeq is
      Cont_Array : Contained_Seq.Element_Array
        := Contained_Seq.To_Element_Array (In_Seq);
      The_Ref : CORBA.Repository_Root.Contained.Ref;
      Result : CORBA.Repository_Root.ContainedSeq
        := CORBA.Repository_Root.ContainedSeq (Contained_For_Seq.Null_Sequence);
   begin
      for I in Cont_Array'Range loop
         declare
            Cont : Object_Ptr := Cont_Array (I);
         begin
            Contained_For_Seq.Append (Contained_For_Seq.Sequence (Result),
                                      To_Forward (Cont));
         end;
      end loop;
      return Result;
   end To_ContainedSeq;

   -----------------------------
   --  To_Contained_Sequence  --
   -----------------------------
   function To_Contained_Sequence (In_Seq : ContainedSeq)
                                   return Contained_Seq.Sequence is
      Cont_Array : Contained_For_Seq.Element_Array
        := Contained_For_Seq.To_Element_Array
        (Contained_For_Seq.Sequence (In_Seq));
      Result : Contained_Seq.Sequence
        := Contained_Seq.Null_Sequence;
   begin
      for I in Cont_Array'Range loop
         Contained_Seq.Append (Result, To_Object (Cont_Array (I)));
      end loop;
      return Result;
   end To_Contained_Sequence;

   ---------------------
   --  Print_content  --
   ---------------------
   procedure Print_Content
     (In_Seq : Contained_Seq.Sequence;
      Inc : Standard.String)
   is
      use Ada.Text_IO;

      Cont_Array : constant Contained_Seq.Element_Array
        := Contained_Seq.To_Element_Array (In_Seq);

   begin
      for I in Cont_Array'Range loop
         declare
            Success : Boolean;
            Container_Object : Container.Impl.Object_Ptr;
         begin
            Put_Line (Inc & "Node : " &
                      DefinitionKind'Image
                      (Get_Def_Kind (Cont_Array (I))));
            Put_Line (Inc & "Name : " &
                      CORBA.To_Standard_String
                      (CORBA.String (Get_Name (Cont_Array (I)))));
            Put_Line (Inc & "Id : " &
                      CORBA.To_Standard_String
                      (CORBA.String (Get_Id (Cont_Array (I)))));
            Put_Line (Inc & "Vers : " &
                      CORBA.To_Standard_String
                      (CORBA.String (Get_Version (Cont_Array (I)))));
            Put_Line (Inc & "Abs-Name : " &
                      CORBA.To_Standard_String
                      (CORBA.String
                       (Get_Absolute_Name (Cont_Array (I)))));

            Container.Impl.To_Container (Get_Real_Object (Cont_Array (I)),
                                         Success,
                                         Container_Object);
            if Success then
               Print_Content (Container.Impl.Get_Contents
                              (Container_Object),
                              Inc & "     ");
            end if;
         end;

      end loop;

   end;


end CORBA.Repository_Root.Contained.Impl;








