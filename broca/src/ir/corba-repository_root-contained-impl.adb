----------------------------------------------
--  This file has been generated automatically
--  by AdaBroker (http://adabroker.eu.org/)
----------------------------------------------

with Ada.Unchecked_Deallocation;
with Ada.Strings.Unbounded;
with CORBA.AbstractBase;
with CORBA.Impl;

with Corba.Repository_Root; use Corba.Repository_Root;
with CORBA.Repository_Root.Contained.Skel;
with CORBA.Repository_Root.Container.Impl;
with CORBA.Repository_Root.Exceptiondef.Impl;
with CORBA.Repository_Root.Interfacedef.Impl;
with CORBA.Repository_Root.Valuedef.Impl;
with CORBA.Repository_Root.Moduledef.Impl;
with CORBA.Repository_Root.UnionDef.Impl;
with CORBA.Repository_Root.StructDef.Impl;
with CORBA.Repository_Root.IRObject.Impl;
with CORBA.Repository_Root.Repository.Impl;

with Broca.Exceptions;
with Broca.Debug;


package body CORBA.Repository_Root.Contained.Impl is


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
                   Defined_In : CORBA.Repository_Root.Container_Forward.Ref;
                   Absolute_Name : CORBA.ScopedName;
                   Containing_Repository :
                     CORBA.Repository_Root.Repository_Forward.Ref) is
   begin
      IRObject.Impl.Init (IRObject.Impl.Object_Ptr (Self), Real_Object, Def_Kind);
      Self.Id := Id;
      Self.Name := Name;
      Self.Version := Version;
      Self.Defined_In := Defined_In;
      Self.Absolute_Name := Absolute_Name;
      Self.Containing_Repository := Containing_Repository;
   end Init;



   --------------------------------------
   --  Procedure change_absolute_name  --
   --------------------------------------
   --  change the absolute name of the contained_object
   --  if its name or the parent has changed
   procedure Change_Absolute_Name (Contained_Object : Object_Ptr) is
      --  gives a pointer to the container object
      Scope : Container.Impl.Object_Ptr :=
        Container.Impl.To_Object (Contained_Object.Defined_In);
      Scope_As_Contained : Object_Ptr;
      Success : Boolean;
   begin
      --  We must change the absolute_name depending on the case
      if   Container.Impl.Get_Def_Kind (Scope)
        = Dk_Repository
      then
         Contained_Object.Absolute_Name :=
           CORBA.ScopedName (CORBA.To_CORBA_String ("::")
                       & CORBA.String (Contained_Object.Name));
      else
         To_Contained (Container.Impl.Get_Real_Object(Scope),
                       Success,
                       Scope_As_Contained);
         if not Success then
            Broca.Exceptions.Raise_Internal;
         end if;
         Contained_Object.Absolute_Name :=
           CORBA.ScopedName (CORBA.String (Get_Name (Scope_As_Contained))
                       & CORBA.To_CORBA_String ("::")
                       & CORBA.String (Contained_Object.Name));
      end if;
      --  change also recursively all ist descendant if it is a container
      declare
         Container_Object : Container.Impl.Object_Ptr;
      begin
         Container.Impl.To_Container (Get_Real_Object (Contained_Object),
                                      Success,
                                      Container_Object);
         if Success then
            declare
               Cont_Array : Contained_Seq.Element_Array
                 := Contained_Seq.To_Element_Array
                 (Container.Impl.Get_Contents (Container_Object));
            begin
               for I in Cont_Array'Range loop
                  Change_Absolute_Name (Cont_Array (I));
               end loop;
            end;
         end if;
      end;
   end Change_Absolute_Name;

   Package Contained_For_Seq renames IDL_SEQUENCE_CORBA_Repository_Root_Contained_Forward;

   -----------------
   --  To_Object  --
   -----------------
   function To_Object (Fw_Ref : Contained_Forward.Ref)
                       return Object_Ptr is
   begin
      return Contained.Impl.Object_Ptr
        (Contained.Object_Of
         (Contained.Convert_Forward.To_Ref
          (Fw_Ref)));
   end To_Object;

   --------------------
   --  To_Contained  --
   --------------------
   procedure To_Contained
     (Self : IRObject.Impl.Object_Ptr;
      Success : out Boolean;
      Result : out Object_ptr)
   is
   begin
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
      if CORBA.AbstractBase.Is_Nil
        --  Convert to AbstractBase.Ref for using Is_Nil.
        (CORBA.AbstractBase.Ref
         (Repository.Impl.Lookup_Id
          --  Convert the ref to object (must cast).
          (Repository.Impl.To_Object (Self.Containing_Repository),
          To))) then
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
         --  We must change the absolute_name
         Change_Absolute_Name (Object_Ptr (Self));
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
      Cont : Container.Impl.Object_Ptr;
   begin
      Cont := Container.Impl.To_Object (Self.Defined_In);
      case Container.Impl.Get_Def_Kind (Cont) is
         when Dk_Repository =>
            return  CORBA.Null_RepositoryId;
         when  Dk_Struct     =>
              declare
                 Interm : Structdef.Impl.Object_Ptr :=
                   Structdef.Impl.Object_Ptr (Container.Impl.Get_Real_Object (Cont));
              begin
                 return Structdef.Impl.Get_Id (Interm);
              end;
         when  Dk_Union      =>
            declare
               Interm : Uniondef.Impl.Object_Ptr :=
                 Uniondef.Impl.Object_Ptr (Container.Impl.Get_Real_Object (Cont));
            begin
               return Uniondef.Impl.Get_Id (Interm);
            end;
         when Dk_Exception  =>
            declare
               Interm : Exceptiondef.Impl.Object_Ptr :=
                 Exceptiondef.Impl.Object_Ptr (Cont);
            begin
               return Get_Id (Exceptiondef.Impl.Get_Contained_View (Interm));
            end;
         when Dk_Module     =>
            declare
               Interm : Moduledef.Impl.Object_Ptr :=
                 Moduledef.Impl.Object_Ptr (Cont);
            begin
               return Get_Id (Moduledef.Impl.Get_Contained_View (Interm));
            end;
         when Dk_Value      =>
            declare
               Interm : Valuedef.Impl.Object_Ptr :=
                 Valuedef.Impl.Object_Ptr (Cont);
            begin
               return Get_Id (Valuedef.Impl.Get_Contained_View (Interm));
            end;
         when Dk_Interface  =>
            declare
               Interm : Interfacedef.Impl.Object_Ptr :=
                 Interfacedef.Impl.Object_Ptr (Cont);
            begin
               return Get_Id (Interfacedef.Impl.Get_Contained_View (Interm));
            end;
         when others =>
            Broca.Exceptions.Raise_Internal;
            return CORBA.Null_RepositoryId;
      end case;
   end Get_Defined_In;

   function get_absolute_name
     (Self : access Object)
     return CORBA.ScopedName
   is
   begin
      return Self.Absolute_Name;
   end get_absolute_name;


   function get_containing_repository
     (Self : access Object)
     return Repository_Forward.Ref
   is
   begin
      return Self.Containing_Repository;
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

   procedure move
     (Self : access Object;
      new_container : in Container_Forward.Ref;
      new_name : in CORBA.Identifier;
      new_version : in VersionSpec) is

      For_Container_Ptr : Container.Impl.Object_Ptr
        := Container.Impl.To_Object (Self.Defined_In);
      New_Container_Ptr : Container.Impl.Object_Ptr
        := Container.Impl.To_Object (New_Container);
      New_Contained : Object_Ptr;
      Success : Boolean;
      Rep1 : Repository.Impl.Object_Ptr
        := Repository.Impl.To_Object (Self.Containing_Repository);
      Rep2 : Repository.Impl.Object_Ptr;
      Not_Allowed : Boolean := False;
      use Repository.Impl;
   begin
      if Container.Impl.Get_Def_Kind (New_Container_Ptr) = Dk_Repository then
         Rep2 := Repository.Impl.Object_Ptr (New_Container_Ptr);
      else
         To_Contained (Container.Impl.Get_Real_Object (New_Container_Ptr),
                       Success,
                       New_Contained);
         if not Success then
            Broca.Exceptions.Raise_Internal;
            return;
         end if;
         Rep2 := Repository.Impl.To_Object
           (Get_Containing_Repository (New_Contained));
      end if;
      -- It must be in the same Repository
      if Rep1 /= Rep2 then
         Broca.Exceptions.Raise_Bad_Param (Minor => 4);
         Not_Allowed := True;
      else

         --  the move should comply with with p10-8 of the IR spec.
         --  (structure and navigation in the IR)
         case Get_Def_Kind (Self) is
            when
              Dk_Operation |
              Dk_Attribute =>
               if Container.Impl.Get_Def_Kind (New_Container_Ptr) = Dk_Repository
                 or
                 Container.Impl.Get_Def_Kind (New_Container_Ptr) = Dk_Module then
                  Not_Allowed := True;
               end if;
            when
              Dk_ValueMember =>
               if Container.Impl.Get_Def_Kind (New_Container_Ptr) /= Dk_Value
               then
                  Not_Allowed := True;
               end if;
            when
              Dk_Interface |
              Dk_Value  =>
               if Container.Impl.Get_Def_Kind (New_Container_Ptr) = Dk_Interface
                 or
                 Container.Impl.Get_Def_Kind (New_Container_Ptr) = Dk_Value then
                  Not_Allowed := True;
               end if;
            when others =>
               null;
         end case;
         if Not_Allowed then
            Broca.Exceptions.Raise_Bad_Param (Minor => 4);
         else
            --  check if the name is already used in this scope.
            --  should be more precise
            declare
               Other : ContainedSeq;
               use Contained_For_Seq;
            begin
               Other := Container.Impl.Lookup_Name
                 (New_Container_Ptr, New_Name, 1, Dk_All, False);
               if (Contained_For_Seq.Null_Sequence = Contained_For_Seq.Sequence (Other))
               then
                  Not_Allowed := True;
                  Broca.Exceptions.Raise_Bad_Param (Minor => 3);
               else
                  --  remove the contained from the previous container
                  Container.Impl.Delete_From_Contents (For_Container_Ptr,
                                                       Object_Ptr (Self));
                  --  we can move this contained to this container
                  Self.Defined_In := New_Container;
                  Self.Name := New_Name;
                  Self.Version := New_Version;
                  --  we must change the absolute_name, recursively
                  Change_Absolute_Name (Object_Ptr (Self));
                  --  add the contained to the new container
                  Container.Impl.Append_To_Contents (New_Container_Ptr,
                                                     Object_Ptr (Self));
               end if;
            end;
         end if;
      end if;
   end move;

   ------------------------
   -- A Seq of contained --
   ------------------------

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
   begin

      for I in Cont_Array'Range loop
         exit when Result /= null;
         if Cont_Array (I).Id = Search_Id then
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
      Result : ContainedSeq;
   begin
      return Result;
   end;

   -----------------------
   --  To_containedSeq  --
   -----------------------
   function To_ContainedSeq
     (In_Seq : Contained_Seq.Sequence)
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
            Contained.Set (The_Ref,
                           CORBA.Impl.Object_Ptr (Cont));
            Contained_For_Seq.Append (Contained_For_Seq.Sequence (Result),
                                      Contained.Convert_Forward.To_Forward (The_Ref));
         end;
      end loop;
      return Result;
   end To_ContainedSeq;


end CORBA.Repository_Root.Contained.Impl;






