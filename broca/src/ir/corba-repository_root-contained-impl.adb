----------------------------------------------
--  This file has been generated automatically
--  by AdaBroker (http://adabroker.eu.org/)
----------------------------------------------

with Ada.Unchecked_Deallocation;
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
         It : Contained_Iterator;
         Cont : Object_Ptr;
      begin
         Container.Impl.To_Container (Get_Real_Object (Contained_Object),
                                      Success,
                                      Container_Object);
         if Success then
            Init (It, Container.Impl.Get_Contained_List (Container_Object));
            while not Is_End (It) loop
               Get_Next_Contained (It, Cont);
               Change_Absolute_Name (Cont);
            end loop;
         end if;
      end;
   end Change_Absolute_Name;

   Package Contained_Seq renames IDL_SEQUENCE_CORBA_Repository_Root_Contained_Forward;

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
      use Contained_Seq;
   begin
      --  Must check if the name is not already used in this scope
      --  So we check all the nodes in this container with the same name
      Other := Container.Impl.Lookup_Name
        (Container.Impl.To_Object (Self.Defined_In),
         To,
         1,
         Dk_All,
         False);
      if Contained_Seq.Null_Sequence = (Contained_Seq.Sequence (Other))
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
               use Contained_Seq;
            begin
               Other := Container.Impl.Lookup_Name
                 (New_Container_Ptr, New_Name, 1, Dk_All, False);
               if (Contained_Seq.Null_Sequence = Contained_Seq.Sequence (Other))
               then
                  Not_Allowed := True;
                  Broca.Exceptions.Raise_Bad_Param (Minor => 3);
               else
                  --  remove the contained from the previous container
                  Container.Impl.Set_Contained_List
                    (For_Container_Ptr,
                     Remove_Contained (Container.Impl.Get_Contained_List
                                       (For_Container_Ptr),
                                       Object_Ptr (Self)));
                  --  we can move this contained to this container
                  Self.Defined_In := New_Container;
                  Self.Name := New_Name;
                  Self.Version := New_Version;
                  --  we must change the absolute_name, recursively
                  Change_Absolute_Name (Object_Ptr (Self));
                  --  add the contained to the new container
                  Container.Impl.Set_Contained_List
                    (New_Container_Ptr,
                     Append_Contained (Container.Impl.Get_Contained_List
                                       (New_Container_Ptr),
                                       Object_Ptr (Self)));
               end if;
            end;
         end if;
      end if;
   end move;

   -------------------------
   -- A list of contained --
   -------------------------

   ------------
   --  Head  --
   ------------

   function Head
     (NL : Contained_List)
     return Object_Ptr is
   begin
      pragma Assert (NL /= Nil_List);
      return NL.Car;
   end Head;

   ----------------
   --  Is_Empty  --
   ----------------

   function Is_Empty
     (NL : Contained_List)
     return Boolean is
   begin
      return NL = Nil_List;
   end Is_Empty;

   --------------
   --  Length  --
   --------------

   function Length
     (NL : Contained_List)
     return Natural
   is
      Current : Contained_List
        := NL;
      Count : Natural
        := 0;
   begin
      while not Is_Empty (Current) loop
         Count := Count + 1;
         Current := Current.Cdr;
      end loop;

      return Count;
   end Length;

   ----------
   -- Init --
   ----------

   procedure Init (It : out Contained_Iterator; List : Contained_List) is
   begin
      It := Contained_Iterator (List);
   end Init;

   ------------------------
   -- Get_Next_Contained --
   ------------------------

   procedure Get_Next_Contained
     (It : in out Contained_Iterator;
      Contained : out Object_Ptr) is
   begin
      Contained := It.Car;
      It := Contained_Iterator (It.Cdr);
   end Get_Next_Contained;

   --------------
   --  Is_End  --
   --------------
   function Is_End (It : Contained_Iterator) return Boolean is
   begin
      return Is_Empty (Contained_List (It));
   end Is_End;

   ------------------------
   --  Append_Contained  --
   ------------------------

   procedure Append_Contained
     (List : in out Contained_List;
      Contained : in Object_Ptr) is
   begin
      List := Append_Contained (List, Contained);
   end Append_Contained;

   ------------------------
   --  Append_Contained  --
   ------------------------

   function Append_Contained
     (List : Contained_List;
      Contained : Object_Ptr) return Contained_List
   is
      Cell, Last : Contained_List;
   begin
      Cell := new Contained_List_Cell'(Car => Contained, Cdr => null);
      if List = null then
         return Cell;
      else
         Last := List;
         while Last.Cdr /= null loop
            Last := Last.Cdr;
         end loop;
         Last.Cdr := Cell;
         return List;
      end if;
   end Append_Contained;

   ---------------------
   --  Insert_Before  --
   ---------------------

   procedure Insert_Before
     (List : in out Contained_List;
      Contained : Object_Ptr;
      Before : Object_Ptr)
   is
      Cell : Contained_List;
   begin
      pragma Assert (List /= Nil_List);

      if List.Car = Before then
         Cell := new Contained_List_Cell'
           (Car => Contained,
            Cdr => List);
         List := Cell;
      else
         Insert_Before (List.Cdr, Contained, Before);
      end if;
   end Insert_Before;

   --------------------
   --  Insert_After  --
   --------------------

   procedure Insert_After
     (List : in Contained_List;
      Contained : Object_Ptr;
      After : Object_Ptr)
   is
      Cell : Contained_List;
   begin
      pragma Assert (List /= Nil_List);

      if List.Car = After then
         Cell := new Contained_List_Cell'
           (Car => Contained,
            Cdr => List.Cdr);
         List.Cdr := Cell;
      else
         Insert_After (List.Cdr, Contained, After);
      end if;
   end Insert_After;

   ------------------
   --  Is_In_List  --
   ------------------

   function Is_In_List
     (List : Contained_List;
      Contained : Object_Ptr)
     return Boolean is
   begin
      pragma Debug (O2 ("Is_In_List : enter"));
      if List = Nil_List then
         pragma Debug (O2 ("Is_In_List : enter"));
         pragma Debug (O ("Is_In_List : nil_list"));
         return False;
      end if;
      if List.Car = Contained then
         pragma Debug (O2 ("Is_In_List : enter"));
         pragma Debug (O ("Is_In_List : found"));
         return True;
      else
         pragma Debug (O2 ("Is_In_List : enter"));
         pragma Debug (O ("Is_In_List : searching further"));
         return Is_In_List (List.Cdr, Contained);
      end if;
   end Is_In_List;

   --------------------------
   --  Is_In_Pointed_List  --
   --------------------------

   function Is_In_Pointed_List
     (List : Contained_List;
      Contained : Object_Ptr)
      return Boolean is
   begin
      if List = Nil_List then
         return False;
      end if;
      if List.Car = Contained then
         return True;
      else
         return Is_In_Pointed_List (List.Cdr, Contained);
      end if;
   end Is_In_Pointed_List;

   ------------------------
   --  Remove_Contained  --
   ------------------------

   procedure Unchecked_Deallocation is
      new Ada.Unchecked_Deallocation
     (Contained_List_Cell, Contained_List);

   ------------------------
   --  Remove_Contained  --
   ------------------------

   function Remove_Contained
     (List : Contained_List;
      Contained : Object_Ptr)
     return Contained_List is
   begin
      if List /= Nil_List then
         if List.Car = Contained then
            declare
               Old_List : Contained_List := List;
               Old_Cdr : constant Contained_List
                 := List.Cdr;
            begin
               Unchecked_Deallocation (Old_List);
               return Old_Cdr;
            end;
         else
            List.Cdr := Remove_Contained (List.Cdr, Contained);
         end if;
      end if;
      return List;
   end Remove_Contained;

   procedure Remove_Contained
     (List : in out Contained_List;
      Contained : Object_Ptr) is
   begin
      List := Remove_Contained (List, Contained);
   end Remove_Contained;

   ------------
   --  Free  --
   ------------

   procedure Free
     (List : in out Contained_List)
   is
      Old_List : Contained_List;
   begin
      while List /= null loop
         Old_List := List;
         List := List.Cdr;
         Unchecked_Deallocation (Old_List);
      end loop;
   end Free;

   ------------------
   --  Get_Length  --
   ------------------

   function Get_Length
     (List : Contained_List)
     return Integer
   is
      Temp_List : Contained_List := List;
      Result : Integer := 0;
   begin
      while Temp_List /= Nil_List loop
         Result := Result + 1;
         Temp_List := Temp_List.Cdr;
      end loop;
      return Result;
   end Get_Length;

   -------------------------------
   --  Simplify contained list  --
   -------------------------------

   function Simplify_Contained_List
     (In_List : Contained_List)
     return Contained_List
   is
      Result_List : Contained_List := null;
      It : Contained_Iterator;
      Contained : Object_Ptr;
   begin
      Init (It, In_List);
      while not Is_End (It) loop
         Get_Next_Contained (It, Contained);

         if not Is_In_List (Result_List, Contained) then
            Append_Contained (Result_List, Contained);
         end if;
      end loop;
      return Result_List;
   end Simplify_Contained_List;

   procedure Merge_List
     (Into : in out Contained_List;
      From : in Contained_List)
   is
      It : Contained_Iterator;
      N : Object_Ptr;
   begin
      Init (It, From);
      while not Is_End (It) loop
         Get_Next_Contained (It, N);

         if not Is_In_List (Into, N) then
            Append_Contained (Into, N);
         end if;
      end loop;
   end Merge_List;

   -----------------
   --  Lookup_id  --
   -----------------
   function Lookup_Id (In_List : Contained_List;
                       Search_Id : CORBA.RepositoryId)
     return Object_Ptr is
      It : Contained_Iterator;
      N : Object_Ptr;
      Result : Object_Ptr := null;
      Success : Boolean;
      Container_Object : Container.Impl.Object_Ptr;
   begin
      Init (It, In_List);
      while (not Is_End (It)) or (Result = null)  loop
         Get_Next_Contained (It, N);
         if N.Id = Search_Id then
            Result := N;
         else
            Container.Impl.To_Container (Get_Real_Object (N),
                                         Success,
                                         Container_Object);
            if Success then
               Result := Lookup_Id (Container.Impl.Get_Contained_List
                                    (Container_Object),
                                    Search_Id);
            end if;
         end if;
      end loop;
      return Result;
   end Lookup_Id;

   -----------------------
   --  To_containedSeq  --
   -----------------------
   function To_ContainedSeq
     (In_List : Contained_List)
      return  CORBA.Repository_Root.ContainedSeq is
      It : Contained_Iterator;
      The_Ref : CORBA.Repository_Root.Contained.Ref;
      Contained_Object : Object_Ptr;
      Result : CORBA.Repository_Root.ContainedSeq
        := CORBA.Repository_Root.ContainedSeq (Contained_Seq.Null_Sequence);
   begin
      Init (It, In_List);
      while not Is_End (It) loop
         Get_Next_Contained (It, Contained_Object);
         Contained.Set (The_Ref,
                        CORBA.Impl.Object_Ptr (Contained_Object));
         Contained_Seq.Append (Contained_Seq.Sequence (Result),
                               Contained.Convert_Forward.To_Forward (The_Ref));
      end loop;
      return Result;
   end;



end CORBA.Repository_Root.Contained.Impl;





