----------------------------------------------
--  This file has been generated automatically
--  by AdaBroker (http://adabroker.eu.org/)
----------------------------------------------

with CORBA.Repository_Root.IRObject.Impl;
pragma Elaborate_All (CORBA.Repository_Root.IRObject.Impl);

package CORBA.Repository_Root.Contained.Impl is

   type Object is
     new CORBA.Repository_Root.IRObject.Impl.Object with private;

   type Object_Ptr is access all Object'Class;

   --  method used to initialize recursively the object fields.
   procedure Init (Self : access Object;
                   Real_Object :
                     CORBA.Repository_Root.IRObject.Impl.Object_Ptr;
                   Def_Kind : Corba.Repository_Root.DefinitionKind;
                   Id : CORBA.RepositoryId;
                   Name : CORBA.Identifier;
                   Version : CORBA.Repository_Root.VersionSpec;
                   Defined_In : CORBA.Repository_Root.Container_Forward.Ref;
                   Absolute_Name : CORBA.ScopedName;
                   Containing_Repository :
                     CORBA.Repository_Root.Repository_Forward.Ref);

   --  Transform the forward to an impl.object.ptr.
   function To_Object (Fw_Ref : Contained_Forward.Ref)
                       return Object_Ptr;

   --  usefull for the multiple inhertance
   --  transform an IRObject to a container
   --  success is true if it is possible
   procedure To_Contained
     (Self : CORBA.Repository_Root.IRObject.Impl.Object_Ptr;
      Success : out Boolean;
      Result : out Object_Ptr);


   function get_id
     (Self : access Object)
     return CORBA.RepositoryId;

   procedure set_id
     (Self : access Object;
      To : in CORBA.RepositoryId);

   function get_name
     (Self : access Object)
     return CORBA.Identifier;

   procedure set_name
     (Self : access Object;
      To : in CORBA.Identifier);

   function get_version
     (Self : access Object)
     return CORBA.Repository_Root.VersionSpec;

   procedure set_version
     (Self : access Object;
      To : in CORBA.Repository_Root.VersionSpec);

   function get_defined_in
     (Self : access Object)
     return CORBA.Repository_Root.Container_Forward.Ref;

   function get_defined_in
     (Self : access Object)
     return CORBA.RepositoryId;

   function get_absolute_name
     (Self : access Object)
     return CORBA.ScopedName;

   function get_containing_repository
     (Self : access Object)
     return CORBA.Repository_Root.Repository_Forward.Ref;

   function describe
     (Self : access Object)
     return CORBA.Repository_Root.Contained.Description;

   procedure move
     (Self : access Object;
      new_container : in CORBA.Repository_Root.Container_Forward.Ref;
      new_name : in CORBA.Identifier;
      new_version : in CORBA.Repository_Root.VersionSpec);


   --------------------------------
   -- A useful list of contained --
   --------------------------------

   type Contained_List is private;
   --  A list of containeds.

   type Contained_Iterator is private;
   --  An iterator on a contained list.

   --  the empty list
   Nil_List : constant Contained_List;

   function Head
     (NL : Contained_List)
     return Object_Ptr;
   --  Return the first contained in NL.

   function Is_Empty
     (NL : Contained_List)
     return Boolean;
   --  True iff NL is empty.

   function Length
     (NL : Contained_List)
     return Natural;
   --  The length of a list.

   --  Simple way to iterate over a contained_list.
   --  CONTAINED_ITERATOR is a type representing an iterator, which must
   --  be initialiazed by INIT.
   --  End of list is detected by IS_END.
   --  Until the end of list is reached, the contained can be extracted
   --  with GET_CONTAINED and the iterator can be incremented with NEXT.
   --  Therefore, usual way to use an iterator is:
   --  declare
   --    it: contained_iterator;
   --    contained: Object_Ptr;
   --  begin
   --    init (it, rep.contents);
   --    while not is_end (it) loop
   --      get_next_contained (it, contained);
   --      ...
   --    end loop;
   --  end;

   procedure Init
     (It : out Contained_Iterator;
      List : Contained_List);

   procedure Get_Next_Contained
     (It : in out Contained_Iterator;
      Contained : out Object_Ptr);

   function Is_End
     (It : Contained_Iterator)
     return Boolean;

   --  Appends a contained at the end of a list.
   procedure Append_Contained (List : in out Contained_List;
                          Contained : in Object_Ptr);

   --  Appends a contained at the end of a list.
   function Append_Contained (List : in Contained_List;
                         Contained : Object_Ptr) return Contained_List;

   --  Remove the first occurrence of Contained from List
   --  Take care, those functions doesn't remove the Contained, just the cell!
   procedure Remove_Contained
     (List : in out Contained_List;
      Contained : Object_Ptr);
   function Remove_Contained
     (List : in Contained_List;
      Contained : Object_Ptr)
     return Contained_List;

   --  Insert Contained into List immediately before the first
   --  occurrence of Before.
   procedure Insert_Before
     (List : in out Contained_List;
      Contained : Object_Ptr;
      Before : Object_Ptr);

   --  Insert Contained into List immediately after the first
   --  occurrence of After.
   procedure Insert_After
     (List : in Contained_List;
      Contained : Object_Ptr;
      After : Object_Ptr);

   --  Look whether contained is in list or not
   function Is_In_List (List : Contained_List; Contained : Object_Ptr) return Boolean;

   --  Look whether contained is in the list or not
   --  contained is supposed to be a scoped name and the list must be
   --  a list of scoped names. What is compared here is not the containeds
   --  themselves but the contained they are pointing to
   function Is_In_Pointed_List (List : Contained_List; Contained : Object_Ptr)
                                return Boolean;

   --  Frees all the list
   procedure Free (List : in out Contained_List);

   --  computes the length of the list
   function Get_Length (List : in Contained_List) return Integer;

   --  Function that take a contained list and remove all the redondant items
   --  returns the resulting contained list
   --  useful for the inheritance treatement
   function Simplify_Contained_List (In_List : Contained_List) return Contained_List;

   procedure Merge_List
     (Into : in out Contained_List;
      From : in Contained_List);
   --  Appends all containeds of list From to list Into, unless they are
   --  in it already.

   --  transform this list into a corba sequence
   function To_ContainedSeq
     (In_List : Contained_List)
      return  CORBA.Repository_Root.ContainedSeq;

   --  search the list for a given ID
   --  return null if not found
   function Lookup_Id
     (In_List : Contained_List;
      Search_Id : CORBA.RepositoryId)
      return Object_Ptr;

private

   type Object is
     new CORBA.Repository_Root.IRObject.Impl.Object with record
        Id : CORBA.RepositoryId;
        Name : CORBA.Identifier;
        Version : CORBA.Repository_Root.VersionSpec;
        Defined_In : CORBA.Repository_Root.Container_Forward.Ref;
        Absolute_Name : CORBA.ScopedName;
        Containing_Repository : CORBA.Repository_Root.Repository_Forward.Ref;
     end record;

   --------------------
   -- Contained list --
   --------------------

   type Contained_List_Cell;
   type Contained_List is access Contained_List_Cell;
   type Contained_List_Cell is record
      Car : Object_Ptr;
      Cdr : Contained_List;
   end record;

   Nil_List : constant Contained_List := null;

   type Contained_Iterator is new Contained_List;


end CORBA.Repository_Root.Contained.Impl;














