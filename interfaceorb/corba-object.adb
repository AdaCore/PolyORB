-----------------------------------------------------------------------
----                                                               ----
----                  AdaBroker                                    ----
----                                                               ----
----                  package body CORBA.Object                    ----
----                                                               ----
----   authors : Sebastien Ponce, Fabien Azavant                   ----
----   date    : 02/08/99                                          ----
----                                                               ----
----                                                               ----
-----------------------------------------------------------------------

with Ada.Tags ;
with Omniobject ;
use type Omniobject.Object_Ptr ;
with Omniropeandkey ; use Omniropeandkey ;
with Corba.Dynamic_Type ;

package body Corba.Object is



   --------------------------------------------------
   ---        CORBA 2.2 specifications            ---
   --------------------------------------------------

   -- Is_Nil
   ---------
   function Is_Nil(Self: in Ref'Class) return Boolean is
   begin
      return  Self.Omniobj = null ;
   end ;


   -- Release
   ----------
   procedure Release (Self : in out Ref'class) is
   begin
      Omniobject.Release(Self.Omniobj.all) ;
      Self.Omniobj := null ;
      Self.Dynamic_Type := null ;
   end ;



    -- Is_A
    -------
    function Is_A(Self: in Ref ;
                 Logical_Type_Id : in Corba.String)
                 return Corba.Boolean is
    begin
       return (Repository_Id = Logical_Type_Id) ;
    end ;

    -- Is_A
    -------
    function Is_A(Logical_Type_Id : in Corba.String)
                  return Corba.Boolean is
    begin
       return (Repository_Id = Logical_Type_Id) ;
    end ;


    -- Non_Existent
   ---------------
   function Non_Existent(Self : in Ref) return Corba.Boolean is
   begin
      if Is_Nil(self) then
         return True ;
      end if ;
      return False ;
   end ;


   -- Is_Equivalent
   ----------------
   function Is_Equivalent(Self : in Ref ;
                          Other : in Ref)
                          return Corba.Boolean is
      Rak : Omniropeandkey.Object ;
      Other_Rak : Omniropeandkey.Object ;
      S1, S2 : Corba.Boolean ;
   begin
      -- this is copied from corbaObject.cc L160.
      -- Here, Refs are proxy objects
      OmniObject.Get_Rope_And_Key(Self.Omniobj.all, Rak, S1) ;
      Omniobject.Get_Rope_And_Key(Other.Omniobj.all, Other_Rak, S2) ;
      return S1 and S2 and (Rak = Other_Rak) ;
   end;


   -- Hash
   -------
   function Hash(Self : in Ref ;
                 Maximum : in Corba.Unsigned_long)
                 return Corba.Unsigned_Long is
   begin
      -- Copy Paste The C++ code
      -- return Real_Hash(Self, Maximum) ;
      return Corba.Unsigned_Long(0) ;
   end ;


   -- Object_Is_Ready
   ------------------
   procedure Object_Is_Ready(Self : in Ref'Class) is
   begin
      if not Is_Nil(Self) then
         Omniobject.Omniobject_Is_Ready(Self.Omniobj.all) ;
      else
         Ada.Exceptions.Raise_Exception(Corba.Adabroker_Fatal_Error'Identity,
                                        "Corba.Object.Object_Is_Ready(Corba.Object.Ref)"
                                        & Corba.CRLF
                                        & "Cannot be called on nil object") ;
      end if ;
   end ;
   --------------------------------------------------
   ---        AdaBroker  specific                 ---
   --------------------------------------------------

    -- Assert_Ref_Not_Nil
   ---------------------
   procedure Assert_Ref_Not_Nil(Self : in Ref) is
   begin
      if Corba.Object.Is_Nil(Self) then
         declare
         begin
            Corba.Raise_Corba_Exception(Corba.Bad_Operation'Identity,
                                        new Corba.Bad_Param_Members'(0, Corba.Completed_No)) ;
         end ;
      end if ;
   end ;


   -- Get_Repository_Id
   --------------------
   function Get_Repository_Id(Self : in Ref) return Corba.String is
   begin
      return Repository_Id ;
   end ;

   --  Get_Dynamic_Type
   ----------------------
    function Get_Dynamic_Type(Self: in Ref) return Ref'Class is
    begin
       return Self.Dynamic_Type.all ;
    end ;


    -- To_Ref
    ---------
    function To_Ref(The_Ref : in Ref'Class) return Ref is
    begin
       return Ref(The_Ref) ;
    end ;


   -- Object_To_String
   -------------------
   function Object_To_String (Self : in Ref'class)
                              return CORBA.String is
   begin
      if Is_Nil(Self) then
         return Omniobject.Object_To_String(null) ;
      else
         return Omniobject.Object_To_String(Self.Omniobj) ;
      end if ;
   end ;


   -- String_To_Object
   -------------------
   procedure String_to_Object (From : in CORBA.String;
                               To : out CORBA.Object.Ref'class) is
      RepoId : Corba.String ;
      Tmp_Obj_ptr : Omniobject.Object_Ptr ;
   begin
      -- Get the omniobject
      Tmp_Obj_ptr := Omniobject.String_To_Object(From) ;

      -- if there was an error somewhere
      if Tmp_Obj_Ptr = null then
         To.Omniobj := null ;
         To.Dynamic_Type := null ;
         return ;
      end if ;

      -- check if the omniobject we got can be put into
      -- To (type implied the repoId)
      RepoId := Omniobject.Get_Repository_Id(Tmp_Obj_Ptr.all) ;

      if Is_A(To, RepoId) then
         To.Omniobj := Tmp_Obj_ptr ;
         To.Dynamic_Type :=
           Corba.Dynamic_Type.Get_Dynamic_Type_From_Repository_Id(From) ;
         return ;
      end if ;

      -- otherwise, the operation is illegal
      Ada.Exceptions.Raise_Exception(Constraint_Error'Identity,
                                     "Corba.Object.String_To_Object :"
                                     & Corba.CRLF
                                     & "Cannot cast "
                                     & Corba.To_Standard_String(RepoId)
                                     & Corba.CRLF
                                     & "Into "
                                     & Corba.To_Standard_String(Get_Repository_Id(To))) ;
   end ;

    --------------------------------------------------
    ---        omniORB specific                    ---
    --------------------------------------------------



   -- Marshal_Object_Reference
   ---------------------------
   procedure Marshal_Object_Reference(The_Ref : in Ref ;
                                      S : in out NetBufferedStream.Object) is
      Tmp : Ref'Class := The_Ref ;
   begin
      null ;
   end ;



    -- Marshal_Obj_Ref
    ------------------
    procedure Marshal_Obj_Ref(The_Ref: Ref'Class ;
                              RepoID : String ;
                              Nbs : NetBufferedStream.Object) is
    begin
       Ada.Exceptions.Raise_Exception(Corba.AdaBroker_Not_Implemented_Yet'Identity,
                                      "Corba.Object.Marshal_Obj_Ref") ;
    end ;


    -- Marshal_Obj_Ref
    ------------------
    procedure Marshal_Obj_Ref(The_Ref: Ref'Class ;
                              RepoID : String ;
                              Mbs : MemBufferedStream.Object) is
    begin
       Ada.Exceptions.Raise_Exception(Corba.AdaBroker_Not_Implemented_Yet'Identity,
                                      "Corba.Object.Marshal_Obj_Ref") ;
    end ;

    -- UnMarshal_Obj_Ref
    --------------------
    function UnMarshal_Obj_Ref(Repoid : in String ;
                               Nbs : in NetBufferedStream.Object'Class)
                               return Ref'Class is
    begin
       Ada.Exceptions.Raise_Exception(Corba.AdaBroker_Not_Implemented_Yet'Identity,
                                      "Corba.Object.UnMarshal_Obj_Ref") ;
       return Nil_Ref ;
    end ;


    -- UnMarshal_Obj_Ref
    --------------------
    function UnMarshal_Obj_Ref(Repoid : in String ;
                               Mbs : in MemBufferedStream.Object'Class)
                               return Ref'Class is
    begin
       Ada.Exceptions.Raise_Exception(Corba.AdaBroker_Not_Implemented_Yet'Identity,
                                      "Corba.Object.UnMarshal_Obj_Ref") ;
       return Nil_Ref ;
    end ;

    -- UnMarshal_Obj_Ref
    --------------------
   function UnMarshal_Obj_Ref(Nbs : in NetBufferedStream.Object'Class)
                              return Ref'Class is
    begin
       Ada.Exceptions.Raise_Exception(Corba.AdaBroker_Not_Implemented_Yet'Identity,
                                      "Corba.Object.UnMarshal_Obj_Ref") ;
       return Nil_Ref ;
    end ;


    -- UnMarshal_Obj_Ref
    --------------------
   function UnMarshal_Obj_Ref(Nbs : in MemBufferedStream.Object'Class)
                              return Ref'Class is
    begin
       Ada.Exceptions.Raise_Exception(Corba.AdaBroker_Not_Implemented_Yet'Identity,
                                      "Corba.Object.UnMarshal_Obj_Ref") ;
       return Nil_Ref ;
    end ;


    -- Aligned_Obj_Ref
    --------------------
   function Aligned_Obj_Ref(The_Ref : Ref'Class ;
                            RepoID : String ;
                            Initial_Offset : Integer)
                            return Integer is
   begin
      Ada.Exceptions.Raise_Exception(Corba.AdaBroker_Not_Implemented_Yet'Identity,
                                     "Corba.Object.Aligned_Obj_Ref") ;
      return 0 ;
   end ;


   --------------------------------------------------
   --------------------------------------------------
   --------------------------------------------------
   ---            private part                    ---
   --------------------------------------------------
   --------------------------------------------------
   --------------------------------------------------


   --------------------------------------------------
   ---     references to implementations          ---
   --------------------------------------------------

   -- Initialize
   -------------
   procedure Initialize (Self: in out Ref) is
   begin
      null ;
   end ;


   -- Adjust
   ---------
   procedure Adjust (Self: in out Ref) is
   begin
      if not Is_Nil(Self) then
         Omniobject.Duplicate(Self.Omniobj.all) ;
      end if ;
   end ;

   -- Finalize
   -----------
   procedure Finalize (Self: in out Ref) is
   begin
      if not Is_Nil(Self) then
         Omniobject.Release(Self.Omniobj.all) ;
      end if ;
   end ;


end Corba.Object ;










