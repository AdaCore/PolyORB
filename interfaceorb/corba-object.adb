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

package body Corba.Object is



   --------------------------------------------------
   ---        CORBA 2.2 specifications            ---
   --------------------------------------------------

   -- Is_Nil
   ---------
   function Is_Nil(Self: in Ref'Class) return Boolean is
   begin
      return Self.Is_Nil ;
   end ;


   -- Release
   ----------
   procedure Release (Self : in out Ref'class) is
   begin
      -- to be implemented
      null ;
   end ;



    -- Is_A
    -------
    function Is_A(Self: in Ref ;
                 Logical_Type_Id : in Corba.String)
                 return Corba.Boolean is
    begin
       return (Get_Repository_Id(Self) = Logical_Type_Id) ;
    end ;


    -- Non_Existent
   ---------------
   function Non_Existent(Self : in Ref) return Corba.Boolean is
   begin
      if Is_Nil(self) then
         return True ;
      end if ;
      return False ;--Real_Non_Existent(Self) ;
      -- in our omniobject C to Ada, copy paste the code for non_existent
      -- that can be found in corbaObject.cc
   end ;


   -- Is_Equivalent
   ----------------
   function Is_Equivalent(Self : in Ref ;
                          Other : in Ref)
                          return Corba.Boolean is
      --Rak : Ropeandkey.Object ;
      --Other_Rak : Ropeandkey.Object ;
   begin
      -- this is copied from corbaObject.cc L160.
      -- Here, Refs are proxy objects
      --Rak := Get_Rope_And_Key(Self) ;
      --Other_Rak := Get_Rope_And_Key(Other) ;
      --if Rak = Other_Rak then
      --   return True ;
      --else return False ;
      --end if ;
      return False ;
      -- to be implemented
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

   --------------------------------------------------
   ---        AdaBroker  specific                 ---
   --------------------------------------------------

    -- Assert_Ref_Not_Nil
   ---------------------
   procedure Assert_Ref_Not_Nil(Self : in Ref) is
   begin
      if Corba.Object.Is_Nil(Self) then
         declare
            Excp_Members : Corba.Bad_Param_Members ;
         begin
            Excp_Members := (0, Corba.Completed_No) ;
            Corba.Raise_Corba_Exception(Corba.Bad_Operation'Identity, Excp_Members) ;
         end ;
      end if ;
   end ;


   -- Get_Repository_Id
   --------------------
   function Get_Repository_Id(Self : in Ref) return Corba.String is
   begin
      return Corba.To_Corba_String("IDL:omg.org/CORBA/Object:1.0") ;
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


    -- AdaBroker_Set_Dynamic_Type
    -----------------------------
    procedure AdaBroker_Set_Dynamic_Type(Self : in out Ref'Class ;
                                        Dynamic_Type : in Ref_Ptr) is
    begin
       Self.Dynamic_Type := Dynamic_Type ;
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
   procedure Adjust (Self: in out Ref)
     renames Initialize;

   -- Finalize
   -----------
   procedure Finalize (Self: in out Ref) is
   begin
      null ;
      -- nothing to do for the moment
      -- releases the underlying C++ pointer
   end ;

   --------------------------------------------------
   ---     real implementations of objects        ---
   --------------------------------------------------

   -- Initialize
   -------------
   procedure Initialize (Self: in out Object) is
   begin
      null ;
   end ;


   -- Adjust
   ---------
   procedure Adjust (Self: in out Object)
     renames Initialize;

   -- Finalize
   -----------
   procedure Finalize (Self: in out Object) is
   begin
      null ;
   end ;


end Corba.Object ;






