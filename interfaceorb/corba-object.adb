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
      null ;
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



   --  Get_Dynamic_Ref
   ----------------------
    function Get_Dynamic_Ref(Self: in Ref) return Ref'Class is
    begin
       return Self ;
    end ;


    -- To_Ref
    ---------
    function To_Ref(The_Ref : in Ref'Class) return Ref is
    begin
       return Ref(The_Ref) ;
    end ;


    -- AdaBroker_Cast_To_Parent
    ---------------------------
    procedure AdaBroker_Cast_To_Parent(Real_Ref: in Ref;
                                       Result: out Corba.Object.Ref'Class ) is
    begin
       if Result in Ref then
          declare
             Tmp_Result : Corba.Object.Ref'Class := Real_Ref ;
          begin
             Result := Tmp_Result ;
             return ;
          end ;
       end if ;

       Ada.Exceptions.Raise_Exception(Constraint_Error'Identity,
                                      "Corba.Object.To_Ref :"
                                      & Corba.CRLF
                                      & "  Cannot cast Corba.Object.Ref"
                                      & Corba.CRLF
                                      & "  into "
                                      & Ada.Tags.External_Tag(Result'tag)) ;
    end ;


    --------------------------------------------------
    ---        omniORB specific                    ---
    --------------------------------------------------

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






