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



   --  Get_Dynamic_Object
   ----------------------
    function Get_Dynamic_Object() return Ref'Class is
    begin
       return Dynamic_Object.all ;
    end ;


    -- To_Ref
    ---------
    function To_Ref(The_Ref : in Ref'Class) return Ref is
       Real_Object : Ref'Class :=
         Get_Dynamic_Object(The_Ref) ;
       Result : Ref ;
    begin
       AdaBroker_Cast_To_Parent(Real_Object,Result) ;
       return Result;
    end ;



private

   -- Initialize
   -------------
   procedure Initialize (Self: in out Ref'Class) is
   begin
      Dynamic_Object := Ref'Access;
   end ;


   -- Adjust
   ---------
   procedure Adjust (Self: in out Ref'Class)
     renames Initialize;

   -- Finalize
   -----------
   procedure Finalize (Self: in out Ref'Class) is
   begin
      -- nothing to do for the moment
      -- releases the underlying C++ pointer
   end ;


end Corba.Object ;






