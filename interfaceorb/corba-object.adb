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
   ---        AdaBroker  specific                 ---
   --------------------------------------------------

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






