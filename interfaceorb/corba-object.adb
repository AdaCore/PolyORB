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



   --  Get_Dynamic_Object
   ----------------------
    function Get_Dynamic_Object() return Ref'Class is
    begin
       return Dynamic_Object.all ;
    end ;


private

   procedure Initialise (Self: in out Ref'Class) is
   begin
      Dynamic_Object := Ref'Access;
   end Initialise;


   procedure Adjust (Self: in out Ref'Class)
     renames Initialise;

   procedure Finalize (Self: in out Ref'Class) is
   begin
      -- nothing to do for the moment
   end Finalize;







