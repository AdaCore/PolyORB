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


