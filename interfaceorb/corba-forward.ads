-----------------------------------------------------------------------
----                                                               ----
----                  AdaBroker                                    ----
----                                                               ----
----                  package Corba.Forward                        ----
----                                                               ----
----   authors : Sebastien Ponce, Fabien Azavant                   ----
----   date    : 02/12/99                                          ----
----                                                               ----
----                                                               ----
-----------------------------------------------------------------------

with Corba.Object ;

generic
package Corba.Forward is

   type Ref is new Corba.Object.Ref with null record;

   generic
      type Ref_Type is new Corba.Object.Ref with private ;

   package Convert is
      function From_Forward(The_Forward : in Ref)
                            return Ref_Type ;
      function To_Forward(The_Ref : in Ref_Type)
                          return Ref ;
   end Convert ;


   end Corba.Forward ;

