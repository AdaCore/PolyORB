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


package body Corba.Forward is


   --------------------------------------------------------
   ----             package Convert                    ----
   --------------------------------------------------------
   package body Convert is

      -- From_Forward
      ---------------
      function From_Forward(The_Forward : in Ref)
                            return Ref_Type is
         Intermediate_Result : Corba.Object.Ref ;
         Result : Ref_Type ;
      begin
         Intermediate_Result :=  Corba.Object.Ref(The_Forward) ;
         Result := (Intermediate_Result with null record) ;
         return Result ;
      end ;

      -- To_Forward
      -------------
      function To_Forward(The_Ref : in Ref_Type)
                          return Ref is
         Intermediate_Result : Corba.Object.Ref ;
         Result : Ref ;
      begin
         Intermediate_Result :=  Corba.Object.Ref(The_Ref) ;
         Result := (Intermediate_Result with null record) ;
         return Result ;
      end ;


   end Convert ;


end Corba.Forward ;

