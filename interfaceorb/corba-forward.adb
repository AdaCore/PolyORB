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
         Result : Ref_Type ;
      begin
         return Result ;
      end ;

      -- To_Forward
      -------------
      function To_Forward(The_Ref : in Ref_Type)
                          return Ref is
         Result : Ref ;
      begin
         return Result ;
      end ;


   end Convert ;


end Corba.Forward ;

