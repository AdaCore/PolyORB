-----------------------------------------------------------------------
----                                                               ----
----                  AdaBroker                                    ----
----                                                               ----
----                  package iop                                  ----
----                                                               ----
----   authors : Sebastien Ponce, Fabien Azavant                   ----
----   date    :                                                   ----
----                                                               ----
----                                                               ----
-----------------------------------------------------------------------


with System ;

package Iop is

   type Tagged_Profile_List is new System.Address ;
   -- corresponds to IOP::TaggedProfileList (see IOP.h)
   -- This object is never used in Ada (just taken from a C function
   -- and given to another one) so it is not right implemented.
   -- We just keep the system Address of the object.

end Iop ;
