-----------------------------------------------------------------------
----                                                               ----
----                  AdaBroker                                    ----
----                                                               ----
----              package Corba.Command_Line                       ----
----                                                               ----
----      This package allows to transforms the Ada command        ----
----      line arguments into C's argc and argv                    ----
----                                                               ----
----   authors : Fabien Azavant                                    ----
----   date    : 03/05/99                                          ----
----                                                               ----
----                                                               ----
-----------------------------------------------------------------------

with Ada.Command_Line ;  use Ada.Command_Line ;
with Interfaces.C ; use Interfaces.C ;
with Interfaces.C.Strings ; use Interfaces.C.Strings ;


package body Corba.Command_Line is


   procedure Get_Command_Line(C_Argc : out Interfaces.C.Int ;
                              C_Argv : out System.Address) is
      Argc :  Natural := Argument_Count ;
      CArgc : Size_T := Size_T(Argc) ;
      Argv : Interfaces.C.Strings.Chars_Ptr_Array(0..CArgc) ;
   begin
      Argv(0) := New_String("name_of_the_program") ;
      for I in 1..Argument_Count loop
         declare
            S : Size_T := Size_T(I) ;
         begin
            Argv(S) := New_String(Argument(I)) ;
         end ;
      end loop ;
      C_Argc := Interfaces.C.Int(CArgc+1) ;
      C_Argv := Argv'Address ;
   end ;

end Corba.Command_Line ;
