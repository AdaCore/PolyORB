-----------------------------------------------------------------------
----                                                               ----
----                       AdaBroker                               ----
----                                                               ----
----    This is a debugging package for AdaBroker.                 ----
----    usage : add at the beginnig of each package :              ----
----                                                               ----
----    with Adabroker_Debug ;                                     ----
----    pragma Elaborate(Adabroker_Debug) ;                        ----
----                                                               ----
----   Debug : constant Boolean                                    ----
----           := Adabroker_Debug.Is_Active("specific_name") ;     ----
----                                                               ----
----   and then :                                                  ----
----                                                               ----
----     pragma Debug(Output(Debug, "degug info !!"))  ;           ----
----                                                               ----
----                                                               ----
----    The printing will be done if the Debug_Filename file       ----
----    contains a line with "specific_name"                       ----
----                                                               ----
----                  package AdaBroker_Debug                      ----
----                                                               ----
----   authors : Sebastien Ponce, Fabien Azavant                   ----
----   date    : 02/17/99                                          ----
----                                                               ----
----                                                               ----
-----------------------------------------------------------------------

package Adabroker_Debug is

   Debug_Filename : constant String := "AdaBroker_Debug_Options.txt" ;

   function Is_Active (Flag : in String) return Boolean;
   --  returns True if Flag was found in the debug file

   procedure Output (Flag : in Boolean ; Msg : in String ) ;
   --  Prints S on standard output if Flag is true

end Adabroker_Debug ;
