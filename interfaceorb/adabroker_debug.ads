-----------------------------------------------------------------------
-----------------------------------------------------------------------
----                                                               ----
----                         AdaBroker                             ----
----                                                               ----
----                package AdaBroker_Debug                        ----
----                                                               ----
----                                                               ----
----   Copyright (C) 1999 ENST                                     ----
----                                                               ----
----   This file is part of the AdaBroker library                  ----
----                                                               ----
----   The AdaBroker library is free software; you can             ----
----   redistribute it and/or modify it under the terms of the     ----
----   GNU Library General Public License as published by the      ----
----   Free Software Foundation; either version 2 of the License,  ----
----   or (at your option) any later version.                      ----
----                                                               ----
----   This library is distributed in the hope that it will be     ----
----   useful, but WITHOUT ANY WARRANTY; without even the implied  ----
----   warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR     ----
----   PURPOSE.  See the GNU Library General Public License for    ----
----   more details.                                               ----
----                                                               ----
----   You should have received a copy of the GNU Library General  ----
----   Public License along with this library; if not, write to    ----
----   the Free Software Foundation, Inc., 59 Temple Place -       ----
----   Suite 330, Boston, MA 02111-1307, USA                       ----
----                                                               ----
----                                                               ----
----                                                               ----
----   Description                                                 ----
----   -----------                                                 ----
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
----                                                               ----
----   authors : Fabien Azavant                                    ----
----   date    : 03/10/99                                          ----
----                                                               ----
-----------------------------------------------------------------------
-----------------------------------------------------------------------


package Adabroker_Debug is

   Debug_Filename : constant String := "AdaBroker_Debug_Options.txt" ;

   function Is_Active (Flag : in String) return Boolean;
   --  returns True if Flag was found in the debug file

   procedure Output (Flag : in Boolean ; Msg : in String ) ;
   --  Prints S on standard output if Flag is true

end Adabroker_Debug ;
