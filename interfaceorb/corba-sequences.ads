-----------------------------------------------------------------------
-----------------------------------------------------------------------
----                                                               ----
----                         AdaBroker                             ----
----                                                               ----
----                  package Corba.Sequences                      ----
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
----                                                               ----
----   CORBA.Sequences is the parent of the bounded and unbounded  ----
----   sequence packages.  Some exceptions and types common to     ----
----   both are declared here (following the structure of          ----
----   Ada.Strings).                                               ----
----                                                               ----
----   Length_Error is raised when sequence lengths are exceeded.  ----
----   Pattern_Error is raised when a null pattern string is       ----
----   passed. Index_Error is raised when indexes are out of       ----
----   range.                                                      ----
----                                                               ----
----                                                               ----
----   author : unknown                                            ----
----                                                               ----
----                                                               ----
-----------------------------------------------------------------------
-----------------------------------------------------------------------


package Corba.Sequences is


    Length_Error, Pattern_Error, Index_Error : exception;

    type Alignment is (Left, Right, Center);
    type Truncation is (Left, Right, Error);
    type Membership is (Inside, Outside);
    type Direction is (Forward, Backward);

    type Trim_End is (Left, Right, Both);

end Corba.Sequences;

