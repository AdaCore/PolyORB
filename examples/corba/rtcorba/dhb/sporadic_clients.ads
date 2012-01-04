------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                     S P O R A D I C _ C L I E N T S                      --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2006-2012, Free Software Foundation, Inc.          --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.                                               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

with CORBA;
with RTCORBA;

package Sporadic_Clients is

   procedure Run_Test_1
     (Stamp             : Standard.String;
      Worker_String_Ref : CORBA.String;
      How_Many          : Positive);

   procedure Run_Test_1b
     (Stamp                        : Standard.String;
      Worker_String_Ref            : CORBA.String;
      Worker_Priority              : RTCORBA.Priority;
      Background_Worker_String_Ref : CORBA.String;
      Background_Worker_Priority   : RTCORBA.Priority;
      How_Many                     : Positive);

   procedure Run_Test_2
     (Stamp             : Standard.String;
      Worker_String_Ref : CORBA.String;
      How_Many          : Positive);

   procedure Run_Test_3
     (Stamp             : Standard.String;
      Worker_String_Ref : CORBA.String;
      How_Many          : Positive;
      Iterations        : Natural);

end Sporadic_Clients;
