------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                             T E S T _ J O B                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2003-2012, Free Software Foundation, Inc.          --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.                                               --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
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

with PolyORB.Utils.Report;

package body Test_Job is

   procedure Run_Job is
   begin
      PolyORB.Utils.Report.Output
        ("Invocation on servant finished",
         "Hello Ada World !" =
         CORBA.To_Standard_String
         (Echo.echoString (Global_Obj_Ref,
                           CORBA.To_CORBA_String ("Hello Ada World !"))));
   end Run_Job;

   procedure Run_Job_Wait is
   begin
      PolyORB.Utils.Report.Output
        ("Invocation on servant finished",
         "Hello Ada World !" =
         CORBA.To_Standard_String
         (Echo.echoString_wait
          (Global_Obj_Ref,
           CORBA.To_CORBA_String ("Hello Ada World !"))));
   end Run_Job_Wait;

end Test_Job;
