------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                         E V O L U T E D _ P K G                          --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2002-2012, Free Software Foundation, Inc.          --
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

with Common; use Common;

package Evoluted_Pkg is

   ----------------------------------------------
   -- Global variables for automatic test mode --
   ----------------------------------------------

   protected Received_Counter is
      procedure Set_Expected (N : Integer);
      procedure Message_Received;
      entry Wait_For_Completion;
   private
      Expected : Integer := 0;
   end Received_Counter;

   type Instrumented_Penpal is new Penpal_Type with null record;
   procedure New_Message
     (Sender    : String;
      Recipient : access Instrumented_Penpal;
      Message   : String);

   Penpal : aliased Instrumented_Penpal;
   --  The penpal representing the user

   procedure Mainloop;
   --  Enter the mainloop

end Evoluted_Pkg;
