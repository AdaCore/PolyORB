------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                         E V O L U T E D _ P K G                          --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2002-2008, Free Software Foundation, Inc.          --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
-- not, write to the Free Software Foundation, 59 Temple Place - Suite 330, --
-- Boston, MA 02111-1307, USA.                                              --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
--                PolyORB is maintained by ACT Europe.                      --
--                    (email: sales@act-europe.fr)                          --
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
