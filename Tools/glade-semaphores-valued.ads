------------------------------------------------------------------------------
--                                                                          --
--                              GLADE TOOLS                                 --
--                                                                          --
--              G L A D E . S E M A P H O R E S . V A L U E D               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision$                             --
--                                                                          --
--         Copyright (C) 1996-2000 Free Software Foundation, Inc.           --
--                                                                          --
-- GLADE  is free software;  you can redistribute it and/or modify it under --
-- terms of the  GNU General Public License  as published by the Free Soft- --
-- ware Foundation;  either version 2,  or (at your option)  any later ver- --
-- sion.  GLADE  is distributed  in the hope that  it will be  useful,  but --
-- WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHANTABI- --
-- LITY or  FITNESS FOR A PARTICULAR PURPOSE.  See the  GNU General Public  --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License  distributed  with GLADE;  see file COPYING.  If  --
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
--               GLADE  is maintained by ACT Europe.                        --
--               (email: glade-report@act-europe.fr)                        --
--                                                                          --
------------------------------------------------------------------------------

with GLADE.Semaphores;

generic
   type Data_Type is private;
   Null_Data : Data_Type;

package GLADE.Semaphores.Valued is

   pragma Remote_Types;

   type Valued_Semaphore_Type is new Semaphores.Semaphore_Type with private;
   --  A valued semaphore is a semaphore that can hold a value with it.
   --  The owner of the semaphore at a given time can freely modify the
   --  data associated to the semaphore using the Set_Data and Get_Data
   --  subprograms below.

   procedure V (Semaphore : access Valued_Semaphore_Type);
   --  Release the semaphore. Program_Error is raised if no P operation
   --  has been performed on the semaphore.

   procedure Set_Data (Semaphore : access Valued_Semaphore_Type;
                       Data      : in Data_Type);
   --  Replace the data stored in the semaphore. If the semaphore has not
   --  been acquired at this time, a P operation will be performed first and
   --  the semaphore will be released after this procedure call.

   function Get_Data (Semaphore : access Valued_Semaphore_Type)
     return Data_Type;
   --  Get the data stored in the semaphore. The same behaviour as exposed
   --  above will arise if the semaphore has not been acquired at the time
   --  of the call.

   type Valued_Semaphore_Access is access all Valued_Semaphore_Type'Class;

private

   procedure Signal (Recipient : access Valued_Semaphore_Type;
                     Data      : in Data_Type);
   --  The semaphore can be used

   type Valued_Semaphore_Type is new Semaphores.Semaphore_Type with record
      Data : Data_Type := Null_Data;
   end record;

end GLADE.Semaphores.Valued;
