------------------------------------------------------------------------------
--                                                                          --
--                              GLADE TOOLS                                 --
--                                                                          --
--              G L A D E . V A L U E D _ S E M A P H O R E S               --
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

package GLADE.Valued_Semaphores is

   pragma Remote_Types;

   type Semaphore_Type is new GLADE.Semaphores.Semaphore_Type with private;
   --  Semaphore

   procedure Initialize
     (Semaphore : access Semaphore_Type;
      Name      : in String);
   --  Initialize a semaphore given its name. The same name must be used
   --  to have several virtual instances of the semaphore, which is quite
   --  useful. The private data of the semaphore will be set to Null_Data.

   procedure P (Semaphore : access Semaphore_Type);
   --  Get the semaphore. This is a blocking operation since the semaphore
   --  may be used by someone else at this time. Program_Error is raised if
   --  the semaphore has not been initialized.

   procedure V (Semaphore : access Semaphore_Type);
   --  Release the semaphore. Program_Error is raised if no P operation
   --  has been performed on the semaphore.

   function Acquired (Semaphore : access Semaphore_Type) return Boolean;
   --  Return True if the semaphore has been acquired at the time of the call

   procedure Set_Data (Semaphore : access Semaphore_Type;
		       Data      : in Data_Type);
   --  Replace the data stored in the semaphore. If the semaphore has not
   --  been acquired at this time, a P operation will be performed first and
   --  the semaphore will be released after this procedure call.

   function Get_Data (Semaphore : access Semaphore_Type) return Data_Type;
   --  Get the data stored in the semaphore. The same behaviour as exposed
   --  above will arise if the semaphore has not been acquired at the time
   --  of the call.

   type Semaphore_Access is access all Semaphore_Type'Class;

private

   procedure Send_Request_To (Recipient : access Semaphore_Type;
                              Client    : in Semaphore_Access);
   --  Send the request for the semaphore

   procedure Signal (Recipient : access Semaphore_Type;
		     Data      : in Data_Type);
   --  The semaphore can be used

   type Semaphore_Type is tagged limited record
      Is_Used        : Boolean := False;
      Probable_Owner : Semaphore_Access;
      Promised_To    : Semaphore_Access;
      Barrier        : Positive;
      Data           : Data_Type;
   end record;

end GLADE.Valued_Semaphores;
