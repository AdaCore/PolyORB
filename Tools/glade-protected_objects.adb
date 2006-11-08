------------------------------------------------------------------------------
--                                                                          --
--                              GLADE TOOLS                                 --
--                                                                          --
--              G L A D E . P R O T E C T E D _ O B J E C T S               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 1996-2006 Free Software Foundation, Inc.           --
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

package body GLADE.Protected_Objects is

   ------------------
   -- Barrier_Type --
   ------------------

   protected body Barrier_Type is

      ------------
      -- Signal --
      ------------

      procedure Signal
        (At_Least_One : Boolean := False;
         All_Clients  : Boolean := False)
      is
      begin
         if At_Least_One or else Wait'Count > 0 then
            Signaled := True;
            For_All  := All_Clients;
         end if;
      end Signal;

      ----------
      -- Wait --
      ----------

      entry Wait when Signaled is
      begin
         if Wait'Count = 0 or else not For_All then
            Signaled := False;
         end if;
      end Wait;

   end Barrier_Type;

   ----------------
   -- Mutex_Type --
   ----------------

   protected body Mutex_Type is

      ------------
      --  Enter --
      ------------

      entry Enter when not Locked is
      begin
         Locked := True;
      end Enter;

      -----------
      -- Leave --
      -----------

      procedure Leave is
      begin
         Locked := False;
      end Leave;

   end Mutex_Type;

end GLADE.Protected_Objects;
