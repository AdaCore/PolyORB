------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--              S Y S T E M . G A R L I C . P R O T O C O L S               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision$
--                                                                          --
--         Copyright (C) 1996-2000 Free Software Foundation, Inc.           --
--                                                                          --
-- GARLIC is free software;  you can redistribute it and/or modify it under --
-- terms of the  GNU General Public License  as published by the Free Soft- --
-- ware Foundation;  either version 2,  or (at your option)  any later ver- --
-- sion.  GARLIC is distributed  in the hope that  it will be  useful,  but --
-- WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHANTABI- --
-- LITY or  FITNESS FOR A PARTICULAR PURPOSE.  See the  GNU General Public  --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License  distributed with GARLIC;  see file COPYING.  If  --
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

package body System.Garlic.Protocols is

   --------------
   -- Get_Data --
   --------------

   function Get_Data
     (Protocol  : access Protocol_Type)
     return Utils.String_Array_Access is
   begin
      return null;
   end Get_Data;

   --------------------------------
   -- Receive_From_All_Protocols --
   --------------------------------

   procedure Receive_From_All_Protocols
   is
      Timeout  : Duration := Polling;
      Protocol : Natural  := First_Protocol;

   begin
      if First_Protocol = Last_Protocol then
         Timeout := Forever;
      end if;
      loop
         exit when Receive (Protocol_Table (Protocol), Timeout);
         if Protocol = Last_Protocol then
            Protocol := First_Protocol;
         else
            Protocol := Protocol + 1;
         end if;
      end loop;
   end Receive_From_All_Protocols;

   --------------
   -- Register --
   --------------

   procedure Register (Protocol : in Protocol_Access) is
   begin
      if Protocol = null then
         return;
      end if;
      Last_Protocol := Last_Protocol + 1;
      Protocol_Table (Last_Protocol) := Protocol;
   end Register;

   --------------
   -- Shutdown --
   --------------

   procedure Shutdown is
   begin
      for I in First_Protocol .. Last_Protocol loop
         Shutdown (Protocol_Table (I));
      end loop;
   end Shutdown;

end System.Garlic.Protocols;

