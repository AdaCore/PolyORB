------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--                   S Y S T E M . G A R L I C . T H I N                    --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 1996-2006 Free Software Foundation, Inc.           --
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

with System.Garlic.Debug;
pragma Warnings (Off, System.Garlic.Debug);

package body System.Garlic.Thin is

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      null;
   end Initialize;

   --------------
   -- Shutdown --
   --------------

   procedure Shutdown is
   begin
      null;
   end Shutdown;

   -----------
   -- Clear --
   -----------

   procedure Clear  (FS : in out Fd_Set) is
   begin
      FS := 0;
   end Clear;

   ---------
   -- Set --
   ---------

   procedure Set    (FS : in out Fd_Set; Socket : Socket_Fd) is
   begin
      FS := FS + 2 ** Integer (Socket);
   end Set;

   ------------
   -- Is_Set --
   ------------

   function  Is_Set (FS : Fd_Set; Socket : Socket_Fd)
     return Boolean is
   begin
      return (FS / 2 ** Natural (Socket)) mod 2 = 1;
   end Is_Set;

end System.Garlic.Thin;
