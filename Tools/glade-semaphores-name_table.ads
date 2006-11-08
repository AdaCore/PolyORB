------------------------------------------------------------------------------
--                                                                          --
--                              GLADE TOOLS                                 --
--                                                                          --
--          G L A D E . S E M A P H O R E S . N A M E _ T A B L E           --
--                                                                          --
--                                 S p e c                                  --
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

package GLADE.Semaphores.Name_Table is

   pragma Remote_Call_Interface;

   --  This package takes care of registering a probable owner for a
   --  semaphore when it gets created and redirects all the new requests to
   --  this probable owner.

   procedure Get_Probable_Owner (Semaphore          : in Semaphore_Access;
                                 Name               : in String;
                                 Probable_Owner     : out Semaphore_Access;
                                 Index              : out Positive);

end GLADE.Semaphores.Name_Table;
