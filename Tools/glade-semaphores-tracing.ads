------------------------------------------------------------------------------
--                                                                          --
--                              GLADE TOOLS                                 --
--                                                                          --
--             G L A D E . S E M A P H O R E S . T R A C I N G              --
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

package GLADE.Semaphores.Tracing is

   --  This package provides a way of registering a callback that will be
   --  called in order to trace the behaviour of the algorithm.

   pragma Remote_Call_Interface;
   --  Comment this one to be able to work without GLADE (RAS are the only
   --  thing that does not work without GLADE even in a non-distributed
   --  environment)

   type Operation_Type is
      (Initialize_Semaphore,
       P,
       V,
       Ask_For_Semaphore,
       Promise_Semaphore,
       Acquire_Semaphore,
       Abandon_Semaphore,
       Set_Probable_Owner,
       Forward_Request,
       Comparaison);
   --  Current operation type

   type Callback_Type is
      access procedure (Actor     : in Semaphore_Access;
                        Operation : in Operation_Type;
                        Data      : in Semaphore_Access);
   --  The user must furnish such a callback

   procedure Register_Callback (Callback : in Callback_Type);
   --  Set the current callback

   procedure Record_Transaction (Actor     : in Semaphore_Access;
                                 Operation : in Operation_Type;
                                 Data      : in Semaphore_Access := null);
   --  Record a transaction using the previously registered callback

end GLADE.Semaphores.Tracing;
