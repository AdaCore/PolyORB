------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--             S Y S T E M . G A R L I C . S O F T _ L I N K S              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision$
--                                                                          --
--         Copyright (C) 1996-1999 Free Software Foundation, Inc.           --
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

package System.Garlic.Soft_Links is

   pragma Elaborate_Body;

   --  This package allows soft links to be defined and later called if they
   --  have been installed. The purpose of this is to be able not to register
   --  certain services that require tasking or other high-level features
   --  in order to provide the user with a light run time system.

   -------------------
   -- General types --
   -------------------

   type Parameterless_Procedure is access procedure;

   --------------------------
   -- Termination services --
   --------------------------

   --  Comments relative to these subprograms are located in s-garter.ads

   procedure Register_Add_Non_Terminating_Task
     (P : in Parameterless_Procedure);
   procedure Add_Non_Terminating_Task;

   procedure Register_Sub_Non_Terminating_Task
     (P : in Parameterless_Procedure);
   procedure Sub_Non_Terminating_Task;

   procedure Register_Termination_Initialize
     (P : in Parameterless_Procedure);
   procedure Termination_Initialize;

   procedure Register_Activity_Detected
     (P : in Parameterless_Procedure);
   procedure Activity_Detected;

   procedure Register_Local_Termination
     (P : in Parameterless_Procedure);
   procedure Local_Termination;

   procedure Register_Global_Termination
     (P : in Parameterless_Procedure);
   procedure Global_Termination;

   -------------------------------
   -- Critical section handling --
   -------------------------------

   procedure Register_Enter_Critical_Section
     (P : in Parameterless_Procedure);
   procedure Enter_Critical_Section;

   procedure Register_Leave_Critical_Section
     (P : in Parameterless_Procedure);
   procedure Leave_Critical_Section;

   -------------------------
   -- Shutdown mechanisms --
   -------------------------

   procedure Register_RPC_Shutdown
     (P : in Parameterless_Procedure);
   procedure RPC_Shutdown;

end System.Garlic.Soft_Links;
