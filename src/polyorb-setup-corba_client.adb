------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--           P O L Y O R B . S E T U P . C O R B A _ C L I E N T            --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                Copyright (C) 2001 Free Software Fundation                --
--                                                                          --
-- AdaBroker is free software; you  can  redistribute  it and/or modify it  --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. AdaBroker  is distributed  in the hope that it will be  useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with AdaBroker; see file COPYING. If  --
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
--              PolyORB is maintained by ENST Paris University.             --
--                                                                          --
------------------------------------------------------------------------------

--  Set up a test ORB.

--  $Id$

with Ada.Text_IO; use Ada.Text_IO;

with PolyORB.Binding_Data.IIOP;
pragma Elaborate_All (PolyORB.Binding_Data.IIOP);

with PolyORB.Protocols.GIOP;
pragma Elaborate_All (PolyORB.Protocols.GIOP);

with PolyORB.Smart_Pointers;
pragma Elaborate_All (PolyORB.Smart_Pointers);

with PolyORB.No_Tasking;
with PolyORB.ORB.Task_Policies;

with PolyORB.ORB;
pragma Elaborate_All (PolyORB.ORB);

with PolyORB.Binding_Data.SOAP;
pragma Elaborate_All (PolyORB.Binding_Data.SOAP);

package body PolyORB.Setup.CORBA_Client is

   use PolyORB.ORB;

   procedure Initialize_CORBA_Client
     (SL_Init : Parameterless_Procedure;
      TP : ORB.Tasking_Policy_Access) is
   begin

      -------------------------------
      -- Initialize all subsystems --
      -------------------------------

      Put ("Initializing subsystems...");

      SL_Init.all;
      Put (" soft-links");
      --  Setup soft links.

      PolyORB.Smart_Pointers.Initialize;
      Put (" smart-pointers");
      --  Depends on Soft_Links.

      -------------------------------------------
      -- Initialize personality-specific stuff --
      -------------------------------------------

      PolyORB.Binding_Data.SOAP.Initialize;
      Put (" binding-soap");

      PolyORB.Binding_Data.IIOP.Initialize;
      Put (" binding-iiop");

      PolyORB.Protocols.GIOP.Initialize;
      Put (" protocols-giop");

      --------------------------
      -- Create ORB singleton --
      --------------------------

      Setup.The_ORB := new ORB.ORB_Type (TP);

      PolyORB.ORB.Create (Setup.The_ORB.all);
      Put (" ORB");

      Put_Line (" done");
   end Initialize_CORBA_Client;

begin

   PolyORB.Setup.CORBA_Client.Initialize_CORBA_Client
     (PolyORB.No_Tasking.Initialize'Access,
      new PolyORB.ORB.Task_Policies.No_Tasking);

end PolyORB.Setup.CORBA_Client;
