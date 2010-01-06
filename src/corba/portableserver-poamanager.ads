------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--            P O R T A B L E S E R V E R . P O A M A N A G E R             --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2001-2008, Free Software Foundation, Inc.          --
--                                                                          --
-- This specification is derived from the CORBA Specification, and adapted  --
-- for use with PolyORB. The copyright notice above, and the license        --
-- provisions that follow apply solely to the contents neither explicitly   --
-- nor implicitly specified by the CORBA Specification defined by the OMG.  --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
-- not, write to the Free Software Foundation, 51 Franklin Street, Fifth    --
-- Floor, Boston, MA 02111-1301, USA.                                       --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Exceptions;

with CORBA.Object;

with PolyORB.Errors;
with PolyORB.POA_Manager;

package PortableServer.POAManager is

   type Local_Ref is new CORBA.Object.Ref with null record;

   type State is new PolyORB.POA_Manager.State;
   --  equivalent to
   --       type State is (HOLDING, ACTIVE, DISCARDING, INACTIVE);

   AdapterInactive : exception;

   procedure Activate
     (Self : Local_Ref);

   procedure Hold_Requests
     (Self                : Local_Ref;
      Wait_For_Completion : CORBA.Boolean);

   procedure Discard_Requests
     (Self                : Local_Ref;
      Wait_For_Completion : CORBA.Boolean);

   procedure Deactivate
     (Self                : Local_Ref;
      Etherealize_Objects : CORBA.Boolean;
      Wait_For_Completion : CORBA.Boolean);

   function Get_State
     (Self : Local_Ref)
     return PortableServer.POAManager.State;

   --------------------------------------
   -- POAManager Exceptions Management --
   --------------------------------------

   procedure Raise_From_Error
     (Error   : in out PolyORB.Errors.Error_Container;
      Message : Standard.String);

   --  AdapterInactive

   type AdapterInactive_Members is
     new CORBA.IDL_Exception_Members with null record;

   procedure Get_Members
     (From : Ada.Exceptions.Exception_Occurrence;
      To   : out AdapterInactive_Members);

   procedure Raise_AdapterInactive
     (Excp_Memb : AdapterInactive_Members;
      Message   : Standard.String := "");
   pragma No_Return (Raise_AdapterInactive);

end PortableServer.POAManager;
