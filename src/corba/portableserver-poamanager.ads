------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--            P O R T A B L E S E R V E R . P O A M A N A G E R             --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2001-2003 Free Software Foundation, Inc.           --
--                                                                          --
-- This specification is derived from the CORBA Specification, and adapted  --
-- for use with PolyORB. The copyright notice above, and the license        --
-- provisions that follow apply solely to the contents neither explicitely  --
-- nor implicitely specified by the CORBA Specification defined by the OMG. --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
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
--                PolyORB is maintained by ACT Europe.                      --
--                    (email: sales@act-europe.fr)                          --
--                                                                          --
------------------------------------------------------------------------------

--  $Id: //droopi/main/src/corba/portableserver-poamanager.ads#6 $

with Ada.Exceptions;

with CORBA.Object;

with PolyORB.Exceptions;
with PolyORB.POA_Manager;

package PortableServer.POAManager is

   type Ref is new CORBA.Object.Ref with null record;

   subtype State is PolyORB.POA_Manager.State;
   --  equivalent to
   --       type State is (HOLDING, ACTIVE, DISCARDING, INACTIVE);

   AdapterInactive : exception;

   procedure Activate (Self : in Ref);

   procedure Hold_Requests
     (Self                : in Ref;
      Wait_For_Completion : in CORBA.Boolean);

   procedure Discard_Requests
     (Self                : in Ref;
      Wait_For_Completion : in CORBA.Boolean);

   procedure Deactivate
     (Self                : in Ref;
      Etherealize_Objects : in CORBA.Boolean;
      Wait_For_Completion : in CORBA.Boolean);

   function Get_State (Self : in Ref) return State;

   --------------------------------------
   -- POAManager Exceptions Management --
   --------------------------------------

   procedure Raise_From_Error
     (Error : in out PolyORB.Exceptions.Error_Container);

   --  AdapterInactive

   type AdapterInactive_Members is
     new CORBA.IDL_Exception_Members with null record;

   procedure Get_Members
     (From : in  Ada.Exceptions.Exception_Occurrence;
      To   : out AdapterInactive_Members);

   procedure Raise_AdapterInactive
     (Excp_Memb : in AdapterInactive_Members);
   pragma No_Return (Raise_AdapterInactive);

end PortableServer.POAManager;
