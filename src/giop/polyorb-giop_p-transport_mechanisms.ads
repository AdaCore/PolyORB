------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--  P O L Y O R B . G I O P _ P . T R A N S P O R T _ M E C H A N I S M S   --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--            Copyright (C) 2005 Free Software Foundation, Inc.             --
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
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

--  Abstraction for GIOP Transport Mechanisms.

with PolyORB.Binding_Data;
with PolyORB.Components;
with PolyORB.Errors;
with PolyORB.GIOP_P.Tagged_Components;
with PolyORB.Smart_Pointers;
with PolyORB.Transport;
with PolyORB.Utils.Chained_Lists;

package PolyORB.GIOP_P.Transport_Mechanisms is

   --  Transport mechanism

   type Transport_Mechanism is abstract tagged null record;

   type Transport_Mechanism_Access is access all Transport_Mechanism'Class;

   procedure Bind_Mechanism
     (Mechanism : Transport_Mechanism;
      Profile   : access PolyORB.Binding_Data.Profile_Type'Class;
      The_ORB   : Components.Component_Access;
      BO_Ref    : out Smart_Pointers.Ref;
      Error     : out Errors.Error_Container) is abstract;
   --  Create a transport endpoint and an attached protocol stack instance
   --  that match this transport mechanism, in order to send a message
   --  to the middleware that hosts the designated object.
   --  The Filter at the top of the protocol stack is returned.
   --  Concrete implementations are responsible for registering the
   --  Transport Endpoint with the ORB if necessary.

   procedure Release_Contents (M : access Transport_Mechanism) is abstract;

   --  Transport mechanism factory

   type Transport_Mechanism_Factory is abstract tagged null record;

   type Transport_Mechanism_Factory_Access is
     access all Transport_Mechanism_Factory'Class;

   procedure Create_Factory
     (MF  : out Transport_Mechanism_Factory;
      TAP :     Transport.Transport_Access_Point_Access) is abstract;
   --  Initialize MF to act as transport mechanism factory for
   --  transport access point TAP

   function Is_Local_Mechanism
     (MF : access Transport_Mechanism_Factory;
      M  : access Transport_Mechanism'Class)
      return Boolean is abstract;
   --  True iff M designates an mechanism that can be contacted
   --  at the access point associated with MF

   function Create_Tagged_Components
     (MF : Transport_Mechanism_Factory)
      return Tagged_Components.Tagged_Component_List is abstract;
   --  Create tagged components, which represent transport mechanism's
   --  transport access points and association options in the object profile

   --  List of Transport Mechanisms

   package Transport_Mechanism_Lists is
     new PolyORB.Utils.Chained_Lists (Transport_Mechanism_Access);

   type Transport_Mechanism_List is new Transport_Mechanism_Lists.List;

   procedure Release_Contents (List : in out Transport_Mechanism_List);
   --  Free memory for all tags in List

   function Deep_Copy
     (List : Transport_Mechanism_List)
     return Transport_Mechanism_List;
   --  Return a deep copy of list

   function Duplicate
     (TMA : Transport_Mechanism)
     return Transport_Mechanism is abstract;

   --  List of Transport Mechanism Factories

   package Transport_Mechanism_Factory_Lists is
     new PolyORB.Utils.Chained_Lists (Transport_Mechanism_Factory_Access);

   type Transport_Mechanism_Factory_List is
     new Transport_Mechanism_Factory_Lists.List;

   --  Creation of Transport Mechanisms from list of Tagged Component

   function Create_Transport_Mechanisms
     (TC      : Tagged_Components.Tagged_Component_List;
      Profile : Binding_Data.Profile_Access)
     return Transport_Mechanism_List;
   --  Create list of profile's Transport Mechanisms from profile's list
   --  of Tagged Components

   type Transport_Mechanism_Constructor is
     access function
     (TC      : Tagged_Components.Tagged_Component_Access;
      Profile : Binding_Data.Profile_Access)
     return Transport_Mechanism_List;

   procedure Register
    (Tag         : Tagged_Components.Tag_Value;
     Constructor : Transport_Mechanism_Constructor);
   --  Register tagged component to transport mechanism convertor

end PolyORB.GIOP_P.Transport_Mechanisms;
