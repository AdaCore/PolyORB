------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--            P O L Y O R B . B I N D I N G _ D A T A . G I O P             --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2004-2012, Free Software Foundation, Inc.          --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.                                               --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

--  ???pragma Ada_2005;
--  This pragma, and the "overriding" keywords below, are commented out because
--  of a compiler bug. In particular, if a program is compiled in Ada 95 mode,
--  and imports this package, we get an error on Is_Local_Profile, "not subtype
--  conformant with declaration at line 45", "type of "P" does not match".

with PolyORB.GIOP_P.Tagged_Components;
with PolyORB.GIOP_P.Transport_Mechanisms;
with PolyORB.Protocols.GIOP;

package PolyORB.Binding_Data.GIOP is

   package PGTC renames PolyORB.GIOP_P.Tagged_Components;
   package PGTM renames PolyORB.GIOP_P.Transport_Mechanisms;

   type GIOP_Profile_Type is abstract new Profile_Type with private;
   type GIOP_Profile_Factory is abstract new Profile_Factory with private;

   --  ???overriding (see above)
   procedure Bind_Profile
     (Profile : access GIOP_Profile_Type;
      The_ORB : Components.Component_Access;
      QoS     : PolyORB.QoS.QoS_Parameters;
      BO_Ref  : out Smart_Pointers.Ref;
      Error   : out Errors.Error_Container);

   --  ???overriding (see above)
   procedure Release (P : in out GIOP_Profile_Type);

   function Get_Component
     (P : GIOP_Profile_Type;
      C : PGTC.Tag_Value) return PGTC.Tagged_Component_Access;

   --  ???overriding (see above)
   function Is_Colocated
     (Left  : GIOP_Profile_Type;
      Right : Profile_Type'Class) return Boolean;

   --  ???overriding (see above)
   function Is_Local_Profile
     (PF : access GIOP_Profile_Factory;
      P  : access Profile_Type'Class) return Boolean;

   function Get_GIOP_Version
     (P : GIOP_Profile_Type) return Protocols.GIOP.GIOP_Version;
   --  Return the GIOP version indicated in profile P

   function Get_Primary_Transport_Mechanism
     (P : GIOP_Profile_Type) return PGTM.Transport_Mechanism_Access;
   --  Return primary transport mechanism for profile

   function Get_Primary_Transport_Mechanism_Factory
     (P : GIOP_Profile_Factory) return PGTM.Transport_Mechanism_Factory_Access;
   --  Return primary transport mechanism factory for profile factory

   type Is_Security_Selected_Hook is
     access function
     (QoS       : PolyORB.QoS.QoS_Parameters;
      Mechanism : PGTM.Transport_Mechanism_Access) return Boolean;

   Is_Security_Selected : Is_Security_Selected_Hook := null;
   --  This hook is used in profile binding procedure to avoid binding with
   --  transport mechanisms other than those selected by security service.
   --  Binding of such mechanisms may cause unexpected behavior because some
   --  security related information (credentials, for example) are unavailable.

private

   type GIOP_Profile_Type is abstract new Profile_Type with record
      Version_Major : Types.Octet;
      Version_Minor : Types.Octet;

      Components    : PGTC.Tagged_Component_List;
      --  Tagged components list

      Mechanisms    : PGTM.Transport_Mechanism_List;
      --  Transport mechanisms list
   end record;

   type GIOP_Profile_Factory is abstract new Profile_Factory with record
      Mechanisms : PGTM.Transport_Mechanism_Factory_List;
   end record;

end PolyORB.Binding_Data.GIOP;
