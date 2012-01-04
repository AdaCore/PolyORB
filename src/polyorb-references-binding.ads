------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--           P O L Y O R B . R E F E R E N C E S . B I N D I N G            --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2001-2012, Free Software Foundation, Inc.          --
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

--  Object references (binding operation).

with PolyORB.Binding_Data;
with PolyORB.Components;
with PolyORB.Errors;
with PolyORB.ORB;
with PolyORB.QoS;

package PolyORB.References.Binding is

   pragma Elaborate_Body;

   procedure Bind
     (R          :        Ref'Class;
      Local_ORB  :        ORB.ORB_Access;
      QoS        :        PolyORB.QoS.QoS_Parameters;
      Servant    :    out Components.Component_Access;
      Pro        :    out Binding_Data.Profile_Access;
      Local_Only :        Boolean;
      Error      : in out PolyORB.Errors.Error_Container);
   --  Bind R to a servant, and return that servant (or a surrogate thereof)
   --  and the object id corresponding to the profile of R that was used.
   --  Local_ORB is the local middleware. It is used to determine whether
   --  reference profiles are local. Its object adapter is queried to resolve
   --  local object ids into servants.

   --  When a remote reference is to be bound, Local_ORB is in charge of all
   --  the transport and communication aspects of the binding operation. It
   --  must then return a remote surrogate of the object designated by R. If
   --  Local_Only is set to True, no remote binding is done. In that case, only
   --  references to local objects can be bound, and the returned Servant will
   --  be an actual local servant (not a surrogate).

   procedure Unbind (R : Ref'Class);
   --  Dissociate R from its continuation

   procedure Get_Tagged_Profile
     (R         :        Ref;
      Tag       :        Binding_Data.Profile_Tag;
      Pro       :    out Binding_Data.Profile_Access;
      Error     : in out PolyORB.Errors.Error_Container);
   --  Find a profile in R that matches Tag, and return it. If R has no profile
   --  with a matching tag, create a proxy profile that designates R using this
   --  ORB as a proxy. If R has no profile matching Tag, and this ORB cannot
   --  behave as a proxy either, null is returned.

   function Get_Preferred_Profile
     (R            : Ref'Class;
      Ignore_Local : Boolean)
      return Binding_Data.Profile_Access;
   --  Compute preferred profile which will be used in object binding
   --  operation. If Ignore_Local is True then ignore local profile even if it
   --  is a most preferred profile.

end PolyORB.References.Binding;
