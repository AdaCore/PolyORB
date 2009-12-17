------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--              P O L Y O R B . B I N D I N G _ O B J E C T S               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2004-2009, Free Software Foundation, Inc.          --
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

--  Binding object: A protocol stacks considered as a reference-counted entity

with PolyORB.Errors;
with PolyORB.Filters.Iface;
with PolyORB.Log;
with PolyORB.ORB;

package body PolyORB.Binding_Objects is

   use PolyORB.Log;

   package L is new PolyORB.Log.Facility_Log ("polyorb.binding_objects");
   procedure O (Message : String; Level : Log_Level := Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
     renames L.Enabled;

   use PolyORB.Binding_Data;

   use type PolyORB.Components.Component_Access;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (X : in out Binding_Object) is
      use PolyORB.Annotations;
      use PolyORB.Components;
      use PolyORB.Errors;

      Error : Error_Container;
   begin
      pragma Debug (C, O ("Finalizing binding object"));

      --  First remove the reference to this BO from its ORB so that is does
      --  not get reused while being finalized.

      ORB.Unregister_Binding_Object
        (ORB.ORB_Access (X.ORB), X'Unchecked_Access);

      --  Notify protocol stack that it is about to be dismantled

      Throw (Error,
        Comm_Failure_E,
        System_Exception_Members'(Minor => 0, Completed => Completed_Maybe));

      Emit_No_Reply (Component_Access (X.Transport_Endpoint),
                     Filters.Iface.Disconnect_Indication'(Error => Error));

      --  Destroy the transport endpoint at the bottom of the protocol stack
      --  (and all other components connected up).

      pragma Debug (C, O ("Destroying protocol stack"));
      Transport.Destroy (X.Transport_Endpoint);

      --  Finalize the data (profile and annotations)

      if X.Profile /= null then
         Destroy_Profile (X.Profile);
      end if;

      Destroy (X.Notepad);

      pragma Debug (C, O ("RIP"));
   end Finalize;

   -------------------
   -- Get_Component --
   -------------------

   function Get_Component (X : Smart_Pointers.Ref)
      return Components.Component_Access
   is
   begin
      return Components.Component_Access
        (Binding_Object_Access (Smart_Pointers.Entity_Of (X)).Top);
   end Get_Component;

   ------------------
   -- Get_Endpoint --
   ------------------

   function Get_Endpoint (X : Smart_Pointers.Ref)
      return Transport.Transport_Endpoint_Access
   is
   begin
      return Binding_Object_Access
        (Smart_Pointers.Entity_Of (X)).Transport_Endpoint;
   end Get_Endpoint;

   -----------------
   -- Get_Profile --
   -----------------

   function Get_Profile (BO : Binding_Object_Access)
      return Binding_Data.Profile_Access is
   begin
      return BO.Profile;
   end Get_Profile;

   ----------
   -- Link --
   ----------

   function Link
     (X     : access Binding_Object'Class;
      Which : Utils.Ilists.Link_Type) return access Binding_Object_Access
   is
   begin
      return X.Links (Which)'Unchecked_Access;
   end Link;

   ----------------
   -- Referenced --
   ----------------

   function Referenced (BO : Binding_Object_Access) return Boolean is
   begin
      return BO.Referenced;
   end Referenced;

   --------------------
   -- Set_Referenced --
   --------------------

   procedure Set_Referenced
     (BO         : Binding_Object_Access;
      Referenced : Boolean)
   is
   begin
      BO.Referenced := Referenced;
   end Set_Referenced;

   --------------------------
   -- Setup_Binding_Object --
   --------------------------

   procedure Setup_Binding_Object
     (ORB     : Components.Component_Access;
      TE      : Transport.Transport_Endpoint_Access;
      FFC     : Filters.Factory_Array;
      BO_Ref  : out Smart_Pointers.Ref;
      Pro     : Binding_Data.Profile_Access)
   is
      BO : Binding_Object_Access;
      Bottom : Filters.Filter_Access;
   begin
      BO  := new Binding_Object;

      Smart_Pointers.Set (BO_Ref, Smart_Pointers.Entity_Ptr (BO));

      Set_Profile (BO, Pro);

      BO.ORB := ORB;
      BO.Transport_Endpoint := TE;
      Filters.Create_Filter_Chain
        (FFC,
         Bottom => Bottom,
         Top    => BO.Top);

      Transport.Connect_Upper
        (TE, Components.Component_Access (Bottom));
      Filters.Connect_Lower
        (Bottom, Components.Component_Access (TE));

   end Setup_Binding_Object;

   -----------------
   -- Set_Profile --
   -----------------

   procedure Set_Profile
     (BO : Binding_Object_Access; P : Binding_Data.Profile_Access) is
   begin
      if BO.Profile /= null then
         Destroy_Profile (BO.Profile);
      end if;

      --  We need to take a copy of P, rather than point into the original
      --  reference that was used to create this binding object, since the
      --  original reference may be destroyed after the binding object gets
      --  reused for another reference.

      if P /= null then
         BO.Profile := Duplicate_Profile (P.all);
      else
         BO.Profile := null;
      end if;
   end Set_Profile;

   ----------------
   -- Notepad_Of --
   ----------------

   function Notepad_Of (BO : Binding_Object_Access)
     return Annotations.Notepad_Access is
   begin
      return BO.Notepad'Access;
   end Notepad_Of;

   -----------
   -- Valid --
   -----------

   function Valid (BO : Binding_Object_Access) return Boolean is
      use Components;
      use Filters.Iface;

      Reply : constant Message'Class :=
                Emit (Component_Access (BO.Top), Check_Validity'(null record));
   begin
      return Reply in Components.Null_Message;
   end Valid;

end PolyORB.Binding_Objects;
