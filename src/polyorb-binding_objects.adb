------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--              P O L Y O R B . B I N D I N G _ O B J E C T S               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2004 Free Software Foundation, Inc.             --
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

--  Binding objects: protocol stacks seen globally as a reference-counted
--  entity.

--  $Id$

with Ada.Exceptions;

with PolyORB.Filters.Interface;
with PolyORB.Components;
with PolyORB.Log;

package body PolyORB.Binding_Objects is

   use PolyORB.Log;

   package L is new PolyORB.Log.Facility_Log ("polyorb.binding_objects");
   procedure O (Message : in String; Level : Log_Level := Debug)
     renames L.Output;

   use type PolyORB.Components.Component_Access;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (X : in out Binding_Object) is
      use PolyORB.Components;

   begin
      pragma Debug (O ("Finalizing binding object."));
      Emit_No_Reply (Component_Access (X.Transport_Endpoint),
                     Filters.Interface.Disconnect_Indication'(null record));
      pragma Debug (O ("Destroying protocol stack"));
      Destroy (Component_Access (X.Transport_Endpoint));
      --  This will recursively destroy all the protocol stack.
      pragma Debug (O ("RIP."));
   exception
      when E : others =>
         pragma Debug (O ("Finalization of binding object raised: "
                          & Ada.Exceptions.Exception_Information (E)));
         raise;
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

   --------------------------
   -- Setup_Binding_Object --
   --------------------------

   procedure Setup_Binding_Object
     (The_ORB :        ORB.ORB_Access;
      TE      :        Transport.Transport_Endpoint_Access;
      FFC     :        Filters.Factory_Array;
      Role    :        ORB.Endpoint_Role;
      BO_Ref  :    out Smart_Pointers.Ref)
   is
      BO : Binding_Object_Access;
      Bottom : Filters.Filter_Access;
   begin
      BO  := new Binding_Object;
      Smart_Pointers.Set (BO_Ref, Smart_Pointers.Entity_Ptr (BO));

      BO.Transport_Endpoint := TE;
      Filters.Create_Filter_Chain
        (FFC,
         Bottom => Bottom,
         Top    => BO.Top);

      Transport.Connect_Upper
        (TE, Components.Component_Access (Bottom));
      Filters.Connect_Lower
        (Bottom, Components.Component_Access (TE));

      ORB.Register_Binding_Object
        (The_ORB,
         BO_Ref,
         Role);
   end Setup_Binding_Object;

end PolyORB.Binding_Objects;
