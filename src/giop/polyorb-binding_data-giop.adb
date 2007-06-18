------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--            P O L Y O R B . B I N D I N G _ D A T A . G I O P             --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2004-2007, Free Software Foundation, Inc.          --
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

with PolyORB.Binding_Objects;

package body PolyORB.Binding_Data.GIOP is

   use PolyORB.Errors;
   use PolyORB.GIOP_P.Tagged_Components;
   use PolyORB.GIOP_P.Transport_Mechanisms;

   use PolyORB.Objects;
   use PolyORB.Types;

   ------------------
   -- Bind_Profile --
   ------------------

   procedure Bind_Profile
     (Profile : access GIOP_Profile_Type;
      The_ORB : Components.Component_Access;
      QoS     : PolyORB.QoS.QoS_Parameters;
      BO_Ref  : out Smart_Pointers.Ref;
      Error   : out Errors.Error_Container)
   is
      use PolyORB.Binding_Objects;
      use PolyORB.Protocols.GIOP;

      use Transport_Mechanism_Lists;

      Iter : Transport_Mechanism_Lists.Iterator := First (Profile.Mechanisms);

   begin
      --  Go through all transport mechanism and try to bind it until the
      --  operation completes successfully.

      --  XXX This is a temporary implementation. It is not conformant
      --  with PortableInterceptors and RebindPolicy specifications.

      Throw (Error, No_Resources_E,
             System_Exception_Members'
             (Minor => 0, Completed => Completed_Maybe));

      while not Last (Iter) loop
         if Is_Security_Selected = null
           or else Is_Security_Selected (QoS, Value (Iter).all)
         then
            Catch (Error);
            Bind_Mechanism
              (Value (Iter).all.all, Profile, The_ORB, QoS, BO_Ref, Error);

            exit when not Found (Error);
         end if;

         Next (Iter);
      end loop;

      if not Found (Error) then
         Locate_Object
           (GIOP_Session (Get_Component (BO_Ref).all)'Access,
            Profile_Access (Profile), Error);
      end if;
   end Bind_Profile;

   ----------------------
   -- Get_GIOP_Version --
   ----------------------

   function Get_GIOP_Version
     (P : GIOP_Profile_Type) return Protocols.GIOP.GIOP_Version
   is
      use Protocols.GIOP;
      Minor : constant Integer := Integer (P.Version_Minor);
   begin
      pragma Assert (P.Version_Major = 1);
      if Minor in To_GIOP_Version'Range then
         return To_GIOP_Version (Minor);
      else
         raise GIOP_Error; --  with "unsupported GIOP version 1." & Minor'Img;
      end if;
   end Get_GIOP_Version;

   ------------------
   -- Is_Colocated --
   ------------------

   function Is_Colocated
     (Left  : GIOP_Profile_Type;
      Right : Profile_Type'Class) return Boolean
   is
   begin
      if Right not in GIOP_Profile_Type'Class then
         return False;
      end if;

      --  Compare transport mechanisms

      declare
         L_Mechanisms, R_Mechanisms : Transport_Mechanism_List;
      begin
         L_Mechanisms := Left.Mechanisms;
         R_Mechanisms := GIOP_Profile_Type (Right).Mechanisms;
         return Is_Colocated (L_Mechanisms, R_Mechanisms);
      end;
   end Is_Colocated;

   ----------------------
   -- Is_Local_Profile --
   ----------------------

   function Is_Local_Profile
     (PF : access GIOP_Profile_Factory;
      P  : access Profile_Type'Class)
      return Boolean
   is
      use Transport_Mechanism_Lists;
      use Transport_Mechanism_Factory_Lists;

      F_Iter : Transport_Mechanism_Factory_Lists.Iterator
        := First (PF.Mechanisms);

   begin
      if P.all not in GIOP_Profile_Type'Class then
         return False;
      end if;

      --  Profile designates a local object if at least one of its
      --  transport mechanism is local.

      while not Last (F_Iter) loop
         declare
            M_Iter : Transport_Mechanism_Lists.Iterator
              := First (GIOP_Profile_Type (P.all).Mechanisms);

         begin
            while not Last (M_Iter) loop
               if Is_Local_Mechanism
                    (Value (F_Iter).all, Value (M_Iter).all)
               then
                  return True;
               end if;

               Next (M_Iter);
            end loop;
         end;

         Next (F_Iter);
      end loop;

      return False;
   end Is_Local_Profile;

   -------------------
   -- Get_Component --
   -------------------

   function Get_Component
     (P : GIOP_Profile_Type;
      C : Tag_Value)
      return Tagged_Component_Access
   is
   begin
      return Get_Component (P.Components, C);
   end Get_Component;

   -------------------------------------
   -- Get_Primary_Transport_Mechanism --
   -------------------------------------

   function Get_Primary_Transport_Mechanism
     (P : GIOP_Profile_Type)
      return PolyORB.GIOP_P.Transport_Mechanisms.Transport_Mechanism_Access
   is
   begin
      return Element (P.Mechanisms, 0).all;
   end Get_Primary_Transport_Mechanism;

   ---------------------------------------------
   -- Get_Primary_Transport_Mechanism_Factory --
   ---------------------------------------------

   function Get_Primary_Transport_Mechanism_Factory
     (P : GIOP_Profile_Factory)
      return
        PolyORB.GIOP_P.Transport_Mechanisms.Transport_Mechanism_Factory_Access
   is
   begin
      return Element (P.Mechanisms, 0).all;
   end Get_Primary_Transport_Mechanism_Factory;

   -------------
   -- Release --
   -------------

   procedure Release (P : in out GIOP_Profile_Type) is
   begin
      Free (P.Object_Id);
      PolyORB.Annotations.Destroy (P.Notepad);
      Release_Contents (P.Components);
      Release_Contents (P.Mechanisms);
   end Release;

end PolyORB.Binding_Data.GIOP;
