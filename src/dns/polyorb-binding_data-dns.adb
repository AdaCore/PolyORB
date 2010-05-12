------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--            P O L Y O R B . B I N D I N G _ D A T A . D N S              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2010, Free Software Foundation, Inc.          --
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

package body PolyORB.Binding_Data.DNS is

   use PolyORB.Errors;
   use PolyORB.DNS.Transport_Mechanisms;
   use PolyORB.Objects;
   use PolyORB.Types;

   ------------------
   -- Bind_Profile --
   ------------------

   procedure Bind_Profile
     (Profile : access DNS_Profile_Type;
      The_ORB : Components.Component_Access;
      QoS     : PolyORB.QoS.QoS_Parameters;
      BO_Ref  : out Smart_Pointers.Ref;
      Error   : out Errors.Error_Container)
   is
      use Transport_Mechanism_Lists;
      Iter : Transport_Mechanism_Lists.Iterator := First (Profile.Mechanisms);

   begin
      Throw (Error, No_Resources_E,
             System_Exception_Members'
             (Minor => 0, Completed => Completed_Maybe));

      while not Last (Iter) loop

         Catch (Error);
         Bind_Mechanism
              (Value (Iter).all.all, Profile, The_ORB, QoS, BO_Ref, Error);
         exit when not Found (Error);
         Next (Iter);
      end loop;
   end Bind_Profile;

   ------------------
   -- Is_Colocated --
   ------------------

   function Is_Colocated
     (Left  : DNS_Profile_Type;
      Right : Profile_Type'Class) return Boolean
   is
   begin
      if Right not in DNS_Profile_Type'Class then
         return False;
      end if;

      --  Compare transport mechanisms

      declare
         L_Mechanisms, R_Mechanisms : Transport_Mechanism_List;
      begin
         L_Mechanisms := Left.Mechanisms;
         R_Mechanisms := DNS_Profile_Type (Right).Mechanisms;
         return Is_Colocated (L_Mechanisms, R_Mechanisms);
      end;
   end Is_Colocated;

   ----------------------
   -- Is_Local_Profile --
   ----------------------

   function Is_Local_Profile
     (PF : access DNS_Profile_Factory;
      P  : access Profile_Type'Class)
      return Boolean
   is
      use Transport_Mechanism_Lists;
      use Transport_Mechanism_Factory_Lists;

      F_Iter : Transport_Mechanism_Factory_Lists.Iterator
        := First (PF.Mechanisms);

   begin
      if P.all not in DNS_Profile_Type'Class then
         return False;
      end if;

      --  Profile designates a local object if at least one of its
      --  transport mechanism is local.

      while not Last (F_Iter) loop
         declare
            M_Iter : Transport_Mechanism_Lists.Iterator
              := First (DNS_Profile_Type (P.all).Mechanisms);

         begin
            while not Last (M_Iter) loop
               if Is_Local_Mechanism
                    (Value (F_Iter).all, Value (M_Iter).all)
               then
                  P.Known_Local := True;
                  return True;
               end if;

               Next (M_Iter);
            end loop;
         end;

         Next (F_Iter);
      end loop;

      return False;
   end Is_Local_Profile;

   -------------------------------------
   -- Get_Primary_Transport_Mechanism --
   -------------------------------------

   function Get_Primary_Transport_Mechanism
     (P : DNS_Profile_Type)
      return PolyORB.DNS.Transport_Mechanisms.Transport_Mechanism_Access
   is
   begin
      return Element (P.Mechanisms, 0).all;
   end Get_Primary_Transport_Mechanism;

   ---------------------------------------------
   -- Get_Primary_Transport_Mechanism_Factory --
   ---------------------------------------------

   function Get_Primary_Transport_Mechanism_Factory
     (P : DNS_Profile_Factory)
      return
        PolyORB.DNS.Transport_Mechanisms.Transport_Mechanism_Factory_Access
   is
   begin
      return Element (P.Mechanisms, 0).all;
   end Get_Primary_Transport_Mechanism_Factory;

   -------------
   -- Release --
   -------------

   procedure Release (P : in out DNS_Profile_Type) is
   begin
      Free (P.Object_Id);
      PolyORB.Annotations.Destroy (P.Notepad);
      Release_Contents (P.Mechanisms);
   end Release;

end PolyORB.Binding_Data.DNS;
