------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--         P O L Y O R B . Q O S . C L I E N T S _ S E C U R I T Y          --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2005-2006, Free Software Foundation, Inc.          --
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

with Ada.Unchecked_Deallocation;

with PolyORB.ASN1;

package body PolyORB.QoS.Clients_Security is

   use PolyORB.Annotations;
   use PolyORB.ASN1;
   use PolyORB.Security.Authentication_Mechanisms;
   use PolyORB.Security.Authority_Mechanisms;
   use PolyORB.Security.Authority_Mechanisms.Client_Authority_Mechanism_Lists;
   use PolyORB.Security.Transport_Mechanisms;
   use PolyORB.Security.Types;
   use OID_Lists;

   procedure Release_Contents (Item : in out Client_Mechanism);

   procedure Free is
     new Ada.Unchecked_Deallocation
        (Client_Mechanism, Client_Mechanism_Access);

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Mechanism : in out Client_Mechanism_Access) is
   begin
      if Mechanism /= null then
         Release_Contents (Mechanism.all);
         Free (Mechanism);
      end if;
   end Destroy;

   ------------------
   -- Is_Protected --
   ------------------

   function Is_Protected (Mechanism : Client_Mechanism) return Boolean is
   begin
      return Target_Requires (Mechanism) /= 0;
   end Is_Protected;

   ----------------------
   -- Release_Contents --
   ----------------------

   procedure Release_Contents (Item : in out Client_Mechanism) is

      procedure Free is
        new Ada.Unchecked_Deallocation
        (Client_Transport_Mechanism'Class, Client_Transport_Mechanism_Access);

   begin
      Free (Item.Transport);

      Destroy (Item.Authentication_Mechanism);

      declare
         Iter : Client_Authority_Mechanism_Lists.Iterator
           := First (Item.Authorities);

      begin
         while not Last (Iter) loop
            Destroy (Value (Iter).all);
            Next (Iter);
         end loop;
      end;

      Deallocate (Item.Authorities);

      declare
         Iter : OID_Lists.Iterator := First (Item.Naming_Mechanisms);

      begin
         while not Last (Iter) loop
            Free (Value (Iter).all);
            Next (Iter);
         end loop;
      end;

      Deallocate (Item.Naming_Mechanisms);

      Destroy (Item.Notepad);
   end Release_Contents;

   procedure Release_Contents (QoS : access QoS_Client_Security_Parameter) is
      use Client_Mechanism_Lists;

      Iter : Client_Mechanism_Lists.Iterator := First (QoS.Mechanisms);

   begin
      while not Last (Iter) loop
         Release_Contents (Value (Iter).all.all);
         Free (Value (Iter).all);

         Next (Iter);
      end loop;

      Deallocate (QoS.Mechanisms);
   end Release_Contents;

   ---------------------
   -- Target_Requires --
   ---------------------

   function Target_Requires
     (Mechanism : Client_Mechanism)
      return PolyORB.Security.Types.Association_Options
   is
      Result : Association_Options := 0;

   begin
      if Mechanism.Transport /= null then
         Result := Target_Requires (Mechanism.Transport);
      end if;

      if Mechanism.Authentication_Mechanism /= null
        and then Mechanism.Authentication_Required
      then
         Result := Result or Establish_Trust_In_Client;
      end if;

      if Mechanism.Authorities /= Client_Authority_Mechanism_Lists.Empty
        and then Mechanism.Delegation_Supported
        and then Mechanism.Delegation_Required
      then
         Result := Result or Delegation_By_Client;
      end if;

      return Result;
   end Target_Requires;

   ---------------------
   -- Target_Supports --
   ---------------------

   function Target_Supports
     (Mechanism : Client_Mechanism)
      return PolyORB.Security.Types.Association_Options
   is
      Result : Association_Options := 0;

   begin
      if Mechanism.Transport /= null then
         Result := Target_Supports (Mechanism.Transport);
      end if;

      if Mechanism.Authentication_Mechanism /= null then
         Result := Result or Establish_Trust_In_Client;
      end if;

      if Mechanism.Identity_Assertion then
         Result := Result or Identity_Assertion;
      end if;

      if Mechanism.Authorities /= Client_Authority_Mechanism_Lists.Empty
        and then Mechanism.Delegation_Supported
      then
         Result := Result or Delegation_By_Client;
      end if;

      return Result;
   end Target_Supports;

end PolyORB.QoS.Clients_Security;
