------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--         P O L Y O R B . Q O S . T A R G E T S _ S E C U R I T Y          --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2005-2012, Free Software Foundation, Inc.          --
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

pragma Ada_2012;

with PolyORB.Utils.Unchecked_Deallocation;

with PolyORB.ASN1;

package body PolyORB.QoS.Targets_Security is

   use PolyORB.Annotations;
   use PolyORB.ASN1;
   use PolyORB.Security.Authentication_Mechanisms;
   use PolyORB.Security.Backward_Trust_Evaluators;
   use PolyORB.Security.Forward_Trust_Evaluators;
   use PolyORB.Security.Authority_Mechanisms;
   use PolyORB.Security.Authority_Mechanisms.Target_Authority_Mechanism_Lists;
   use PolyORB.Security.Transport_Mechanisms;
   use PolyORB.Security.Types;
   use OID_Lists;

   procedure Release_Contents (Item : in out Target_Mechanism);

   ------------------
   -- Is_Protected --
   ------------------

   function Is_Protected (Mechanism : Target_Mechanism) return Boolean is
   begin
      return Target_Requires (Mechanism) /= 0;
   end Is_Protected;

   ----------------------
   -- Release_Contents --
   ----------------------

   overriding procedure Release_Contents
     (QoS : access QoS_Target_Security_Parameter)
   is
      use Target_Mechanism_Lists;

      procedure Free is
        new PolyORB.Utils.Unchecked_Deallocation.Free


        (Object => Target_Mechanism,


         Name   => Target_Mechanism_Access);

      Iter : Target_Mechanism_Lists.Iterator := First (QoS.Mechanisms);

   begin
      while not Last (Iter) loop
         Release_Contents (Value (Iter).all.all);
         Free (Value (Iter).all);

         Next (Iter);
      end loop;

      Deallocate (QoS.Mechanisms);
   end Release_Contents;

   procedure Release_Contents (Item : in out Target_Mechanism) is

      procedure Free is
        new PolyORB.Utils.Unchecked_Deallocation.Free


        (Object => Target_Transport_Mechanism'Class,


         Name   => Target_Transport_Mechanism_Access);

   begin
      Free (Item.Transport);

      Destroy (Item.Authentication_Mechanism);

      declare
         Iter : Target_Authority_Mechanism_Lists.Iterator
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

   -------------------------------
   -- Set_Accepting_Credentials --
   -------------------------------

   procedure Set_Accepting_Credentials
     (Mechanism   : in out Target_Mechanism;
      Credentials :        PolyORB.Security.Credentials.Credentials_Ref)
   is
   begin
      Mechanism.Credentials := Credentials;

      if Mechanism.Transport /= null then
         Set_Accepting_Credentials (Mechanism.Transport, Credentials);
      end if;
   end Set_Accepting_Credentials;

   ---------------------
   -- Target_Requires --
   ---------------------

   function Target_Requires
     (Mechanism : Target_Mechanism)
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

      if not Is_Empty (Mechanism.Authorities)
        and then Mechanism.Forward_Trust_Evaluator /= null
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
     (Mechanism : Target_Mechanism)
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

      if Mechanism.Backward_Trust_Evaluator /= null
        or else Mechanism.Forward_Trust_Evaluator /= null
      then
         Result := Result or Identity_Assertion;
      end if;

      if not Is_Empty (Mechanism.Authorities)
        and then Mechanism.Forward_Trust_Evaluator /= null
      then
         Result := Result or Delegation_By_Client;
      end if;

      return Result;
   end Target_Supports;

end PolyORB.QoS.Targets_Security;
