------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--      P O L Y O R B . C O R B A _ P . S E C U R I T Y _ P O L I C Y       --
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

with PolyORB.Utils.Unchecked_Deallocation;

with CORBA.Object.Policies;
with PolyORB.CORBA_P.Policy_Management;
with PolyORB.Security.Security_Manager;

package body PolyORB.CORBA_P.Security_Policy is

   type Credentials_List_Access is
     access all PolyORB.Security.Credentials.Credentials_List;

   type Client_Policy_Info is record
      Registered : Boolean;
      Convertor  : Convert_Client_Policy;
   end record;

   Registry :
     array (PolyORB.CORBA_P.Policy_Management.Policy_List'Range)
     of Client_Policy_Info
     := (others => (False, null));

   procedure Free is
     new PolyORB.Utils.Unchecked_Deallocation.Free


     (Object => PolyORB.Security.Credentials.Credentials_List,


      Name   => Credentials_List_Access);

   -----------------------
   -- Get_Client_Policy --
   -----------------------

   function Get_Client_Policy
     (Object : PolyORB.References.Ref)
      return Client_Policy
   is
      use PolyORB.Security.Types;

      Target   : constant CORBA.Object.Ref :=
        CORBA.Object.Internals.To_CORBA_Ref (Object);
      Creds    : Credentials_List_Access := null;
      Requires : PolyORB.Security.Types.Association_Options := 0;
      Policy   : CORBA.Policy.Ref;

   begin
      --  Analize overridden policy

      for J in Registry'Range loop
         if Registry (J).Registered then
            begin
               Policy := CORBA.Object.Policies.Get_Policy (Target, J);

               declare
                  Aux : constant Client_Policy :=
                    Registry (J).Convertor (Policy);

               begin
                  Requires := Requires or Aux.Client_Requires;

                  if Aux.Invocation_Credentials'Length /= 0 then
                     Free (Creds);
                     Creds :=
                       new PolyORB.Security.Credentials.Credentials_List'
                       (Aux.Invocation_Credentials);
                  end if;
               end;

            exception
               when CORBA.Inv_Policy =>
                  --  CORBA::INV_POLICY can be raised by
                  --  CORBA::Object::get_policy when policy's value is not
                  --  defined.

                  null;
            end;
         end if;
      end loop;

      --  Force minimum capsule's level of protection

      Requires :=
        Requires or PolyORB.Security.Security_Manager.Client_Requires;

      --  Use capsule's credentials if no invocation credentials defined

      if Creds = null then
         Creds :=
           new PolyORB.Security.Credentials.Credentials_List'
           (PolyORB.Security.Security_Manager.Own_Credentials);
      end if;

      declare
         Result : constant Client_Policy
           := (Length                 => Creds'Length,
               Client_Requires        => Requires,
               Invocation_Credentials => Creds.all);

      begin
         Free (Creds);

         return Result;
      end;
   end Get_Client_Policy;

   ----------------------------
   -- Register_Client_Policy --
   ----------------------------

   procedure Register_Client_Policy
     (The_Type  : CORBA.PolicyType;
      Convertor : Convert_Client_Policy)
   is
   begin
      Registry (The_Type) := (True, Convertor);
   end Register_Client_Policy;

end PolyORB.CORBA_P.Security_Policy;
