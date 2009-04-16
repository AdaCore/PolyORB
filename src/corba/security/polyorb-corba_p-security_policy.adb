------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--      P O L Y O R B . C O R B A _ P . S E C U R I T Y _ P O L I C Y       --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2005-2009, Free Software Foundation, Inc.          --
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
     new Ada.Unchecked_Deallocation
     (PolyORB.Security.Credentials.Credentials_List, Credentials_List_Access);

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

   begin
      --  Analize overridden policy

      for J in Registry'Range loop
         if Registry (J).Registered then
            declare
               Aux : constant Client_Policy
                 := Registry (J).Convertor
                 (CORBA.Object.Policies.Get_Policy (Target, J));

            begin
               Requires := Requires or Aux.Client_Requires;

               if Aux.Invocation_Credentials'Length /= 0 then
                  Free (Creds);
                  Creds :=
                    new PolyORB.Security.Credentials.Credentials_List'
                    (Aux.Invocation_Credentials);
               end if;

            exception
               when CORBA.Inv_Policy =>
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
