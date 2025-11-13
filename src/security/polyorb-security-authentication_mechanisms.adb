------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--               POLYORB.SECURITY.AUTHENTICATION_MECHANISMS                 --
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

with PolyORB.Log;
with PolyORB.Parameters;
with PolyORB.Types;
with PolyORB.Utils.Chained_Lists;

package body PolyORB.Security.Authentication_Mechanisms is

   use PolyORB.Log;

   package L is new PolyORB.Log.Facility_Log
     ("polyorb.security.authentication_mechanisms");
   procedure O (Message : String; Level : Log_Level := Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
     renames L.Enabled;

   type Client_Registry_Item is record
      Mechanism_OID  : PolyORB.ASN1.Object_Identifier;
      Constructor    : Client_Mechanism_Constructor;
   end record;

   package Client_Registry_Item_Lists is
     new PolyORB.Utils.Chained_Lists (Client_Registry_Item);

   Client_Registry : Client_Registry_Item_Lists.List;

   type Target_Registry_Item is record
      Mechanism_Name : PolyORB.Types.String;
      Constructor    : Target_Mechanism_Constructor;
   end record;

   package Target_Registry_Item_Lists is
     new PolyORB.Utils.Chained_Lists (Target_Registry_Item);

   Target_Registry : Target_Registry_Item_Lists.List;

   -----------------------------
   -- Create_Client_Mechanism --
   -----------------------------

   function Create_Client_Mechanism
     (Mechanism_OID : PolyORB.ASN1.Object_Identifier;
      Target_Name   : PolyORB.Security.Exported_Names.Exported_Name_Access)
      return Client_Authentication_Mechanism_Access
   is
      use Client_Registry_Item_Lists;

      Iter : Iterator := First (Client_Registry);

   begin
      while not Last (Iter) loop
         if PolyORB.ASN1.Is_Equivalent
           (Value (Iter).Mechanism_OID, Mechanism_OID)
         then
            return Value (Iter).Constructor (Target_Name);
         end if;

         Next (Iter);
      end loop;

      return null;
   end Create_Client_Mechanism;

   -----------------------------
   -- Create_Target_Mechanism --
   -----------------------------

   function Create_Target_Mechanism
     (Section_Name : String)
      return Target_Authentication_Mechanism_Access
   is
      use Target_Registry_Item_Lists;
      use PolyORB.Parameters;
      use type PolyORB.Types.String;

      Iter : Iterator := First (Target_Registry);

      Mechanism : constant String
        := Get_Conf (Section_Name, "mechanism", "");

   begin
      while not Last (Iter) loop
         if Value (Iter).Mechanism_Name = Mechanism then
            return Value (Iter).Constructor (Section_Name);
         end if;

         Next (Iter);
      end loop;

      return null;
   end Create_Target_Mechanism;

   -------------
   -- Destroy --
   -------------

   procedure Destroy
     (Mechanism : in out Client_Authentication_Mechanism_Access)
   is

      procedure Free is
        new PolyORB.Utils.Unchecked_Deallocation.Free


        (Object => Client_Authentication_Mechanism'Class,


         Name   => Client_Authentication_Mechanism_Access);

   begin
      if Mechanism /= null then
         Release_Contents (Mechanism);
         Free (Mechanism);
      end if;
   end Destroy;

   procedure Destroy
     (Mechanism : in out Target_Authentication_Mechanism_Access)
   is

      procedure Free is
        new PolyORB.Utils.Unchecked_Deallocation.Free


        (Object => Target_Authentication_Mechanism'Class,


         Name   => Target_Authentication_Mechanism_Access);

   begin
      if Mechanism /= null then
         Release_Contents (Mechanism);
         Free (Mechanism);
      end if;
   end Destroy;

   -----------------------
   -- Get_Mechanism_OID --
   -----------------------

   function Get_Mechanism_OID
     (Mechanism : access Target_Authentication_Mechanism)
      return PolyORB.ASN1.Object_Identifier
   is
   begin
      return Mechanism.Mechanism_OID;
   end Get_Mechanism_OID;

   ----------------------------------
   -- Get_Supported_Identity_Types --
   ----------------------------------

   function Get_Supported_Identity_Types
     (Mechanism : access Target_Authentication_Mechanism)
      return PolyORB.Security.Types.Identity_Token_Type
   is
   begin
      return Mechanism.Identity_Types;
   end Get_Supported_Identity_Types;

   -------------------------------------
   -- Get_Supported_Naming_Mechanisms --
   -------------------------------------

   function Get_Supported_Naming_Mechanisms
     (Mechanism : access Target_Authentication_Mechanism)
      return PolyORB.Security.Types.OID_Lists.List
   is
   begin
      return Mechanism.Naming_Mechanisms;
   end Get_Supported_Naming_Mechanisms;

   ---------------------
   -- Get_Target_Name --
   ---------------------

   function Get_Target_Name
     (Mechanism : access Target_Authentication_Mechanism)
      return PolyORB.Security.Exported_Names.Exported_Name_Access
   is
   begin
      return Mechanism.Target_Name;
   end Get_Target_Name;

   --------------
   -- Register --
   --------------

   procedure Register
     (Mechanism_OID : PolyORB.ASN1.Object_Identifier;
      Constructor   : Client_Mechanism_Constructor)
   is
   begin
      pragma Debug
        (C, O ("Register client authentication mechanism: "
             & PolyORB.ASN1.To_String (Mechanism_OID)));

      Client_Registry_Item_Lists.Append
       (Client_Registry,
        (Mechanism_OID => Mechanism_OID,
         Constructor   => Constructor));
   end Register;

   procedure Register
     (Mechanism_Name : String;
      Constructor    : Target_Mechanism_Constructor)
   is
   begin
      pragma Debug
        (C, O ("Register target authentication mechanism: '"
             & Mechanism_Name & '''));

      Target_Registry_Item_Lists.Append
        (Target_Registry,
         (Mechanism_Name => PolyORB.Types.To_PolyORB_String (Mechanism_Name),
          Constructor    => Constructor));
   end Register;

   ----------------------
   -- Release_Contents --
   ----------------------

   procedure Release_Contents
     (Mechanism : access Client_Authentication_Mechanism)
   is
   begin
      PolyORB.Security.Exported_Names.Destroy (Mechanism.Target_Name);
   end Release_Contents;

   procedure Release_Contents
     (Mechanism : access Target_Authentication_Mechanism)
   is
   begin
      PolyORB.ASN1.Destroy (Mechanism.Mechanism_OID);
      PolyORB.Security.Exported_Names.Destroy (Mechanism.Target_Name);

      declare
         use PolyORB.Security.Types.OID_Lists;

         Iter : Iterator := First (Mechanism.Naming_Mechanisms);

      begin
         while not Last (Iter) loop
            PolyORB.ASN1.Destroy (Value (Iter).all);
            Next (Iter);
         end loop;

         Deallocate (Mechanism.Naming_Mechanisms);
      end;
   end Release_Contents;

end PolyORB.Security.Authentication_Mechanisms;
