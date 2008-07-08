------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--               POLYORB.SECURITY.AUTHENTICATION_MECHANISMS                 --
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

with PolyORB.Log;
with PolyORB.Parameters;
with PolyORB.Types;
with PolyORB.Utils.Chained_Lists;

package body PolyORB.Security.Authentication_Mechanisms is

   use PolyORB.Log;

   package L is new PolyORB.Log.Facility_Log
     ("polyorb.security.authentication_mechanisms");
   procedure O (Message : in String; Level : Log_Level := Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
     renames L.Enabled;
   pragma Unreferenced (C); --  For conditional pragma Debug

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
        new Ada.Unchecked_Deallocation
        (Client_Authentication_Mechanism'Class,
         Client_Authentication_Mechanism_Access);

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
        new Ada.Unchecked_Deallocation
        (Target_Authentication_Mechanism'Class,
         Target_Authentication_Mechanism_Access);

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
        (O ("Register client authentication mechanism: "
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
        (O ("Register target authentication mechanism: '"
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
