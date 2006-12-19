------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                  POLYORB.SECURITY.AUTHORITY_MECHANISMS                   --
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

with PolyORB.Parameters;

package body PolyORB.Security.Authority_Mechanisms is

   use PolyORB.Parameters;
   use PolyORB.Types;

   type Target_Registry_Item is record
      Name        : PolyORB.Types.String;
      Constructor : Target_Constructor;
   end record;

   type Client_Registry_Item is record
      Syntax      : Service_Configuration_Syntax;
      Constructor : Client_Constructor;
   end record;

   package Target_Registry_Lists is
     new PolyORB.Utils.Chained_Lists (Target_Registry_Item);

   package Client_Registry_Lists is
     new PolyORB.Utils.Chained_Lists (Client_Registry_Item);

   Target_Registry : Target_Registry_Lists.List;
   Client_Registry : Client_Registry_Lists.List;

   ---------------------------------------
   -- Create_Client_Authority_Mechanism --
   ---------------------------------------

   function Create_Client_Authority_Mechanism
     (Syntax : Service_Configuration_Syntax;
      Name   : Ada.Streams.Stream_Element_Array)
      return Client_Authority_Mechanism_Access
   is
      use Client_Registry_Lists;

      Iter : Iterator := First (Client_Registry);

   begin
      while not Last (Iter) loop
         if Value (Iter).all.Syntax = Syntax then
            return Value (Iter).all.Constructor (Name);
         end if;

         Next (Iter);
      end loop;

      return null;
   end Create_Client_Authority_Mechanism;

   ---------------------------------------
   -- Create_Target_Authority_Mechanism --
   ---------------------------------------

   function Create_Target_Authority_Mechanism
     (Section_Name : Standard.String)
      return Target_Authority_Mechanism_Access
   is
      use Target_Registry_Lists;

      Mechanism : constant String := Get_Conf (Section_Name, "mechanism", "");
      Iter      : Iterator := First (Target_Registry);

   begin
      while not Last (Iter) loop
         if Value (Iter).all.Name = Mechanism then
            return Value (Iter).all.Constructor (Section_Name);
         end if;

         Next (Iter);
      end loop;

      return null;
   end Create_Target_Authority_Mechanism;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Item : in out Client_Authority_Mechanism_Access) is

      procedure Free is
        new Ada.Unchecked_Deallocation
        (Client_Authority_Mechanism'Class, Client_Authority_Mechanism_Access);

   begin
      Release_Contents (Item);
      Free (Item);
   end Destroy;

   procedure Destroy (Item : in out Target_Authority_Mechanism_Access) is

      procedure Free is
        new Ada.Unchecked_Deallocation
        (Target_Authority_Mechanism'Class, Target_Authority_Mechanism_Access);

   begin
      Release_Contents (Item);
      Free (Item);
   end Destroy;

   --------------
   -- Register --
   --------------

   procedure Register (Syntax      : Service_Configuration_Syntax;
                       Constructor : Client_Constructor)
   is
   begin
      Client_Registry_Lists.Append (Client_Registry, (Syntax, Constructor));
   end Register;

   procedure Register (Name        : Standard.String;
                       Constructor : Target_Constructor)
   is
   begin
      Target_Registry_Lists.Append
        (Target_Registry, (To_PolyORB_String (Name), Constructor));
   end Register;

end PolyORB.Security.Authority_Mechanisms;
