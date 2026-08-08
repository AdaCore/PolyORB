------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                  POLYORB.SECURITY.AUTHORITY_MECHANISMS                   --
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
        new PolyORB.Utils.Unchecked_Deallocation.Free


        (Object => Client_Authority_Mechanism'Class,


         Name   => Client_Authority_Mechanism_Access);

   begin
      Release_Contents (Item);
      Free (Item);
   end Destroy;

   procedure Destroy (Item : in out Target_Authority_Mechanism_Access) is

      procedure Free is
        new PolyORB.Utils.Unchecked_Deallocation.Free


        (Object => Target_Authority_Mechanism'Class,


         Name   => Target_Authority_Mechanism_Access);

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
