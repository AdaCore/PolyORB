------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                T E S T _ A D A P T E R A C T I V A T O R                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2003-2012, Free Software Foundation, Inc.          --
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

with CORBA.ORB;
with CORBA.Policy;

with PortableServer.POA.Helper;

with PolyORB.Utils.Report;

package body Test_AdapterActivator is

   use PolyORB.Utils.Report;

   ---------------------
   -- Unknown_Adapter --
   ---------------------

   Null_Activator_Called : Boolean := False;

   function Unknown_Adapter
     (Self   : NullAdapter_Ref;
      Parent : PortableServer.POA_Forward.Ref;
      Name   : CORBA.String)
     return Boolean
   is
      pragma Unreferenced (Self, Parent, Name);
   begin
      Null_Activator_Called := True;

      return False;
   end Unknown_Adapter;

   Meta_Child_POA : PortableServer.POA.Local_Ref;
   --  pragma Unreferenced (Meta_Child_POA);
   pragma Warnings (Off, Meta_Child_POA); --  WAG:5.02 DB08-008
   --  Assigned but never read

   Simple_Activator_Called : Boolean := False;

   function Unknown_Adapter
     (Self   : SimpleAdapter_Ref;
      Parent : PortableServer.POA_Forward.Ref;
      Name   : CORBA.String)
     return Boolean
   is
      pragma Unreferenced (Self);

      package Convert is new
        PortableServer.POA_Forward.Convert (PortableServer.POA.Local_Ref);

      Policies : CORBA.Policy.PolicyList;

      POA : constant PortableServer.POA.Local_Ref := Convert.To_Ref (Parent);

   begin
      Simple_Activator_Called := True;

      Meta_Child_POA := PortableServer.POA.Local_Ref
        (PortableServer.POA.Create_POA
         (POA,
          Name,
          PortableServer.POA.Get_The_POAManager (POA),
          Policies));

      return True;
   end Unknown_Adapter;

   -------------------------------
   -- Run_Test_AdapterActivator --
   -------------------------------

   procedure Run_Test_AdapterActivator
   is
      NullAdapter : constant NullAdapter_Access := new NullAdapter_Ref;

      SimpleAdapter : constant SimpleAdapter_Access := new SimpleAdapter_Ref;

      Policies : CORBA.Policy.PolicyList;

      Root_POA : constant PortableServer.POA.Local_Ref :=
        PortableServer.POA.Helper.To_Local_Ref
        (CORBA.ORB.Resolve_Initial_References
         (CORBA.ORB.To_CORBA_String ("RootPOA")));

      Child_POA : PortableServer.POA.Local_Ref;
      Foo_POA : PortableServer.POA.Local_Ref;
      --  pragma Unreferenced (Foo_POA);
      pragma Warnings (Off, Foo_POA); --  WAG:5.02 DB08-008
      --  Assigned but never read

   begin
      New_Test ("Adapter Activator");

      --  Register a Child POA

      Child_POA := PortableServer.POA.Local_Ref
        (PortableServer.POA.Create_POA
         (Root_POA,
          CORBA.To_CORBA_String ("Child_POA"),
          PortableServer.POA.Get_The_POAManager (Root_POA),
          Policies));

      Output ("Created child POA", True);

      --  Look for a non existent child POA without AdapterActivator

      begin
         Foo_POA := PortableServer.POA.Local_Ref
           (PortableServer.POA.Find_POA
            (Child_POA,
             CORBA.To_CORBA_String ("Foo"),
             True));

         Output ("Non existent POA found !", False);
      exception
         when PortableServer.POA.AdapterNonExistent =>
            Output ("Non existent POA not found", True);
      end;

      --  Set Null Adapter Activator

      PortableServer.POA.Set_The_Activator
        (Child_POA,
         NullAdapter);

      --  Look for a non existent child POA with Null AdapterActivator

      begin
         Foo_POA := PortableServer.POA.Local_Ref
           (PortableServer.POA.Find_POA
            (Child_POA,
             CORBA.To_CORBA_String ("Foo"),
             True));
      exception
         when PortableServer.POA.AdapterNonExistent =>
            Output ("Null_Adapter did nothing ", True);
      end;

      Output ("Null Unknown_Adapter invoked", Null_Activator_Called);

      --  Set Simple Adapter Activator

      PortableServer.POA.Set_The_Activator
        (Child_POA,
         SimpleAdapter);

      --  Look for a non existent child POA with Simple AdapterActivator

      Foo_POA := PortableServer.POA.Local_Ref
        (PortableServer.POA.Find_POA
         (Child_POA,
          CORBA.To_CORBA_String ("Foo"),
          True));

      Output ("Simple Unknown_Adapter invoked", Simple_Activator_Called);

      --  Simple check

      Foo_POA := PortableServer.POA.Local_Ref
        (PortableServer.POA.Find_POA
         (Child_POA,
          CORBA.To_CORBA_String ("Foo"),
          True));

      PortableServer.POA.Destroy (Child_POA, False, False);
   end Run_Test_AdapterActivator;

end Test_AdapterActivator;
