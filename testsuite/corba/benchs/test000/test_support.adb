------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                         T E S T _ S U P P O R T                          --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2005-2008, Free Software Foundation, Inc.          --
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

with Ada.Text_IO;

with CORBA.IDL_SEQUENCES;
with CORBA.Impl;
with CORBA.Object;
with CORBA.ORB;
with CORBA.Policy;
with PortableServer.POA.Helper;

with Test.Activator.Impl;
with Test.Echo.Helper;
with Test.Echo.Impl;
with Test.Factory.Impl;

package body Test_Support is

   function To_ObjectId (Item : Wide_String) return PortableServer.ObjectId;

   My_POA : PortableServer.POA.Local_Ref;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
      Root_POA : constant PortableServer.POA.Local_Ref
        := PortableServer.POA.Helper.To_Local_Ref
            (CORBA.ORB.Resolve_Initial_References
              (CORBA.ORB.To_CORBA_String ("RootPOA")));

      Policies : CORBA.Policy.PolicyList;

   begin
      CORBA.Policy.IDL_SEQUENCE_Policy.Append
       (Policies,
        CORBA.Policy.Ref
         (PortableServer.POA.Create_Lifespan_Policy
           (PortableServer.PERSISTENT)));
      CORBA.Policy.IDL_SEQUENCE_Policy.Append
       (Policies,
        CORBA.Policy.Ref
         (PortableServer.POA.Create_Id_Assignment_Policy
           (PortableServer.USER_ID)));
      CORBA.Policy.IDL_SEQUENCE_Policy.Append
       (Policies,
        CORBA.Policy.Ref
         (PortableServer.POA.Create_Implicit_Activation_Policy
           (PortableServer.NO_IMPLICIT_ACTIVATION)));
      CORBA.Policy.IDL_SEQUENCE_Policy.Append
       (Policies,
        CORBA.Policy.Ref
         (PortableServer.POA.Create_Request_Processing_Policy
           (PortableServer.USE_SERVANT_MANAGER)));

      My_POA :=
        PortableServer.POA.Local_Ref
         (PortableServer.POA.Create_POA
           (Root_POA,
            CORBA.To_CORBA_String ("Ring_POA"),
            PortableServer.POA.Get_The_POAManager (Root_POA),
            Policies));

      declare
         Obj : constant Test.Activator.Impl.Object_Ptr
           := new Test.Activator.Impl.Object;
         Ref : Test.Activator.Local_Ref;

      begin
         Test.Activator.Set (Ref, CORBA.Impl.Object_Ptr (Obj));

         PortableServer.POA.Set_Servant_Manager (My_POA, Ref);
      end;

      declare
         Srv : constant Test.Factory.Impl.Object_Ptr
           := new Test.Factory.Impl.Object;
         Ref : constant CORBA.Object.Ref
           := PortableServer.POA.Servant_To_Reference
               (Root_POA,
                PortableServer.Servant (Srv));
      begin
         Ada.Text_IO.Put_Line
           ("'"
            & CORBA.To_Standard_String (CORBA.ORB.Object_To_String (Ref))
            & "'");
      end;
   end Initialize;

   -----------------
   -- Preallocate --
   -----------------

   procedure Preallocate (Count : Natural) is
   begin
      for J in 1 .. Count loop
         declare
            Srv : constant Test.Echo.Impl.Object_Ptr
              := new Test.Echo.Impl.Object;
         begin
            PortableServer.POA.Activate_Object_With_Id
             (My_POA,
              To_ObjectId (Integer'Wide_Image (J)),
              PortableServer.Servant (Srv));
         end;
      end loop;
   end Preallocate;

   -----------------
   -- To_ObjectId --
   -----------------

   function To_ObjectId
     (Item : Wide_String)
      return PortableServer.ObjectId
   is
      use CORBA.IDL_SEQUENCES.IDL_SEQUENCE_Octet;

      Result : PortableServer.ObjectId;

   begin
      for J in Item'Range loop
         Append (CORBA.IDL_SEQUENCES.IDL_SEQUENCE_Octet.Sequence (Result),
                 CORBA.Octet (Wide_Character'Pos (Item (J)) / 256));
         Append (CORBA.IDL_SEQUENCES.IDL_SEQUENCE_Octet.Sequence (Result),
                 CORBA.Octet (Wide_Character'Pos (Item (J)) mod 256));
      end loop;

      return Result;
   end To_ObjectId;

   -------------------------
   -- To_Object_Reference --
   -------------------------

   function To_Object_Reference (Id : Natural) return Test.Echo.Ref is
   begin
      return
        Test.Echo.Helper.To_Ref
         (PortableServer.POA.Create_Reference_With_Id
           (My_POA,
            To_ObjectId (Natural'Wide_Image (Id)),
            CORBA.To_CORBA_String (Test.Echo.Repository_Id)));
   end To_Object_Reference;

end Test_Support;
