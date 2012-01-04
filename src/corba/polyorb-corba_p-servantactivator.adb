------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--     P O L Y O R B . C O R B A _ P . S E R V A N T A C T I V A T O R      --
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

with CORBA.Impl;
with CORBA.Object;

package body PolyORB.CORBA_P.ServantActivator is

   ------------
   -- Create --
   ------------

   procedure Create
     (Self : out PPT.ServantActivator_Access;
      SA   :     PortableServer.ServantActivator.Local_Ref'Class)
   is
      Activator : constant Object_Ptr := new Object;

   begin
      Self := new CORBA_ServantActivator;
      Activator.SA := PortableServer.ServantActivator.Local_Ref (SA);

      Set (CORBA_ServantActivator (Self.all),
           PolyORB.Smart_Pointers.Entity_Ptr (Activator));
   end Create;

   -------------------------
   -- Get_Servant_Manager --
   -------------------------

   function Get_Servant_Manager
     (Self : CORBA_ServantActivator)
      return PortableServer.ServantActivator.Local_Ref'Class
   is
      Activator : constant Object_Ptr := Object_Ptr (Entity_Of (Self));

   begin
      return Activator.SA;
   end Get_Servant_Manager;

   ---------------
   -- Incarnate --
   ---------------

   procedure Incarnate
     (Self    : access CORBA_ServantActivator;
      Oid     :        PPT.Object_Id;
      Adapter : access PPT.Obj_Adapter'Class;
      Returns :    out PolyORB.Servants.Servant_Access;
      Error   : in out PolyORB.Errors.Error_Container)
   is
      use type PortableServer.Servant;

      CORBA_POA : PortableServer.POA_Forward.Ref;

      CORBA_Servant : PortableServer.Servant;

      Activator : constant PortableServer.ServantActivator.Local_Ref'Class :=
                    Get_Servant_Manager (Self.all);

   begin
      PortableServer.POA_Forward.Set
        (CORBA_POA, PolyORB.Smart_Pointers.Entity_Ptr (Adapter));

      begin
         CORBA_Servant := PortableServer.ServantActivator.Incarnate
           (Activator,
            PortableServer.Internals.To_PortableServer_ObjectId (Oid),
            CORBA_POA);

      exception
         when E : PortableServer.ForwardRequest =>
            declare
               Members : PortableServer.ForwardRequest_Members;

            begin
               PortableServer.Get_Members (E, Members);

               Error.Kind := PolyORB.Errors.ForwardRequest_E;
               Error.Member :=
                 new PolyORB.Errors.ForwardRequest_Members'
                 (Forward_Reference =>
                    PolyORB.Smart_Pointers.Ref
                  (CORBA.Object.Internals.To_PolyORB_Ref
                   (Members.Forward_Reference)));
            end;
      end;

      if CORBA_Servant = null then
         Returns := null;
      else
         Returns := PortableServer.To_PolyORB_Servant (CORBA_Servant);
      end if;
   end Incarnate;

   -----------------
   -- Etherealize --
   -----------------

   procedure Etherealize
     (Self                  : access CORBA_ServantActivator;
      Oid                   :        PPT.Object_Id;
      Adapter               : access PPT.Obj_Adapter'Class;
      Serv                  :        PolyORB.Servants.Servant_Access;
      Cleanup_In_Progress   :        Boolean;
      Remaining_Activations :        Boolean)
   is
      CORBA_POA : PortableServer.POA_Forward.Ref;

      POA_Servant : constant PortableServer.Servant :=
        PortableServer.Servant (CORBA.Impl.Internals.To_CORBA_Servant (Serv));

      Activator : constant PortableServer.ServantActivator.Local_Ref'Class :=
                    Get_Servant_Manager (Self.all);

   begin
      PortableServer.POA_Forward.Set
        (CORBA_POA, PolyORB.Smart_Pointers.Entity_Ptr (Adapter));

      PortableServer.ServantActivator.Etherealize
        (Activator,
         PortableServer.Internals.To_PortableServer_ObjectId (Oid),
         CORBA_POA,
         POA_Servant,
         Cleanup_In_Progress,
         Remaining_Activations);
   end Etherealize;

end PolyORB.CORBA_P.ServantActivator;
