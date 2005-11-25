------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--     P O L Y O R B . C O R B A _ P . S E R V A N T A C T I V A T O R      --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2003-2005 Free Software Foundation, Inc.           --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
-- not, write to the Free Software Foundation, 59 Temple Place - Suite 330, --
-- Boston, MA 02111-1307, USA.                                              --
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

      Activator : PortableServer.ServantActivator.Local_Ref'Class :=
        PortableServer.ServantActivator.Local_Ref'Class
        (Get_Servant_Manager (Self.all));

   begin
      PortableServer.POA_Forward.Set
        (CORBA_POA,
         PolyORB.Smart_Pointers.Entity_Ptr (Adapter));

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
         Returns := PolyORB.Servants.Servant_Access
           (PortableServer.To_PolyORB_Servant (CORBA_Servant));
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

      Activator : PortableServer.ServantActivator.Local_Ref'Class :=
        PortableServer.ServantActivator.Local_Ref'Class
        (Get_Servant_Manager (Self.all));

   begin
      PortableServer.POA_Forward.Set
        (CORBA_POA,
         PolyORB.Smart_Pointers.Entity_Ptr (Adapter));

      PortableServer.ServantActivator.Etherealize
        (Activator,
         PortableServer.Internals.To_PortableServer_ObjectId (Oid),
         CORBA_POA,
         POA_Servant,
         Cleanup_In_Progress,
         Remaining_Activations);
   end Etherealize;

end PolyORB.CORBA_P.ServantActivator;
