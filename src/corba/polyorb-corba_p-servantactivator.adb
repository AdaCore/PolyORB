------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--     P O L Y O R B . C O R B A _ P . S E R V A N T A C T I V A T O R      --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2003 Free Software Foundation, Inc.             --
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
--                PolyORB is maintained by ACT Europe.                      --
--                    (email: sales@act-europe.fr)                          --
--                                                                          --
------------------------------------------------------------------------------

with CORBA.Impl;

package body PolyORB.CORBA_P.ServantActivator is

   ------------
   -- Create --
   ------------

   procedure Create
     (Self :    out PPT.ServantActivator_Access;
      SA   : access PortableServer.ServantActivator.Ref'Class)
   is
      Activator : Object_Ptr := new Object;

   begin
      Self := new CORBA_ServantActivator;
      Activator.SA := SA_Ptr (SA);

      Set (CORBA_ServantActivator (Self.all),
           PolyORB.Smart_Pointers.Entity_Ptr (Activator));
   end Create;

   -------------------------
   -- Get_Servant_Manager --
   -------------------------

   function Get_Servant_Manager
     (Self : CORBA_ServantActivator)
     return PortableServer.ServantActivator.Ref'Class
   is
      Activator : constant Object_Ptr := Object_Ptr (Entity_Of (Self));

   begin
      return Activator.SA.all;
   end Get_Servant_Manager;

   ---------------
   -- Incarnate --
   ---------------

   function Incarnate
     (Self    : access CORBA_ServantActivator;
      Oid     : in     PPT.Object_Id;
      Adapter : access PPT.Obj_Adapter'Class)
     return PolyORB.Servants.Servant_Access
   is
      use type PortableServer.Servant;

      CORBA_POA : PortableServer.POA_Forward.Ref;

      CORBA_Servant : PortableServer.Servant;

      Activator : PortableServer.ServantActivator.Ref'Class :=
        PortableServer.ServantActivator.Ref'Class
        (Get_Servant_Manager (Self.all));

   begin
      PortableServer.POA_Forward.Set
        (CORBA_POA,
         PolyORB.Smart_Pointers.Entity_Ptr (Adapter));

      CORBA_Servant := PortableServer.ServantActivator.Incarnate
        (Activator,
         PortableServer.ObjectId (Oid),
         CORBA_POA);

      if CORBA_Servant = null then
         return null;
      else
         return PolyORB.Servants.Servant_Access
           (PortableServer.To_PolyORB_Servant (CORBA_Servant));
      end if;
   end Incarnate;

   -----------------
   -- Etherealize --
   -----------------

   procedure Etherealize
     (Self                  : access CORBA_ServantActivator;
      Oid                   : in     PPT.Object_Id;
      Adapter               : access PPT.Obj_Adapter'Class;
      Serv                  : in     PolyORB.Servants.Servant_Access;
      Cleanup_In_Progress   : in     Boolean;
      Remaining_Activations : in     Boolean)
   is
      CORBA_POA : PortableServer.POA_Forward.Ref;

      POA_Servant : constant PortableServer.Servant :=
        PortableServer.Servant (CORBA.Impl.To_CORBA_Servant (Serv));

      Activator : PortableServer.ServantActivator.Ref'Class :=
        PortableServer.ServantActivator.Ref'Class
        (Get_Servant_Manager (Self.all));

   begin
      PortableServer.POA_Forward.Set
        (CORBA_POA,
         PolyORB.Smart_Pointers.Entity_Ptr (Adapter));

      PortableServer.ServantActivator.Etherealize
        (Activator,
         PortableServer.ObjectId (Oid),
         CORBA_POA,
         POA_Servant,
         Cleanup_In_Progress,
         Remaining_Activations);
   end Etherealize;

end PolyORB.CORBA_P.ServantActivator;
