------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                  PORTABLESERVER.SERVANTACTIVATOR.IMPL                    --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2005-2010, Free Software Foundation, Inc.          --
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

package body PortableServer.ServantActivator.Impl is

   -----------------
   -- Etherealize --
   -----------------

   procedure Etherealize
     (Self                  : access Object;
      Oid                   :        PortableServer.ObjectId;
      Adapter               :        PortableServer.POA_Forward.Ref;
      Serv                  :        PortableServer.Servant;
      Cleanup_In_Progress   :        CORBA.Boolean;
      Remaining_Activations :        CORBA.Boolean)
   is
      pragma Unreferenced (Self);
      pragma Unreferenced (Oid);
      pragma Unreferenced (Adapter);
      pragma Unreferenced (Serv);
      pragma Unreferenced (Cleanup_In_Progress);
      pragma Unreferenced (Remaining_Activations);

   begin
      null;
   end Etherealize;

   ---------------
   -- Incarnate --
   ---------------

   function Incarnate
     (Self    : access Object;
      Oid     :        PortableServer.ObjectId;
      Adapter :        PortableServer.POA_Forward.Ref)
      return PortableServer.Servant
   is
      pragma Unreferenced (Self);
      pragma Unreferenced (Oid);
      pragma Unreferenced (Adapter);

   begin
      return null;
   end Incarnate;

   ----------
   -- Is_A --
   ----------

   function Is_A
     (Self            : not null access Object;
      Logical_Type_Id : Standard.String) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return
        CORBA.Is_Equivalent
          (Logical_Type_Id, PortableServer.ServantActivator.Repository_Id)
          or else
        CORBA.Is_Equivalent
          (Logical_Type_Id, PortableServer.ServantManager.Repository_Id)
          or else
        CORBA.Is_Equivalent
          (Logical_Type_Id, "IDL:omg.org/CORBA/Object:1.0");
   end Is_A;

end PortableServer.ServantActivator.Impl;
