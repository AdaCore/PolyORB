------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--     P O L Y O R B . C O R B A _ P . S E R V A N T A C T I V A T O R      --
--                                                                          --
--                                 S p e c                                  --
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

pragma Ada_2012;

--  This package provides glue codee between PolyORB's
--  ServantActivator and CORBA specific ServantActivator.

with PortableServer.ServantActivator;

with PolyORB.Errors;
with PolyORB.POA_Types;
with PolyORB.Servants;
with PolyORB.Smart_Pointers;

package PolyORB.CORBA_P.ServantActivator is

   package PPT renames PolyORB.POA_Types;

   type CORBA_ServantActivator is new PPT.ServantActivator with private;

   procedure Create
     (Self : out PPT.ServantActivator_Access;
      SA   :     PortableServer.ServantActivator.Local_Ref'Class);

   function Get_Servant_Manager
     (Self : CORBA_ServantActivator)
     return PortableServer.ServantActivator.Local_Ref'Class;

   overriding procedure Incarnate
     (Self    : access CORBA_ServantActivator;
      Oid     :        PPT.Object_Id;
      Adapter : access PPT.Obj_Adapter'Class;
      Returns :    out PolyORB.Servants.Servant_Access;
      Error   : in out PolyORB.Errors.Error_Container);

   overriding procedure Etherealize
     (Self                  : access CORBA_ServantActivator;
      Oid                   :        PPT.Object_Id;
      Adapter               : access PPT.Obj_Adapter'Class;
      Serv                  :        PolyORB.Servants.Servant_Access;
      Cleanup_In_Progress   :        Boolean;
      Remaining_Activations :        Boolean);

private

   type CORBA_ServantActivator is new PPT.ServantActivator with null record;

   type Object is new PolyORB.Smart_Pointers.Non_Controlled_Entity with record
      SA : PortableServer.ServantActivator.Local_Ref;
   end record;

   type Object_Ptr is access all Object;

end PolyORB.CORBA_P.ServantActivator;
