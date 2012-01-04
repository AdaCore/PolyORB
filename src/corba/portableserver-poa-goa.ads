------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--               P O R T A B L E S E R V E R . P O A . G O A                --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2003-2012, Free Software Foundation, Inc.          --
--                                                                          --
-- This specification is derived from the CORBA Specification, and adapted  --
-- for use with PolyORB. The copyright notice above, and the license        --
-- provisions that follow apply solely to the contents neither explicitly   --
-- nor implicitly specified by the CORBA Specification defined by the OMG.  --
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

with CORBA.Object;

package PortableServer.POA.GOA is

   type Ref is new PortableServer.POA.Local_Ref with null record;

   function To_Ref
     (Self : CORBA.Object.Ref'Class)
     return Ref;

   ----------------------
   -- Group management --
   ----------------------

   function Create_Id_For_Reference
     (Self    : Ref;
      The_Ref : CORBA.Object.Ref)
     return PortableServer.ObjectId;
   --  raises (NotAGroupObject);
   --  create a new objectid and associate it with group The_Ref

   function Reference_To_Ids
     (Self    : Ref;
      The_Ref : CORBA.Object.Ref)
     return IDs;
   --  raises (NotAGroupObject);
   --  user must free all object_id in list

   procedure Associate_Reference_With_Id
     (Self : Ref;
      Ref  : CORBA.Object.Ref;
      Oid  : PortableServer.ObjectId);
   --  raises(NotAGroupObject);

   procedure Disassociate_Reference_With_Id
     (Self : Ref;
      Ref  : CORBA.Object.Ref;
      Oid  : PortableServer.ObjectId);
   --  raises(NotAGroupObject);

end PortableServer.POA.GOA;
