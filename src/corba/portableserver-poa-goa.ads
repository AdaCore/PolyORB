------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--               P O R T A B L E S E R V E R . P O A . G O A                --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2003-2006, Free Software Foundation, Inc.          --
--                                                                          --
-- This specification is derived from the CORBA Specification, and adapted  --
-- for use with PolyORB. The copyright notice above, and the license        --
-- provisions that follow apply solely to the contents neither explicitely  --
-- nor implicitely specified by the CORBA Specification defined by the OMG. --
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
