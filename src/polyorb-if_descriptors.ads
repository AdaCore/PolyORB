------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                 P O L Y O R B . O B J _ A D A P T E R S                  --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                Copyright (C) 2001 Free Software Fundation                --
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
--              PolyORB is maintained by ENST Paris University.             --
--                                                                          --
------------------------------------------------------------------------------

--  Abstract interface for Interface Descriptors: services
--  that provide meta-data regarding the signatures of methods.

--  $Id$

with PolyORB.Any;
with PolyORB.Any.NVList;

with PolyORB.References;
with PolyORB.Requests;
with PolyORB.Smart_Pointers;

package PolyORB.If_Descriptors is

   type If_Descriptor is abstract new Smart_Pointers.Entity
     with private;
   type If_Descriptor_Access is access all If_Descriptor'Class;

   Default_If_Descriptor : If_Descriptor_Access;

   function Get_Empty_Arg_List
     (If_Desc : access If_Descriptor;
      Object  :        PolyORB.References.Ref;
      Method  :        Requests.Operation_Id)
     return Any.NVList.Ref is abstract;
   --  Return the paramter profile of the given method, so the
   --  protocol layer can unmarshall the message into a Request object.

   function Get_Empty_Result
     (If_Desc : access If_Descriptor;
      Object  :        PolyORB.References.Ref;
      Method  :        Requests.Operation_Id)
     return Any.Any is abstract;
   --  Return the result profile of the given method.

private

   type If_Descriptor is abstract new Smart_Pointers.Entity
     with null record;

end PolyORB.If_Descriptors;
