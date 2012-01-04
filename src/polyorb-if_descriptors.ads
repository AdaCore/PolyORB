------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--               P O L Y O R B . I F _ D E S C R I P T O R S                --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2002-2012, Free Software Foundation, Inc.          --
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

--  Abstract interface for Interface Descriptors: services
--  that provide meta-data regarding the signatures of methods.

with PolyORB.Any;
with PolyORB.Any.NVList;
with PolyORB.References;
with PolyORB.Smart_Pointers;

package PolyORB.If_Descriptors is

   type If_Descriptor is abstract new Smart_Pointers.Non_Controlled_Entity
     with private;
   type If_Descriptor_Access is access all If_Descriptor'Class;

   Default_If_Descriptor : If_Descriptor_Access;

   function Get_Empty_Arg_List
     (If_Desc : access If_Descriptor;
      Object  :        PolyORB.References.Ref;
      Method  :        String)
     return Any.NVList.Ref is abstract;
   --  Return the paramter profile of the given method, so the
   --  protocol layer can unmarshall the message into a Request object.

   function Get_Empty_Result
     (If_Desc : access If_Descriptor;
      Object  :        PolyORB.References.Ref;
      Method  :        String)
     return Any.Any is abstract;
   --  Return the result profile of the given method.

private

   type If_Descriptor is abstract new Smart_Pointers.Non_Controlled_Entity
     with null record;

end PolyORB.If_Descriptors;
