------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                  C O R B A . E X C E P T I O N L I S T                   --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2001-2002 Free Software Foundation, Inc.           --
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

--  Implementation Note: this package implements the recommendation of
--  the OMG issue #3706, that add new primitives to CORBA::Object.
--  See CORBA.Object package specifications for more details.

--  $Id$

with CORBA.AbstractBase;
pragma Elaborate_All (CORBA.AbstractBase);

with PolyORB.Any.ExceptionList;

package CORBA.ExceptionList is

   pragma Elaborate_Body;

   type Ref is new CORBA.AbstractBase.Ref with null record;
   Nil_Ref : constant Ref;

   function Get_Count
     (Self : in Ref)
     return CORBA.Unsigned_Long;

   procedure Add
     (Self : in Ref;
      Exc  : in CORBA.TypeCode.Object);

   function Item
     (Self  : in Ref;
      Index : in CORBA.Unsigned_Long)
     return CORBA.TypeCode.Object;

   procedure Remove
     (Self  : in Ref;
      Index : in CORBA.Unsigned_Long);

   procedure Create_List (Self : out Ref);

   function Search_Exception_Id
     (Self : in Ref;
      Name : in CORBA.RepositoryId)
     return CORBA.Unsigned_Long;

   ------------------------------------------
   -- The following is specific to PolyORB --
   ------------------------------------------

   function To_PolyORB_Ref (Self : Ref)
     return PolyORB.Any.ExceptionList.Ref;

   function To_CORBA_Ref (Self : PolyORB.Any.ExceptionList.Ref)
     return Ref;

private

   Nil_Ref : constant Ref
     := (CORBA.AbstractBase.Ref with null record);

   pragma Inline
     (Get_Count,
      Add,
      Item,
      Remove,
      Create_List,
      Search_Exception_Id);

end CORBA.ExceptionList;
