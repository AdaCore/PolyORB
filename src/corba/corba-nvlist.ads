------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                         C O R B A . N V L I S T                          --
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

--  $Id$

with CORBA.AbstractBase;
pragma Elaborate_All (CORBA.AbstractBase);

with PolyORB.Any.NVList;

package CORBA.NVList is

   type Ref is new CORBA.AbstractBase.Ref with private;

   procedure Add_Item
     (Self       :    Ref;
      Item_Name  : in Identifier;
      Item       : in CORBA.Any;
      Item_Flags : in Flags);
   --  Create a NamedValue and add it to this NVList.

   procedure Add_Item
     (Self : Ref;
      Item : in CORBA.NamedValue);
   --  Add a NamedValue to this NVList.

   function Get_Count (Self : Ref) return CORBA.Long;
   --  Return the number of items in this NVList.

   procedure Free (Self : Ref);
   procedure Free_Memory (Self : Ref) renames Free;
   --  Free and Free_Memory are no-ops in Ada.

   ------------------------------------------
   -- The following is specific to PolyORB --
   ------------------------------------------

   procedure Create (Self : out Ref);
   --  XXX THIS MUST BE REPLACED BY AN OVERRIDING OF
   --  Initialize!
   --  Requiring users to call Create is in violation of the
   --  standard CORBA API.

   function Item (Self : Ref; Index : CORBA.Long)
     return CORBA.NamedValue;

   function To_PolyORB_Ref (Self : Ref) return PolyORB.Any.NVList.Ref;
   function To_CORBA_Ref (Self : PolyORB.Any.NVList.Ref) return Ref;

private

   type Ref is new CORBA.AbstractBase.Ref with null record;

   pragma Inline (Add_Item);
   pragma Inline (Get_Count);
   pragma Inline (Free);
   pragma Inline (To_PolyORB_Ref);
   pragma Inline (To_CORBA_Ref);
   pragma Inline (Create);

end CORBA.NVList;
