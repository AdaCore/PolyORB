------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                  C O R B A . P O L I C Y C U R R E N T                   --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2004-2006, Free Software Foundation, Inc.          --
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

with CORBA.Local;
with CORBA.Policy;
with CORBA.PolicyManager;

package CORBA.PolicyCurrent is

   type Local_Ref is new CORBA.PolicyManager.Local_Ref with null record;

   function Get_Policy_Overrides
     (Self : Local_Ref;
      TS   : CORBA.Policy.PolicyTypeSeq)
     return CORBA.Policy.PolicyList;

   procedure Set_Policy_Overrides
     (Self     : Local_Ref;
      Policies : CORBA.Policy.PolicyList;
      Set_Add  : SetOverrideType);

private

   type Object is new CORBA.Local.Object with null record;

   type Object_Ptr is access all Object'Class;

   function Get_Policy_Overrides
     (Self : access Object;
      TS   : CORBA.Policy.PolicyTypeSeq)
     return CORBA.Policy.PolicyList;

   procedure Set_Policy_Overrides
     (Self     : access Object;
      Policies : CORBA.Policy.PolicyList;
      Set_Add  : CORBA.SetOverrideType);

   function Is_A
     (Self            : access Object;
      Logical_Type_Id : Standard.String)
     return Boolean;

end CORBA.PolicyCurrent;
