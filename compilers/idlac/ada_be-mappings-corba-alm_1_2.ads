------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--        A D A _ B E . M A P P I N G S . C O R B A . A L M _ 1 _ 2         --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2005-2006, Free Software Foundation, Inc.          --
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

--  This unit controls the mapping of specific CORBA IDL constructs
--  into Ada units as defined by the Ada Language Mapping version 1.2.

package Ada_Be.Mappings.CORBA.ALM_1_2 is

   function Is_Well_Known_Node (Node : Idl_Fe.Types.Node_Id) return Boolean;
   --  Return True iff Node denotes one CORBA IDL construct with
   --  specific mapping rules.

   function Fetch_Unit_Name (Node : Idl_Fe.Types.Node_Id) return String;
   --  Return fully qualified compilation base unit name for Node

   function Fetch_Helper_Unit_Name
     (Node : Idl_Fe.Types.Node_Id)
      return String;
   --  Return fully qualified compilation helper unit name for Node

   function Fetch_Calling_Stubs_Type_Name
     (Node : Idl_Fe.Types.Node_Id)
      return String;
   --  Return calling stubs type name corresponding to Node

end Ada_Be.Mappings.CORBA.ALM_1_2;
