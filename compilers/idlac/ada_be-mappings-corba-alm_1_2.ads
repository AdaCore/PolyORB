------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--        A D A _ B E . M A P P I N G S . C O R B A . A L M _ 1 _ 2         --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2005-2012, Free Software Foundation, Inc.          --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.                                               --
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
