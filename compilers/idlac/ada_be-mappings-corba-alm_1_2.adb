------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--        A D A _ B E . M A P P I N G S . C O R B A . A L M _ 1 _ 2         --
--                                                                          --
--                                 B o d y                                  --
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

with Ada_Be.Identifiers; use Ada_Be.Identifiers;
with Idl_Fe.Tree;        use Idl_Fe.Tree;
with Idl_Fe.Types;       use Idl_Fe.Types;

package body Ada_Be.Mappings.CORBA.ALM_1_2 is

   function Is_CORBA_TypeCode (Node : Idl_Fe.Types.Node_Id) return Boolean;
   --  Return True iff Node denote CORBA.TypeCode interface declaration

   -----------------------------------
   -- Fetch_Calling_Stubs_Type_Name --
   -----------------------------------

   function Fetch_Calling_Stubs_Type_Name (Node : Node_Id) return String is
   begin
      pragma Assert (Is_Well_Known_Node (Node));

      if Is_CORBA_TypeCode (Node) then
         return "TypeCode.Object";

      else
         raise Program_Error;
      end if;
   end Fetch_Calling_Stubs_Type_Name;

   ----------------------------
   -- Fetch_Helper_Unit_Name --
   ----------------------------

   function Fetch_Helper_Unit_Name (Node : Node_Id) return String is
   begin
      pragma Assert (Is_Well_Known_Node (Node));

      if Is_CORBA_TypeCode (Node) then
         return "CORBA";

      else
         raise Program_Error;
      end if;
   end Fetch_Helper_Unit_Name;

   ---------------------
   -- Fetch_Unit_Name --
   ---------------------

   function Fetch_Unit_Name (Node : Node_Id) return String is
   begin
      pragma Assert (Is_Well_Known_Node (Node));

      if Is_CORBA_TypeCode (Node) then
         return "CORBA";

      else
         raise Program_Error;
      end if;
   end Fetch_Unit_Name;

   -----------------------
   -- Is_CORBA_TypeCode --
   -----------------------

   function Is_CORBA_TypeCode (Node : Node_Id) return Boolean is
   begin
      return Kind (Node) = K_Interface
        and then Ada_Full_Name (Node) = "CORBA.TypeCode";
   end Is_CORBA_TypeCode;

   ------------------------
   -- Is_Well_Known_Node --
   ------------------------

   function Is_Well_Known_Node (Node : Node_Id) return Boolean is
   begin
      if Is_CORBA_TypeCode (Node) then
         return True;

      else
         return False;
      end if;
   end Is_Well_Known_Node;

end Ada_Be.Mappings.CORBA.ALM_1_2;
