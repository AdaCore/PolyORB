------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                               S C O P E S                                --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2005-2014, Free Software Foundation, Inc.          --
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

with Types; use Types;

package Scopes is

   --  Scope_Entity and Visibility:
   --  ----------------------------
   --
   --  To handle scope, iac uses two dedicated node attributes: Scope_Entity
   --  and Potential_Scope. Scope_Entity designates the regular scope of the
   --  corresponding entity while Potential_Scope designates the scope into
   --  which the entity has been imported. Imports occur for type names and
   --  inherited interfaces.
   --
   --  To handle visibility, iac uses two dedicated node attributes: Visible
   --  and Implicitly_Visible. The normal visibility rules are handled by
   --  Visible while Implicitly_Visible is used only in the context of
   --  inherited interfaces. In the scope of an inherited interface, entities
   --  like attributes and operations are inherited (scoped and explicitly
   --  visible) while other entities are just made visible (implicitly
   --  visible).

   D_Scopes : Boolean := False;
   --  When true, displays more information when analyzing the scopes

   procedure Initialize;

   procedure Push_Scope (S : Node_Id);

   procedure Pop_Scope;
   --  Handle special scoping rules for types names (see 3.15.3). The
   --  potential scope of a type name extends over all its enclosing
   --  scopes out to the enclosing non-module scope. Remove nodes
   --  from their homonym chains (used to apply visibility rules).

   function  Current_Scope return Node_Id;
   --  Return current scope

   function Node_Explicitly_In_Scope (N : Node_Id; S : Node_Id) return Node_Id;
   --  Find whether there is a definition for identifier N in scope
   --  S. This node must be explicitly declared in S and not imported
   --  because of special scoping rules.

   function Node_In_Current_Scope (N : Node_Id) return Node_Id;
   --  Find whether there is a definition for identifier N in current
   --  scope. This node can be implicitly declared that is explicitly
   --  or potentially declared because of special scoping rules.

   function Visible_Node (N : Node_Id) return Node_Id;
   --  Find the currently visible definition for a given identifier,
   --  that is to say the first entry in the visibility chain
   --  (implemented using the homonyms chain).

   procedure Enter_Name_In_Scope (N : Node_Id);
   --  Detect naming conflict with N. In case of success, add N to the
   --  current scope.

   IDL_Spec      : Node_Id;
   IDL_Spec_Name : Name_Id;

end Scopes;
