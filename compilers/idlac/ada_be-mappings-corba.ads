------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                A D A _ B E . M A P P I N G S . C O R B A                 --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2001-2006, Free Software Foundation, Inc.          --
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

--  The CORBA personality IDL mapping.

package Ada_Be.Mappings.CORBA is

   type CORBA_Mapping_Type is new Mapping_Type with private;

   function Library_Unit_Name
     (Self : access CORBA_Mapping_Type;
      Node : Idl_Fe.Types.Node_Id)
     return String;

   function Client_Stubs_Unit_Name
     (Self : access CORBA_Mapping_Type;
      Node : Idl_Fe.Types.Node_Id)
     return String;

   function Server_Skel_Unit_Name
     (Self : access CORBA_Mapping_Type;
      Node : Idl_Fe.Types.Node_Id)
     return String;

   function Self_For_Operation
     (Self : access CORBA_Mapping_Type;
      Node : Idl_Fe.Types.Node_Id)
     return String;

   procedure Map_Type_Name
     (Self : access CORBA_Mapping_Type;
      Node : Idl_Fe.Types.Node_Id;
      Unit : out ASU.Unbounded_String;
      Typ  : out ASU.Unbounded_String);

   function Calling_Stubs_Type
     (Self : access CORBA_Mapping_Type;
      Node : Idl_Fe.Types.Node_Id)
     return String;

   function Generate_Scope_In_Child_Package
     (Self : access CORBA_Mapping_Type;
      Node : Idl_Fe.Types.Node_Id)
     return Boolean;

   The_CORBA_Mapping : constant CORBA_Mapping_Type;

   ----------------------------
   -- CORBA specific section --
   ----------------------------

   function Ada_Helper_Unit_Name
     (Mapping : access CORBA_Mapping_Type;
      Node    : Idl_Fe.Types.Node_Id)
     return String;
   --  The name of the helper package where the TypeCode for Node is defined

   function Ada_Type_Defining_Name
     (Mapping : access CORBA_Mapping_Type;
      Node    : Idl_Fe.Types.Node_Id)
     return String;
   --  The defining name of the Ada type that maps Node
   --  (a K_Interface or K_ValueType).
   --  This is not the fully qualified name.

   function Code_Generation_Suppressed
     (Mapping : access CORBA_Mapping_Type;
      Node    : Idl_Fe.Types.Node_Id)
     return Boolean;
   --  Return True iff code generation for Node should be suppressed
   --  because of non-standard or Ada Language Mapping specific rules.

private

   type CORBA_Mapping_Type is new Mapping_Type with null record;

   The_CORBA_Mapping : constant CORBA_Mapping_Type
     := (Mapping_Type with null record);

end Ada_Be.Mappings.CORBA;
