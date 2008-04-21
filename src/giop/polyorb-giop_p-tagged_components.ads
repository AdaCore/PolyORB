------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--     P O L Y O R B . G I O P _ P . T A G G E D _ C O M P O N E N T S      --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2003-2008, Free Software Foundation, Inc.          --
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

--  Implementation of CORBA IOR Tagged components

with Ada.Streams;

with PolyORB.Buffers;
with PolyORB.Errors;
with PolyORB.Objects;
with PolyORB.QoS.Tagged_Components;
with PolyORB.Types;
with PolyORB.Utils.Chained_Lists;

package PolyORB.GIOP_P.Tagged_Components is

   use PolyORB.Buffers;

   type Tag_Value is new Types.Unsigned_Long;

   ----------------------
   -- Tagged_Component --
   ----------------------

   type Tagged_Component (Tag : Tag_Value; At_Most_Once : Boolean)
      is abstract tagged private;

   type Tagged_Component_Access is access all Tagged_Component'Class;

   procedure Marshall_Component_Data
     (Comp   : access Tagged_Component;
      Buffer : access Buffer_Type)
      is abstract;
   --  Marshall tagged component_data associated to component

   procedure Unmarshall_Component_Data
     (Comp   : access Tagged_Component;
      Buffer : access Buffer_Type;
      Error  : out PolyORB.Errors.Error_Container)
     is abstract;
   --  Unmarshall tagged component_data associated to component

   procedure Release_Contents (Comp : access Tagged_Component) is abstract;
   --  Free memory associated with component

   function Duplicate (C : Tagged_Component) return Tagged_Component_Access
     is abstract;

   ---------------------------
   -- Tagged_Component_List --
   ---------------------------

   type Tagged_Component_List is private;

   Null_Tagged_Component_List : constant Tagged_Component_List;
   --  Empty list

   type Tagged_Component_Array is
     array (Positive range <>) of Tagged_Component_Access;

   procedure Release_Contents (List : in out Tagged_Component_List);
   --  Free memory for all tags in List

   procedure Marshall_Tagged_Component
     (Buffer     : access Buffer_Type;
      Components :        Tagged_Component_List);
   --  Marshall Tagged Component List

   function Unmarshall_Tagged_Component
     (Buffer     : access Buffer_Type)
     return Tagged_Component_List;
   --  Unmarshall tagged component List

   function Get_Component
     (List : Tagged_Component_List;
      Tag  : Tag_Value)
     return Tagged_Component_Access;
   --  Search and return a component in a tagged component list

   function Get_Components
     (List : Tagged_Component_List;
      Tag  : Tag_Value)
     return Tagged_Component_Array;
   --  Search and return all components with specified Tag in a tagged
   --  component list.

   function Fetch_Components
     (Oid : access PolyORB.Objects.Object_Id)
     return Tagged_Component_List;
   --  Return a Tagget_Component_List of all tagged components configured for
   --  object denoted by Oid.

   procedure Add
     (List : in out Tagged_Component_List;
      Comp :        Tagged_Component_Access);
   --  Add a component to a tagged component list

   procedure Add
     (List : in out Tagged_Component_List;
      CL   :        Tagged_Component_List);
   --  Add a list of components to a tagged component list

   procedure Remove
     (List : in out Tagged_Component_List;
      Comp : Tagged_Component_Access);
   --  Remove Comp from List

   function Deep_Copy
     (List : Tagged_Component_List) return Tagged_Component_List;
   --  Return a deep copy of List

   -------------------------
   -- Register components --
   -------------------------

   type New_Empty_Component_Func_Access is access
     function return Tagged_Component_Access;

   type Fetch_Component_Func_Access is access
     function (Oid : access PolyORB.Objects.Object_Id)
              return Tagged_Component_Access;

   procedure Register
     (Tag                 : Tag_Value;
      New_Empty_Component : New_Empty_Component_Func_Access;
      Fetch_Component     : Fetch_Component_Func_Access);
   --  Register tagged component with tag Tag

   --------------
   -- Tag List --
   --------------

   Tag_ORB_Type                 : constant Tag_Value;
   Tag_Code_Sets                : constant Tag_Value;
   Tag_Policies                 : constant Tag_Value;
   Tag_Alternate_IIOP_Address   : constant Tag_Value;
   Tag_Association_Options      : constant Tag_Value;
   Tag_Sec_Name                 : constant Tag_Value;
   Tag_SPKM_1_Sec_Mech          : constant Tag_Value;
   Tag_SPKM_2_Sec_Mech          : constant Tag_Value;
   Tag_KerberosV5_Sec_Mech      : constant Tag_Value;
   Tag_CSI_ECMA_Secret_Sec_Mech : constant Tag_Value;
   Tag_CSI_ECMA_Hybrid_Sec_Mech : constant Tag_Value;
   Tag_SSL_Sec_Trans            : constant Tag_Value;
   Tag_CSI_ECMA_Public_Sec_Mech : constant Tag_Value;
   Tag_Generic_Sec_Mech         : constant Tag_Value;
   Tag_Firewall_Trans           : constant Tag_Value;
   Tag_SCCP_Contact_Info        : constant Tag_Value;
   Tag_Java_Codebase            : constant Tag_Value;
   Tag_Transaction_Policy       : constant Tag_Value;
   Tag_FT_Group                 : constant Tag_Value;
   Tag_FT_Primary               : constant Tag_Value;
   Tag_Message_Routers          : constant Tag_Value;
   Tag_OTS_Policy               : constant Tag_Value;
   Tag_INV_Policy               : constant Tag_Value;
   Tag_CSI_Sec_Mech_List        : constant Tag_Value;
   Tag_NULL_Tag                 : constant Tag_Value;
   Tag_SECIOP_Sec_Trans         : constant Tag_Value;
   Tag_TLS_Sec_Trans            : constant Tag_Value;
   Tag_Activity_Policy          : constant Tag_Value;
   Tag_Group                    : constant Tag_Value;
   Tag_INET_Sec_Trans           : constant Tag_Value;

   function Create_QoS_GIOP_Tagged_Components_List
     (Components : Tagged_Component_List)
      return PolyORB.QoS.Tagged_Components.GIOP_Tagged_Component_Lists.List;

   -----------------------
   -- Unknown Component --
   -----------------------

   --  Unknown component is used when tag is unknown at unmarshalling time.
   --  Users cannot access to unknown components data, but unknown
   --  components can be remarshalled without being modified.

   type Octet_Access is access all Ada.Streams.Stream_Element_Array;
   --  Data in an unknow tagged component

   Tag_Unknown                  : constant Tag_Value := Tag_Value'Last;
   --  PolyORB specific value for Unknown tagged components

   type TC_Unknown_Component is
     new Tagged_Component (Tag => Tag_Unknown, At_Most_Once => False)
     with private;

   type TC_Unknown_Component_Access is access all TC_Unknown_Component'Class;

   procedure Marshall_Component_Data
     (Comp   : access TC_Unknown_Component;
      Buffer : access Buffer_Type);

   procedure Unmarshall_Component_Data
     (Comp   : access TC_Unknown_Component;
      Buffer : access Buffer_Type;
      Error  : out PolyORB.Errors.Error_Container);

   function Create_Unknown_Component
     (Unknown_Tag : Tag_Value;
      Data        : Octet_Access)
      return Tagged_Component_Access;

   procedure Release_Contents (Comp : access TC_Unknown_Component);

   function Duplicate
     (Comp : TC_Unknown_Component) return Tagged_Component_Access;

private

   type Tagged_Component (Tag : Tag_Value; At_Most_Once : Boolean)
      is abstract tagged null record;

   package Component_Lists is new
     PolyORB.Utils.Chained_Lists (Tagged_Component_Access);
   use Component_Lists;

   --  Tagged component list

   type Tagged_Component_List is new Component_Lists.List;

   Null_Tagged_Component_List : constant Tagged_Component_List
     := Tagged_Component_List (Component_Lists.Empty);

   procedure Marshall_Tagged_Component
     (Buffer    : access Buffer_Type;
      Component :        Tagged_Component_Access);
   --  Marshall one tagged component

   procedure Unmarshall_Tagged_Component
     (Buffer : access Buffer_Type;
      C      :    out Tagged_Component_Access;
      Error  :    out PolyORB.Errors.Error_Container);
   --  Unmarshall one tagged component

   --------------
   -- Tag List --
   --------------

   Tag_ORB_Type                 : constant Tag_Value := 0;
   Tag_Code_Sets                : constant Tag_Value := 1;
   Tag_Policies                 : constant Tag_Value := 2;
   Tag_Alternate_IIOP_Address   : constant Tag_Value := 3;
   Tag_Association_Options      : constant Tag_Value := 13;
   Tag_Sec_Name                 : constant Tag_Value := 14;
   Tag_SPKM_1_Sec_Mech          : constant Tag_Value := 15;
   Tag_SPKM_2_Sec_Mech          : constant Tag_Value := 16;
   Tag_KerberosV5_Sec_Mech      : constant Tag_Value := 17;
   Tag_CSI_ECMA_Secret_Sec_Mech : constant Tag_Value := 18;
   Tag_CSI_ECMA_Hybrid_Sec_Mech : constant Tag_Value := 19;
   Tag_SSL_Sec_Trans            : constant Tag_Value := 20;
   Tag_CSI_ECMA_Public_Sec_Mech : constant Tag_Value := 21;
   Tag_Generic_Sec_Mech         : constant Tag_Value := 22;
   Tag_Firewall_Trans           : constant Tag_Value := 23;
   Tag_SCCP_Contact_Info        : constant Tag_Value := 24;
   Tag_Java_Codebase            : constant Tag_Value := 25;
   Tag_Transaction_Policy       : constant Tag_Value := 26;
   Tag_FT_Group                 : constant Tag_Value := 27;
   Tag_FT_Primary               : constant Tag_Value := 28;
   Tag_Message_Routers          : constant Tag_Value := 30;
   Tag_OTS_Policy               : constant Tag_Value := 31;
   Tag_INV_Policy               : constant Tag_Value := 32;
   Tag_CSI_Sec_Mech_List        : constant Tag_Value := 33;
   Tag_NULL_Tag                 : constant Tag_Value := 34;
   Tag_SECIOP_Sec_Trans         : constant Tag_Value := 35;
   Tag_TLS_Sec_Trans            : constant Tag_Value := 36;
   Tag_Activity_Policy          : constant Tag_Value := 37;
   Tag_Group                    : constant Tag_Value := 39;
   --  TAO Value
   --  Tag_Group : constant Tag_Value := 1413566211;

   Tag_INET_Sec_Trans           : constant Tag_Value := 123;

   type TC_Unknown_Component is
     new Tagged_Component (Tag => Tag_Unknown, At_Most_Once => False)
     with record
        Unknown_Tag : Tag_Value;
        Data : Octet_Access;
     end record;

end PolyORB.GIOP_P.Tagged_Components;
