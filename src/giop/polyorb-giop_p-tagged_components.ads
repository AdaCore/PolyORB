------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--     P O L Y O R B . G I O P _ P . T A G G E D _ C O M P O N E N T S      --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2003-2004 Free Software Foundation, Inc.           --
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

--  Implementation of CORBA IOR Tagged components

with Ada.Streams;

with PolyORB.Buffers;
with PolyORB.Objects;
with PolyORB.Sequences.Unbounded;
with PolyORB.Types;

package PolyORB.GIOP_P.Tagged_Components is

   use PolyORB.Buffers;

   type Tag_Value is new Types.Unsigned_Long;

   ----------------------
   -- Tagged_Component --
   ----------------------

   type Tagged_Component (Tag : Tag_Value) is abstract tagged private;

   type Tagged_Component_Access is access all Tagged_Component'Class;

   procedure Marshall
     (C      : access Tagged_Component;
      Buffer : access Buffer_Type)
      is abstract;
   --  Marshall tagged component

   procedure Unmarshall
     (C      : access Tagged_Component;
      Buffer : access Buffer_Type)
      is abstract;
   --  Unmarshall tagged component

   procedure Release_Contents
     (C : access Tagged_Component)
      is abstract;
   --  Free memory associated with component

   ---------------------------
   -- Tagged_Component_List --
   ---------------------------

   type Tagged_Component_List is private;

   Null_Tagged_Component_List : constant Tagged_Component_List;
   --  Empty list

   procedure Release_Contents (List : Tagged_Component_List);
   --  Free memory for all tags in list

   procedure Marshall_Tagged_Component
     (Buffer     : access Buffer_Type;
      Components :        Tagged_Component_List);
   --  Marshall Tagged Component List

   function Unmarshall_Tagged_Component
     (Buffer     : access Buffer_Type)
     return Tagged_Component_List;
   --  Unarshall Tagged Component List

   function Get_Component
     (List : Tagged_Component_List;
      Tag  : Tag_Value)
     return Tagged_Component_Access;
   --  Search and return a component in a tagged component list

   function Fetch_Components
     (Oid : access PolyORB.Objects.Object_Id)
     return Tagged_Component_List;
   --  Return a Tagget_Component_List of all tagged components
   --  configured for object represented by Oid.

   procedure Add
     (List : in out Tagged_Component_List;
      C    :        Tagged_Component_Access);
   --  Add a component in a tagged component list

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

   -----------------------
   -- Unknown Component --
   -----------------------

   --  Unknown component is used when tag is unknown at unmarshalling time.
   --  Users cannot access to unknown components data, but unknown
   --  components can be remarshalled without being modified.

   type Octet_Access is access all Ada.Streams.Stream_Element_Array;
   --  Data in an unknow tagged component

   type TC_Unknown_Component is
     new Tagged_Component (Tag_Value'Last) with private;
   type TC_Unknown_Component_Access is access all TC_Unknown_Component'Class;

   procedure Marshall
     (C      : access TC_Unknown_Component;
      Buffer : access Buffer_Type);

   procedure Unmarshall
     (C      : access TC_Unknown_Component;
      Buffer : access Buffer_Type);

   procedure Release_Contents
     (C : access TC_Unknown_Component);

   --------------
   -- Tag List --
   --------------

   Tag_Policies : constant Tag_Value;
   Tag_Group    : constant Tag_Value;

private

   type Tagged_Component (Tag : Tag_Value) is abstract tagged null record;

   type TC_Unknown_Component is
     new Tagged_Component (Tag_Value'Last) with record
        Unknown_Tag : Tag_Value;
        Data : Octet_Access;
     end record;

   package Component_Seq is new
     PolyORB.Sequences.Unbounded (Tagged_Component_Access);

   --  Tagged component list

   type Tagged_Component_List is new Component_Seq.Sequence;

   Null_Tagged_Component_List : constant Tagged_Component_List
     := Tagged_Component_List (Component_Seq.Null_Sequence);

   --------------
   -- Tag List --
   --------------

   Tag_Policies : constant Tag_Value := 2;
   Tag_Group    : constant Tag_Value := 39;
   --  TAO Value
   --  Tag_Group : constant Tag_Value := 1413566211;

end PolyORB.GIOP_P.Tagged_Components;
