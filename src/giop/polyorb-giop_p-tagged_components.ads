------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--     P O L Y O R B . G I O P _ P . T A G G E D _ C O M P O N E N T S      --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--            Copyright (C) 2003 Free Software Foundation, Inc.             --
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
with PolyORB.Sequences.Unbounded;
with PolyORB.Types;

package PolyORB.GIOP_P.Tagged_Components is

   use PolyORB.Buffers;

   Tagged_Components_Error : exception;

   ----------------------
   -- Tagged_Component --
   ----------------------

   type Tagged_Component is abstract tagged record
      Tag : Types.Unsigned_Long := Types.Unsigned_Long'Last;
   end record;

   type Tagged_Component_Access is access all Tagged_Component'Class;

   procedure Marshall
     (C      : access Tagged_Component;
      Buffer : access Buffer_Type)
      is abstract;

   procedure Unmarshall
     (C      : access Tagged_Component;
      Buffer : access Buffer_Type)
      is abstract;

   function Get_Tag
     (C : access Tagged_Component)
     return Types.Unsigned_Long
      is abstract;

   procedure Release_Contents
     (C : access Tagged_Component)
      is abstract;
   --  free memory associate with component

   ---------------------------
   -- Tagged_Component_List --
   ---------------------------

   type Tagged_Component_List is private;

   Null_Tagged_Component_List : constant Tagged_Component_List;

   procedure Release_Contents
     (List : Tagged_Component_List);

   procedure Marshall_Tagged_Component
     (Buffer     : access Buffer_Type;
      Components :        Tagged_Component_List);

   function Unmarshall_Tagged_Component
     (Buffer     : access Buffer_Type)
     return Tagged_Component_List;

   function Get_Component
     (List : Tagged_Component_List;
      Tag  : Types.Unsigned_Long)
     return Tagged_Component_Access;

   procedure Add
     (List : in out Tagged_Component_List;
      C    :        Tagged_Component_Access);

   -------------------------
   -- Register components --
   -------------------------

   type New_Component_Func_Access is access
     function return Tagged_Component_Access;

   procedure Register
     (Tag : Types.Unsigned_Long;
      F   : New_Component_Func_Access);

   -----------------------
   -- Unknown Component --
   -----------------------

   type Octet_Access is access all Ada.Streams.Stream_Element_Array;

   type Unknown_Component is new Tagged_Component with record
      Data : Octet_Access;
   end record;

   type Unknown_Component_Access is access all Unknown_Component'Class;

   procedure Marshall
     (C      : access Unknown_Component;
      Buffer : access Buffer_Type);

   procedure Unmarshall
     (C      : access Unknown_Component;
      Buffer : access Buffer_Type);

   function Get_Tag
     (C : access Unknown_Component)
     return Types.Unsigned_Long;

   procedure Release_Contents
     (C : access Unknown_Component);

private

   package Component_Seq is new
     PolyORB.Sequences.Unbounded (Tagged_Component_Access);

   type Tagged_Component_List is new Component_Seq.Sequence;

   Null_Tagged_Component_List : constant Tagged_Component_List :=
     Tagged_Component_List (Component_Seq.Null_Sequence);

end PolyORB.GIOP_P.Tagged_Components;
