------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--               P O L Y O R B . S M A R T _ P O I N T E R S                --
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

--  $Id: //droopi/main/src/polyorb-smart_pointers.ads#7 $

with Ada.Finalization;

package PolyORB.Smart_Pointers is

   pragma Elaborate_Body;

   procedure Initialize;
   --  Initialize subsystem.

   procedure Finalize;
   --  Finalize subsystem.

   ------------
   -- Entity --
   ------------

   type Non_Controlled_Entity is abstract tagged limited private;
   procedure Finalize (X : in out Non_Controlled_Entity);
   --  Entity is the base type of all objects that can be
   --  referenced. It contains a Counter, which is the number of
   --  references to this object, and is automatically destroyed when
   --  the counter reaches 0. Before the entity is destroyed,
   --  the Finalize operation is called. NOTE however that
   --  Non_Controlled_Entity is *not* a controlled type: Finalize
   --  is *only* called when an Entity is destroyed as a result
   --  of its reference counter dropping to 0.

   type Entity_Ptr is access all Non_Controlled_Entity'Class;

   type Entity is abstract new Non_Controlled_Entity with private;
   procedure Initialize (X : in out Entity);
   --  An entity that is a controlled object. Contrary to
   --  Non_Controlled_Entity, the Finalize operation is called
   --  whenever the entity is finalized.

   ---------
   -- Ref --
   ---------

   type Ref is new Ada.Finalization.Controlled with private;
   --  The base type of all references. This type is often derived
   --  but never extended. It contains one field, which designate
   --  the referenced object.

   procedure Initialize (The_Ref : in out Ref);
   procedure Adjust     (The_Ref : in out Ref);
   procedure Finalize   (The_Ref : in out Ref);

   procedure Set
     (The_Ref : in out Ref;
      The_Entity : Entity_Ptr);

   procedure Unref (The_Ref : in out Ref)
     renames Finalize;

   function Is_Nil (The_Ref : Ref) return Boolean;
   function Is_Null (The_Ref : Ref) return Boolean
     renames Is_Nil;

   procedure Duplicate (The_Ref : in out Ref)
     renames Adjust;

   procedure Release (The_Ref : in out Ref);

   function Entity_Of (The_Ref : Ref) return Entity_Ptr;

   --  The following two low-level functions are exposed for
   --  cases where controlled types cannot be directly used
   --  in a personality. Great care must be taken when
   --  using them outside of this unit!

   procedure Inc_Usage (Obj : Entity_Ptr);
   procedure Dec_Usage (Obj : in out Entity_Ptr);

private

   type Non_Controlled_Entity is abstract tagged limited record
      Counter : Integer := 0;
   end record;

   type Entity_Controller (E : access Entity'Class)
      is new Ada.Finalization.Limited_Controlled
     with null record;

   procedure Initialize (X : in out Entity_Controller);
   procedure Finalize (X : in out Entity_Controller);

   type Entity is abstract new Non_Controlled_Entity with record
      Controller : Entity_Controller (Entity'Access);
   end record;

   type Ref is new Ada.Finalization.Controlled with
      record
         A_Ref : Entity_Ptr := null;
      end record;

end PolyORB.Smart_Pointers;
