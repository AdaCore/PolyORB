------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                  P O L Y O R B . A N N O T A T I O N S                   --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2001-2012, Free Software Foundation, Inc.          --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.                                               --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
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

--  Support the addition of external information ("annotations")
--  to objects by their client. The object does not need to have
--  visibility on the client in order to allow itself to be annotated;
--  it only needs to expose a Notepad attribute.

with PolyORB.Utils.Chained_Lists;

package PolyORB.Annotations is

   pragma Preelaborate;

   type Note is abstract tagged private;
   --  A note that can be attached to an object.

   procedure Destroy (N : in out Note);
   --  Return any associated resource to the system. This primitive is
   --  called for every note in a notepad being destroyed.

   type Notepad is private;
   type Notepad_Access is access all Notepad;
   --  A space for clients of an object to attach Notes into.
   --  Notepad_Access can be used by private types to selectively
   --  expose one Notepad component to their clients.

   procedure Set_Note (NP : in out Notepad; N : Note'Class);
   --  Add note N to notepad NP. If a note with the same tag as N
   --  exists, it is replaced by N.

   procedure Get_Note (NP : Notepad; N : out Note'Class);
   --  Retrieve a note of N's type from NP. Constraint_Error is raised if no
   --  such note exists.

   procedure Get_Note (NP : Notepad; N : out Note'Class; Default : Note'Class);
   --  Retrieve a note of N's type from NP.
   --  Return Default if the note cannot be found.

   procedure Destroy (NP : in out Notepad);
   --  Removes all notes in NP and return any associated
   --  resources to the system.

private

   type Note is abstract tagged null record;
   type Note_Access is access all Note'Class;

   package Note_Lists is new PolyORB.Utils.Chained_Lists (Note_Access);

   type Notepad is new Note_Lists.List;

end PolyORB.Annotations;
