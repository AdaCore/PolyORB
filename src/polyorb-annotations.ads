------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                  P O L Y O R B . A N N O T A T I O N S                   --
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

--  Support the addition of external information ("annotations")
--  to objects by their client. The object does not need to have
--  visibility on the client in order to allow itself to be annotated;
--  it only needs to expose a Notepad attribute.

--  $Id$

with PolyORB.Sequences.Unbounded;

package PolyORB.Annotations is

   pragma Elaborate_Body;

   type Note is abstract tagged private;
   --  A note that can be attached to an object.

   type Notepad is private;
   type Notepad_Access is access all Notepad;
   --  A space for clients of an object to attach Notes into.
   --  Notepad_Access can be used by private types to selectively
   --  expose one Notepad component to their clients.

   procedure Set_Note (NP : in out Notepad; N : Note'Class);
   --  Add note N to notepad NP. If of the same type already
   --  exists, it is replaced by N.

   procedure Get_Note (NP : Notepad; N : out Note'Class);
   --  Retrieve a note of N's type from NP.

   procedure Destroy (NP : in out Notepad);
   --  Removes all notes in NP and return any associated
   --  resources to the system.

private

   type Note is abstract tagged null record;
   type Note_Access is access all Note'Class;

   package Note_Seqs is new PolyORB.Sequences.Unbounded (Note_Access);
   subtype Note_Seq is Note_Seqs.Sequence;

   type Notepad is new Note_Seqs.Sequence;
   --  Cannot be declared as "type Notepad is new Note_Seq;"
   --  because this would be a derivation of a partial view
   --  whose full view is tagged within its immediate scope,
   --  which is illegal.

end PolyORB.Annotations;
