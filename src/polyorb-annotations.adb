------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                  P O L Y O R B . A N N O T A T I O N S                   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--             Copyright (C) 1999-2003 Free Software Fundation              --
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

with Ada.Tags;
with Ada.Unchecked_Deallocation;

package body PolyORB.Annotations is

   use Ada.Tags;
   use Note_Lists;

   --------------
   -- Set_Note --
   --------------

   procedure Set_Note (NP : in out Notepad; N : Note'Class)
   is
      It : Iterator := First (NP);
   begin
      while not Last (It) loop
         if Value (It).all'Tag = N'Tag then
            Value (It).all.all := N;
            --  Here we have checked that The_Notes (I).all and N
            --  are of the same type, but this does not guarantee
            --  that the assignment will succeed: Constraint_Error
            --  may be raised if that type has known discriminants
            --  without default values.
            return;
         end if;
         Next (It);
      end loop;

      Append (NP, new Note'Class'(N));
   end Set_Note;

   --------------
   -- Get_Note --
   --------------

   procedure Get_Note_If_Present
     (NP : Notepad;
      N  : out Note'Class;
      Present : out Boolean);

   procedure Get_Note_If_Present
     (NP : Notepad;
      N  : out Note'Class;
      Present : out Boolean)
   is
      It : Iterator := First (NP);
   begin
      while not Last (It) loop
         if Value (It).all'Tag = N'Tag then
            N := Value (It).all.all;
            Present := True;
            return;
         end if;
         Next (It);
      end loop;
      Present := False;
   end Get_Note_If_Present;

   procedure Get_Note
     (NP : Notepad;
      N : out Note'Class)
   is
      Present : Boolean;
   begin
      Get_Note_If_Present (NP, N, Present);
   end Get_Note;

   procedure Get_Note
     (NP : Notepad;
      N : out Note'Class;
      Default : Note'Class)
   is
      Present : Boolean;
   begin
      Get_Note_If_Present (NP, N, Present);
      if not Present then
         N := Default;
      end if;
   end Get_Note;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (NP : in out Notepad)
   is
      procedure Free is new Ada.Unchecked_Deallocation
        (Note'Class, Note_Access);

      It : Iterator := First (NP);
   begin
      while not Last (It) loop
         Free (Value (It).all);
         Next (It);
      end loop;

      Destroy (NP);
   end Destroy;

end PolyORB.Annotations;
