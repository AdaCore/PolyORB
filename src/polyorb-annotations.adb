------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                  P O L Y O R B . A N N O T A T I O N S                   --
--                                                                          --
--                                 B o d y                                  --
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

with Ada.Tags;
with PolyORB.Utils.Unchecked_Deallocation;

package body PolyORB.Annotations is

   use Ada.Tags;
   use Note_Lists;

   --------------
   -- Set_Note --
   --------------

   procedure Set_Note (NP : in out Notepad; N : Note'Class) is
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

   -------------------------
   -- Get_Note_If_Present --
   -------------------------

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

   --------------
   -- Get_Note --
   --------------

   procedure Get_Note
     (NP : Notepad;
      N : out Note'Class)
   is
      Present : Boolean;
   begin
      Get_Note_If_Present (NP, N, Present);
      if not Present then
         raise Constraint_Error;
      end if;
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

   procedure Destroy (NP : in out Notepad) is
      It : Iterator := First (NP);
      procedure Free is new PolyORB.Utils.Unchecked_Deallocation.Free
        (Object => Note'Class,
         Name => Note_Access);
   begin
      while not Last (It) loop
         Destroy (Value (It).all.all);
         Free (Value (It).all);
         Next (It);
      end loop;
      Deallocate (NP);
   end Destroy;

   procedure Destroy (N : in out Note) is
      pragma Unreferenced (N);

   begin
      null;
   end Destroy;

end PolyORB.Annotations;
