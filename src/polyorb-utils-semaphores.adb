------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                   P O L Y O R B . S E M A P H O R E S                    --
--                                                                          --
--                                 I m p l                                  --
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
--  This package provides an implementation of semaphores

--  $Id$


package body PolyORB.Utils.Semaphores is

   function State (S : Semaphore_Access) return Natural
   is
   begin
      return S.N;
   end State;

   procedure Create (S : out Semaphore_Access)
   is
   begin
      S := new Semaphore'(0, null, null, null);
      Create (S.Down_Lock);
      Create (S.Full_Lock);
      Create (S.Is_Empty);
      Enter (S.Is_Empty);
   end Create;

   procedure Up (S : Semaphore_Access)
   is
   begin
      Enter (S.Full_Lock);
      if S.N = 0 then
         Leave (S.Is_Empty);
      end if;
      S.N := S.N + 1;
      Leave (S.Full_Lock);
   end Up;

   procedure Down (S : Semaphore_Access)
   is
   begin
      Enter (S.Down_Lock);
      Enter (S.Full_Lock);
      if S.N = 0 then
         Leave (S.Full_Lock);
         Enter (S.Is_Empty);
         Leave (S.Is_Empty);
         Enter (S.Full_Lock);
      end if;
      S.N := S.N - 1;
      if S.N = 0 then
         Enter (S.Is_Empty);
      end if;
      Leave (S.Full_Lock);
      Leave (S.Down_Lock);
   end Down;

end PolyORB.Utils.Semaphores;
