------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--              B R O C A . P R O T E C T E D _ O B J E C T S               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1999-2001 ENST Paris University, France.          --
--                                                                          --
-- AdaBroker is free software; you  can  redistribute  it and/or modify it  --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. AdaBroker  is distributed  in the hope that it will be  useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with AdaBroker; see file COPYING. If  --
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
--             AdaBroker is maintained by ENST Paris University.            --
--                     (email: broker@inf.enst.fr)                          --
--                                                                          --
------------------------------------------------------------------------------

with Broca.Soft_Links;

package Broca.Protected_Objects is

   pragma Elaborate_Body;

   procedure Initialize;

   -------------------------------------------
   -- Critical Section for ORB with Tasking --
   -------------------------------------------

   procedure Enter_Critical_Section;

   procedure Leave_Critical_Section;

   ----------------------------------
   -- Barrier for ORB with Tasking --
   ----------------------------------

   type Protected_Barrier_Type is new Soft_Links.Barrier_Type with private;

   function Create return Soft_Links.Barrier_Access;

   procedure Destroy (B : in out Protected_Barrier_Type);

   procedure Signal
     (B : in Protected_Barrier_Type;
      N : in Positive := 1);

   procedure Signal_All
     (B : in Protected_Barrier_Type;
      P : in Boolean := True);

   procedure Wait (B : in Protected_Barrier_Type);

   --------------------------------
   -- Mutex for PCS with Tasking --
   --------------------------------

   type Protected_Mutex_Type is new Soft_Links.Mutex_Type with private;

   function Create return Soft_Links.Mutex_Access;

   procedure Enter (M : in Protected_Mutex_Type);

   procedure Destroy (M : in out Protected_Mutex_Type);

   procedure Leave (M : in Protected_Mutex_Type);

   ----------------------------------
   -- Watcher for PCS with Tasking --
   ----------------------------------

   type Protected_Watcher_Type is new Soft_Links.Watcher_Type with private;

   function Create return Soft_Links.Watcher_Access;

   procedure Destroy (W : in out Protected_Watcher_Type);

   procedure Differ
     (W : in Protected_Watcher_Type;
      V : in Soft_Links.Version_Id);

   procedure Lookup
     (W : in Protected_Watcher_Type;
      V : out Soft_Links.Version_Id);

   procedure Update (W : in Protected_Watcher_Type);

   -----------------------------------------
   -- Advanced Mutex for PCS with Tasking --
   -----------------------------------------

   type Protected_Adv_Mutex_Type is new Soft_Links.Adv_Mutex_Type with private;

   function Create return Soft_Links.Adv_Mutex_Access;

   procedure Enter (M : in Protected_Adv_Mutex_Type);

   procedure Destroy (M : in out Protected_Adv_Mutex_Type);

   procedure Leave (M : in Protected_Adv_Mutex_Type);

private

   type Barrier_PO;

   type Barrier_PO_Access is access Barrier_PO;

   type Protected_Barrier_Type is new Soft_Links.Barrier_Type
     with record
        X : Barrier_PO_Access;
     end record;


   type Mutex_PO;

   type Mutex_PO_Access is access Mutex_PO;

   type Protected_Mutex_Type is new Soft_Links.Mutex_Type
     with record
        X : Mutex_PO_Access;
     end record;

   type Watcher_PO;

   type Watcher_PO_Access is access Watcher_PO;

   type Protected_Watcher_Type is new Soft_Links.Watcher_Type
     with record
        X : Watcher_PO_Access;
     end record;


   type Adv_Mutex_PO;

   type Adv_Mutex_PO_Access is access Adv_Mutex_PO;

   type Protected_Adv_Mutex_Type is new Soft_Links.Adv_Mutex_Type
     with record
        X : Adv_Mutex_PO_Access;
     end record;

end Broca.Protected_Objects;
