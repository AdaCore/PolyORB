------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                 P O L Y O R B . D Y N A M I C _ D I C T                  --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2001-2010, Free Software Foundation, Inc.          --
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

--  A dynamic dictionnary of objects, indexed by Strings.

with PolyORB.Utils.HFunctions.Hyper;
with PolyORB.Utils.HTables.Perfect;

package body PolyORB.Dynamic_Dict is

   --  A hash table that stores the Value associated with a String key

   package Perfect_HTable is
      new PolyORB.Utils.HTables.Perfect
     (Value,
      PolyORB.Utils.HFunctions.Hyper.Hash_Hyper_Parameters,
      PolyORB.Utils.HFunctions.Hyper.Default_Hash_Parameters,
      PolyORB.Utils.HFunctions.Hyper.Hash,
      PolyORB.Utils.HFunctions.Hyper.Next_Hash_Parameters);

   use Perfect_HTable;

   T : Table_Instance;

   T_Initialized : Boolean := False;

   procedure Ensure_Initialization;
   pragma Inline (Ensure_Initialization);
   --  Ensure that T is initialized.

   ---------------------------
   -- Ensure_Initialization --
   ---------------------------

   procedure Ensure_Initialization is
   begin
      if T_Initialized then
         return;
      end if;
      Initialize (T);
      T_Initialized := True;
   end Ensure_Initialization;

   --------------
   -- For_Each --
   --------------

   procedure For_Each (Action : Dict_Action) is
      It : Iterator;
   begin
      Ensure_Initialization;
      It := First (T);
      while not Last (It) loop
         Action (K => Key (It), V => Perfect_HTable.Value (It));
         Next (It);
      end loop;
   end For_Each;

   ------------
   -- Lookup --
   ------------

   function Lookup
     (K       : String;
      Default : Value)
     return Value is
   begin
      Ensure_Initialization;
      return Lookup (T, K, Default);
   end Lookup;

   --------------
   -- Register --
   --------------

   procedure Register
     (K : String;
      V : Value) is
   begin
      Ensure_Initialization;
      Insert (T, K, V);
   end Register;

   -----------
   -- Reset --
   -----------

   procedure Reset is
   begin
      Finalize (T);
      Initialize (T);
   end Reset;

   ----------------
   -- Unregister --
   ----------------

   procedure Unregister
     (K : String) is
   begin
      Ensure_Initialization;
      Delete (T, K);
   end Unregister;

end PolyORB.Dynamic_Dict;
