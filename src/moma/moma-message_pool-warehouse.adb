------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                 P O L Y O R B . D Y N A M I C _ D I C T                  --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--             Copyright (C) 1999-2002 Free Software Fundation              --
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

--  A dynamic, protected dictionnary of objects, indexed by Strings.

--  $Id$

with PolyORB.Utils.HTables.Perfect;
with PolyORB.Locks;

package body MOMA.Message_Pool.Warehouse is

   use PolyORB.Locks;

   ---------------------------
   -- Ensure_Initialization --
   ---------------------------

   procedure Ensure_Initialization (W : in out Warehouse) is
   begin
      if W.T_Initialized then
         return;
      end if;
      Initialize (W.T);
      Create (W.T_Lock);
      W.T_Initialized := True;
   end Ensure_Initialization;

   ------------
   -- Lookup --
   ------------

   function Lookup
     (W : Warehouse;
      K : String)
      return MOMA.Types.String
   is
      Result : MOMA.Types.String;
      Temp : Warehouse := W;
   begin
      Ensure_Initialization (Temp);

      Lock_R (W.T_Lock);
      Result := Lookup (W.T, K);
      Unlock_R (W.T_Lock);

      return Result;
      exception
         when No_Key => raise Key_Not_Found;
   end Lookup;

   function Lookup
     (W : Warehouse;
      K : String;
      Default : MOMA.Types.String)
     return MOMA.Types.String
   is
      V : MOMA.Types.String;
      Temp : Warehouse := W;
   begin
      Ensure_Initialization (Temp);

      Lock_R (W.T_Lock);
      V := Perfect_Htable.Lookup (W.T, K, Default);
      Unlock_R (W.T_Lock);

      return V;
   end Lookup;

   --------------
   -- Register --
   --------------

   procedure Register
     (W : Warehouse;
      K : String;
      V : MOMA.Types.String)
   is
      Temp : Warehouse := W;
   begin
      Ensure_Initialization (Temp);

      Lock_W (W.T_Lock);
      Insert (W.T, K, V);
      Unlock_W (W.T_Lock);
   end Register;

   ----------------
   -- Unregister --
   ----------------

   procedure Unregister
     (W : Warehouse;
      K : String)
   is
      V : MOMA.Types.String;
      Temp : Warehouse := W;
   begin
      Ensure_Initialization (Temp);

      Lock_R (W.T_Lock);
      V := Lookup (W.T, K);
      Unlock_R (W.T_Lock);

      Lock_W (W.T_Lock);
      Delete (W.T, K);
      Unlock_W (W.T_Lock);

   exception
      when No_Key => raise Key_Not_Found;
   end Unregister;

end MOMA.Message_Pool.Warehouse;
