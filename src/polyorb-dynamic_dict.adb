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

--  A dynamic dictionnary of objects, indexed by Strings.

--  $Id$

with PolyORB.Utils.HTables.Perfect;

package body PolyORB.Dynamic_Dict is

   --------------------------------------------------------
   -- A hash table that stores the Value associated with --
   -- a String key.                                      --
   --------------------------------------------------------

   package Perfect_Htable is
      new PolyORB.Utils.HTables.Perfect (Value);

   use Perfect_Htable;

   T : Table_Instance;

   T_Initialize : Boolean := False;

   Prime : constant Natural := 1777771;
   Max   : constant Natural := 10;

   ------------
   -- Lookup --
   ------------

   function Lookup
     (K : String)
      return Value
   is
   begin
      if T_Initialize = False then
         Initialize (T, Prime, Max);
         T_Initialize := True;
      end if;
      return Lookup (T, K);
      exception
         when No_Key => raise Key_Not_Found;
   end Lookup;

   function Lookup
     (K : String;
      Default : Value)
     return Value
   is
      V : Value;
   begin
      if T_Initialize = False then
         Initialize (T, Prime, Max);
         T_Initialize := True;
      end if;
      V := Lookup (T, K, Default);
      return V;
   end Lookup;

   procedure Register
     (K : String;
      V : Value)
   is
   begin
      if T_Initialize = False then
         Initialize (T, Prime, Max);
         T_Initialize := True;
      end if;
      Insert (T, K, V);
   end Register;

   procedure Unregister
     (K : String)
   is
      V : Value;
   begin
      if T_Initialize = False then
         Initialize (T, Prime, Max);
         T_Initialize := True;
      end if;
      V := Lookup (T, K);
      Delete (T, K);
   exception
      when No_Key => raise Key_Not_Found;
   end Unregister;

end PolyORB.Dynamic_Dict;
