------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                         P O L Y O R B . D I C T                          --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                Copyright (C) 2001 Free Software Fundation                --
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
--              PolyORB is maintained by ENST Paris University.             --
--                                                                          --
------------------------------------------------------------------------------

--  A generic package to manage dictionnary (a map of associations between
--  keys and values).

--  $Id$

with Sequences.Unbounded;

generic

   type Key is private;
   type Value is private;

   with function "=" (K1 : Key; K2 : Key) return Boolean is <>;
   with function "=" (V1 : Value; K2 : Value) return Boolean is <>;

package PolyORB.Dict is

   Bad_Element : exception;
   Duplicate_Key : exception;

   Null_Index : constant Natural;

   type Dict is private;
   type Dict_Access is access all Dict;

   procedure Set
     (D : in out Dict; K : Key; V : Value; Index : out Natural);

   function Find_Key (D : Dict; K : Key) return Natural;
   function Find_Value (D : Dict; V : Value) return Natural;

   function Get (D : Dict; K : Key) return Value;
   function Get_By_Index (D : Dict; Index : Natural) return Value;

   procedure Remove (D : in out Dict; K : Key; V : out Value);
   procedure Remove_By_Index
     (D : in out Dict; Index : Natural; V : out Value);

private

   Null_Index : constant Natural := 0;

   type Dict_Entry is record
      K : Key;
      V : Value;
   end record;

   package Dict_Entry_Seqs is new Sequences.Unbounded (Dict_Entry);

   type Dict is record
      Entries : Dict_Entry_Seqs.Sequence;
   end record;

end PolyORB.Dict;

