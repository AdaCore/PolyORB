------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                      P O L Y O R B . O B J E C T S                       --
--                                                                          --
--                                 B o d y                                  --
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

--  Root type for concrete object implementations (servants).

--  $Id$

with Ada.Unchecked_Deallocation;

with PolyORB.Utils;

package body PolyORB.Objects is

   use Ada.Streams;

   ----------
   -- Free --
   ----------

   procedure Free (X : in out Object_Id_Access)
   is
      procedure Free is new Ada.Unchecked_Deallocation
        (Object_Id, Object_Id_Access);
   begin
      Free (X);
   end Free;

   ---------------
   -- To_String --
   ---------------

   function To_String (Oid : Object_Id) return String is
   begin
      return Utils.To_String (Stream_Element_Array (Oid));
   end To_String;

   ------------
   -- To_Oid --
   ------------

   function To_Oid (S : String) return Object_Id is
   begin
      return Object_Id (Utils.To_Stream_Element_Array (S));
   end To_Oid;

   -----------
   -- Image --
   -----------

   function Image (Oid : Object_Id) return String
     renames To_String;

end PolyORB.Objects;
