------------------------------------------------------------------------------
--                                                                          --
--                              GLADE TOOLS                                 --
--                                                                          --
--          G L A D E _ N A M I N G _ I M P L E M E N T A T I O N           --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 1996-2006 Free Software Foundation, Inc.           --
--                                                                          --
-- GLADE  is free software;  you can redistribute it and/or modify it under --
-- terms of the  GNU General Public License  as published by the Free Soft- --
-- ware Foundation;  either version 2,  or (at your option)  any later ver- --
-- sion.  GLADE  is distributed  in the hope that  it will be  useful,  but --
-- WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHANTABI- --
-- LITY or  FITNESS FOR A PARTICULAR PURPOSE.  See the  GNU General Public  --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License  distributed  with GLADE;  see file COPYING.  If  --
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
--               GLADE  is maintained by ACT Europe.                        --
--               (email: glade-report@act-europe.fr)                        --
--                                                                          --
------------------------------------------------------------------------------

with GLADE.Lists;

package body GLADE_Naming_Implementation is

   type String_Access is access String;

   type Binding_Record is
      record
         B : Binding_Type;
         N : String_Access;
      end record;

   package Binding_List is new GLADE.Lists (Binding_Record);
   use Binding_List;

   ----------
   -- Bind --
   ----------

   procedure Bind
     (N : String;
      B : Binding_Type)
   is
      L : List;
      R : Binding_Record;
   begin
      Head (L);
      while Next (L) loop
         Read (L, R);
         if R.N.all = N then
            raise Already_Bound;
         end if;
      end loop;
      R.B := B;
      R.N := new String'(N);
      Append (L, R);
   end Bind;

   -------------
   -- Resolve --
   -------------

   function Resolve
     (N : String)
      return Binding_Type
   is
      L : List;
      R : Binding_Record;
   begin
      Head (L);
      while Next (L) loop
         Read (L, R);
         if R.N.all = N then
            return R.B;
         end if;
      end loop;
      raise Not_Found;
   end Resolve;

end GLADE_Naming_Implementation;
