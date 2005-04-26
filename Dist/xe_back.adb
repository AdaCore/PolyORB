------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--                              X E _ B A C K                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 1995-2005 Free Software Foundation, Inc.           --
--                                                                          --
-- GNATDIST is  free software;  you  can redistribute  it and/or  modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 2,  or  (at your option) any later --
-- version. GNATDIST is distributed in the hope that it will be useful, but --
-- WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHANTABI- --
-- LITY or FITNESS  FOR A PARTICULAR PURPOSE.  See the  GNU General  Public --
-- License  for more details.  You should  have received a copy of the  GNU --
-- General Public License distributed with  GNATDIST; see file COPYING.  If --
-- not, write to the Free Software Foundation, 59 Temple Place - Suite 330, --
-- Boston, MA 02111-1307, USA.                                              --
--                                                                          --
--                   GLADE  is maintained by AdaCore                        --
--                      (email: sales@adacore.com)                          --
--                                                                          --
------------------------------------------------------------------------------

with GNAT.HTable;

package body XE_Back is

   type String_Ptr is access all String;
   type Header_Num is range 1 .. 7;
   function Hash (S : String_Ptr) return Header_Num;

   function Eq (S1, S2 : String_Ptr) return Boolean;
   --  Test equality of designated strings

   package All_Backends is new GNAT.HTable.Simple_HTable
     (Header_Num => Header_Num,
      Element    => Backend_Access,
      No_Element => null,
      Key        => String_Ptr,
      Hash       => Hash,
      Equal      => Eq);

   function Eq (S1, S2 : String_Ptr) return Boolean is
   begin
      if S1 = null or else S2 = null then
         return S1 = S2;
      end if;
      return S1.all = S2.all;
   end Eq;

   function Find_Backend (PCS_Name : String) return Backend_Access is
      S : aliased String := PCS_Name;
   begin
      return All_Backends.Get (S'Unchecked_Access);
   end Find_Backend;

   function Hash (S : String_Ptr) return Header_Num is
      function Hash is new GNAT.HTable.Hash (Header_Num);
   begin
      if S = null then
         return Header_Num'First;
      end if;
      return Hash (S.all);
   end Hash;

   procedure Register_Backend (PCS_Name : String; The_Backend : Backend_Access)
   is
   begin
      All_Backends.Set (new String'(PCS_Name), The_Backend);
   end Register_Backend;

end XE_Back;
