------------------------------------------------------------------------------
--                                                                          --
--                           ADABROKER SERVICES                             --
--                                                                          --
--       C O S N A M I N G . B I N D I N G I T E R A T O R . I M P L        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1999-2000 ENST Paris University, France.          --
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

with CORBA;

with CosNaming.BindingIterator.Helper;
with CosNaming.BindingIterator.Skel;
pragma Elaborate (CosNaming.BindingIterator.Skel);

with Ada.Unchecked_Deallocation;

with Broca.Server_Tools;

with GNAT.Task_Lock; use GNAT.Task_Lock;

package body CosNaming.BindingIterator.Impl is

   Null_Binding : constant Binding := (To_Sequence (0), Nobject);

   procedure Free is
      new Ada.Unchecked_Deallocation
     (Bindings.Element_Array, Binding_Element_Array_Ptr);

   ------------
   -- Create --
   ------------

   function Create return Object_Ptr is
      Obj : Object_Ptr;

   begin
      Obj := new Object;
      Obj.Self := Obj;
      return Obj;
   end Create;

   -------------
   -- Destroy --
   -------------

   procedure Destroy
     (Self : access Object) is
   begin
      Lock;
      if Self.Table /= null then
         Free (Self.Table);
      end if;
      Unlock;
   end Destroy;

   --------------
   -- Next_One --
   --------------

   procedure Next_One
     (Self    : access Object;
      B       : out CosNaming.Binding;
      Returns : out CORBA.Boolean) is
   begin
      Lock;
      if Self.Index <= Self.Table'Last then
         B := Self.Table (Self.Index);
         Self.Index := Self.Index + 1;
         Returns := True;

      else
         B := Null_Binding;
         Returns := False;
      end if;
      Unlock;
   end Next_One;

   ------------
   -- Next_N --
   ------------

   procedure Next_N
     (Self     : access Object;
      How_Many : in CORBA.Unsigned_Long;
      BL       : out CosNaming.BindingList;
      Returns  : out CORBA.Boolean)
   is
      First : Natural renames Self.Index;
      Last  : Natural;

   begin
      Lock;
      Last := Self.Index + Natural (How_Many) - 1;
      if Last <= Self.Table'Last then
         BL := BindingList (Bindings.To_Sequence (Self.Table (First .. Last)));
         Self.Index := Last + 1;
         Returns := True;

      else
         Returns := False;
      end if;
      Unlock;
   end Next_N;

end CosNaming.BindingIterator.Impl;
