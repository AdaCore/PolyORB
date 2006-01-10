------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                  D Y N A M I C A N Y . D Y N U N I O N                   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2005-2006, Free Software Foundation, Inc.          --
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

with CORBA.Object;

with DynamicAny.DynAny;
with DynamicAny.DynUnion.Impl;

package body DynamicAny.DynUnion is

   ------------------------
   -- Discriminator_Kind --
   ------------------------

   function Discriminator_Kind (Self : Local_Ref) return CORBA.TCKind is
      Self_Ref : constant CORBA.Object.Ref := CORBA.Object.Ref (Self);

   begin
      if CORBA.Object.Is_Nil (Self_Ref) then
         CORBA.Raise_Inv_Objref (CORBA.Default_Sys_Member);
      end if;

      return Impl.Discriminator_Kind (Impl.Object_Ptr (Entity_Of (Self)));
   end Discriminator_Kind;

   -----------------------
   -- Get_Discriminator --
   -----------------------

   function Get_Discriminator (Self : Local_Ref) return DynAny.Local_Ref is
      Self_Ref : constant CORBA.Object.Ref := CORBA.Object.Ref (Self);

   begin
      if CORBA.Object.Is_Nil (Self_Ref) then
         CORBA.Raise_Inv_Objref (CORBA.Default_Sys_Member);
      end if;

      return Impl.Get_Discriminator (Impl.Object_Ptr (Entity_Of (Self)));
   end Get_Discriminator;

   --------------------------
   -- Has_No_Active_Member --
   --------------------------

   function Has_No_Active_Member (Self : Local_Ref) return CORBA.Boolean is
      Self_Ref : constant CORBA.Object.Ref := CORBA.Object.Ref (Self);

   begin
      if CORBA.Object.Is_Nil (Self_Ref) then
         CORBA.Raise_Inv_Objref (CORBA.Default_Sys_Member);
      end if;

      return Impl.Has_No_Active_Member (Impl.Object_Ptr (Entity_Of (Self)));
   end Has_No_Active_Member;

   ------------
   -- Member --
   ------------

   function Member (Self : Local_Ref) return DynAny.Local_Ref is
      Self_Ref : constant CORBA.Object.Ref := CORBA.Object.Ref (Self);

   begin
      if CORBA.Object.Is_Nil (Self_Ref) then
         CORBA.Raise_Inv_Objref (CORBA.Default_Sys_Member);
      end if;

      return Impl.Member (Impl.Object_Ptr (Entity_Of (Self)));
   end Member;

   -----------------
   -- Member_Kind --
   -----------------

   function Member_Kind (Self : Local_Ref) return CORBA.TCKind is
      Self_Ref : constant CORBA.Object.Ref := CORBA.Object.Ref (Self);

   begin
      if CORBA.Object.Is_Nil (Self_Ref) then
         CORBA.Raise_Inv_Objref (CORBA.Default_Sys_Member);
      end if;

      return Impl.Member_Kind (Impl.Object_Ptr (Entity_Of (Self)));
   end Member_Kind;

   -----------------
   -- Member_Name --
   -----------------

   function Member_Name (Self : Local_Ref) return FieldName is
      Self_Ref : constant CORBA.Object.Ref := CORBA.Object.Ref (Self);

   begin
      if CORBA.Object.Is_Nil (Self_Ref) then
         CORBA.Raise_Inv_Objref (CORBA.Default_Sys_Member);
      end if;

      return Impl.Member_Name (Impl.Object_Ptr (Entity_Of (Self)));
   end Member_Name;

   -----------------------
   -- Set_Discriminator --
   -----------------------

   procedure Set_Discriminator
     (Self : Local_Ref;
      D    : DynAny.Local_Ref)
   is
      Self_Ref : constant CORBA.Object.Ref := CORBA.Object.Ref (Self);

   begin
      if CORBA.Object.Is_Nil (Self_Ref) then
         CORBA.Raise_Inv_Objref (CORBA.Default_Sys_Member);
      end if;

      Impl.Set_Discriminator (Impl.Object_Ptr (Entity_Of (Self)), D);
   end Set_Discriminator;

   ---------------------------
   -- Set_To_Default_Member --
   ---------------------------

   procedure Set_To_Default_Member (Self : Local_Ref) is
      Self_Ref : constant CORBA.Object.Ref := CORBA.Object.Ref (Self);

   begin
      if CORBA.Object.Is_Nil (Self_Ref) then
         CORBA.Raise_Inv_Objref (CORBA.Default_Sys_Member);
      end if;

      Impl.Set_To_Default_Member (Impl.Object_Ptr (Entity_Of (Self)));
   end Set_To_Default_Member;

   -----------------------------
   -- Set_To_No_Active_Member --
   -----------------------------

   procedure Set_To_No_Active_Member (Self : Local_Ref) is
      Self_Ref : constant CORBA.Object.Ref := CORBA.Object.Ref (Self);

   begin
      if CORBA.Object.Is_Nil (Self_Ref) then
         CORBA.Raise_Inv_Objref (CORBA.Default_Sys_Member);
      end if;

      Impl.Set_To_No_Active_Member (Impl.Object_Ptr (Entity_Of (Self)));
   end Set_To_No_Active_Member;

end DynamicAny.DynUnion;
