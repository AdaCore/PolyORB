------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--             POLYORB.SERVICES.NAMING.BINDINGITERATOR.HELPER               --
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

--  $Id$

pragma Warnings (Off);

with PolyORB.Utils.Strings;
with PolyORB.Initialization;
pragma Elaborate_All (PolyORB.Initialization);
with CORBA;
with CORBA.Object.Helper;
with CORBA.Object;
with PolyORB.Exceptions;

package body PolyORB.Services.Naming.BindingIterator.Helper is

   pragma Warnings (Off);
   --  Constructing typecodes tends to yield long lines.

   function Unchecked_To_Ref
     (The_Ref : in CORBA.Object.Ref'Class)
     return CosNaming.BindingIterator.Ref
   is
      Result : CosNaming.BindingIterator.Ref;
   begin
      Set (Result,
           CORBA.Object.Object_Of (The_Ref));
      return Result;
   end Unchecked_To_Ref;

   function To_Ref
     (The_Ref : in CORBA.Object.Ref'Class)
     return CosNaming.BindingIterator.Ref
   is
      use CORBA;
   begin
      if CORBA.Object.Is_Nil (The_Ref)
        or else CORBA.Object.Is_A (The_Ref, Repository_Id) then
         return Unchecked_To_Ref (The_Ref);
      end if;
      PolyORB.Exceptions.Raise_Bad_Param;
   end To_Ref;

   function From_Any (Item : in CORBA.Any)
      return CosNaming.BindingIterator.Ref is
   begin
      return To_Ref (CORBA.Object.Helper.From_Any (Item));
   end From_Any;

   function To_Any
     (Item : in CosNaming.BindingIterator.Ref)
     return CORBA.Any is
      A : CORBA.Any := CORBA.Object.Helper.To_Any
        (CORBA.Object.Ref (Item));
   begin
      CORBA.Set_Type (A, TC_BindingIterator);
      return A;
   end To_Any;

   Deferred_Initialization_Done : Boolean := False;

   procedure Deferred_Initialization is
      begin
         if not Deferred_Initialization_Done then
            null;

            declare
               Name : CORBA.String := CORBA.To_CORBA_String ("BindingIterator");
               Id : CORBA.String := CORBA.To_CORBA_String ("IDL:omg.org/CosNaming/BindingIterator:1.0");
            begin
               CORBA.TypeCode.Add_Parameter (TC_BindingIterator, CORBA.To_Any (Name));
               CORBA.TypeCode.Add_Parameter (TC_BindingIterator, CORBA.To_Any (Id));
            end;
         end if;

         Deferred_Initialization_Done := True;
      end Deferred_Initialization;

begin
   declare
      use PolyORB.Initialization;
      use PolyORB.Initialization.String_Lists;
      use PolyORB.Utils.Strings;
   begin
      Register_Module
        (Module_Info'
         (Name      => +"CosNaming.BindingIterator.Helper",
          Conflicts => Empty,
          Depends   =>
            +"soft_links"
          ,
          Provides  => Empty,
          Init      => Deferred_Initialization'Access));
   end;

end PolyORB.Services.Naming.BindingIterator.Helper;
