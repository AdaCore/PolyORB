------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--        D Y N A M I C A N Y . D Y N S E Q U E N C E . H E L P E R         --
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

with PolyORB.Initialization;
with PolyORB.Utils.Strings;

package body DynamicAny.DynSequence.Helper is

   procedure Deferred_Initialization;

   -----------------------------
   -- Deferred_Initialization --
   -----------------------------

   procedure Deferred_Initialization is
   begin
      declare
         Name : constant CORBA.String := CORBA.To_CORBA_String ("DynSequence");
         Id   : constant CORBA.String
           := CORBA.To_CORBA_String ("IDL:omg.org/DynamicAny/DynSequence:1.0");

      begin
         CORBA.TypeCode.Internals.Add_Parameter
           (TC_DynSequence, CORBA.To_Any (Name));
         CORBA.TypeCode.Internals.Add_Parameter
           (TC_DynSequence, CORBA.To_Any (Id));
      end;
   end Deferred_Initialization;

   ------------------
   -- To_Local_Ref --
   ------------------

   function To_Local_Ref
     (The_Ref : CORBA.Object.Ref'Class)
      return DynamicAny.DynSequence.Local_Ref
   is
   begin
      if CORBA.Object.Is_Nil (The_Ref)
        or else CORBA.Object.Is_A (The_Ref, Repository_Id)
      then
         return Unchecked_To_Local_Ref (The_Ref);
      end if;

      CORBA.Raise_Bad_Param (CORBA.Default_Sys_Member);
   end To_Local_Ref;

   ----------------------------
   -- Unchecked_To_Local_Ref --
   ----------------------------

   function Unchecked_To_Local_Ref
     (The_Ref : CORBA.Object.Ref'Class)
      return DynamicAny.DynSequence.Local_Ref
   is
      Result : DynamicAny.DynSequence.Local_Ref;

   begin
      Set (Result, CORBA.Object.Object_Of (The_Ref));

      return Result;
   end Unchecked_To_Local_Ref;

begin
   declare
      use PolyORB.Initialization;
      use PolyORB.Initialization.String_Lists;
      use PolyORB.Utils.Strings;

   begin
      Register_Module
        (Module_Info'
         (Name      => +"DynamicAny.DynSequence.Helper",
          Conflicts => Empty,
          Depends   => +"any",
          Provides  => Empty,
          Implicit  => False,
          Init      => Deferred_Initialization'Access));
   end;
end DynamicAny.DynSequence.Helper;
