------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                POLYORB.GIOP_P.TAGGED_COMPONENTS.POLICIES                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2003-2004 Free Software Foundation, Inc.           --
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
--                PolyORB is maintained by ACT Europe.                      --
--                    (email: sales@act-europe.fr)                          --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Streams;
with Ada.Unchecked_Deallocation;

with PolyORB.Initialization;
pragma Elaborate_All (PolyORB.Initialization); --  WAG:3.15

with PolyORB.Utils.Chained_Lists;
with PolyORB.Utils.Strings;

package body PolyORB.GIOP_P.Tagged_Components.Policies is

   use Ada.Streams;
   use PolyORB.Representations.CDR;

   package Sub_Component_Allocator_Lists is
      new PolyORB.Utils.Chained_Lists (Fetch_Sub_Component_Func_Access);

   Sub_Component_Allocators : Sub_Component_Allocator_Lists.List;

   ----------------------------
   -- Create_Empty_Component --
   ----------------------------

   function Create_Empty_Component return Tagged_Component_Access;

   function Create_Empty_Component return Tagged_Component_Access is
   begin
      return new TC_Policies;
   end Create_Empty_Component;

   ---------------------
   -- Fetch_Component --
   ---------------------

   function Fetch_Component
     (Oid : access PolyORB.Objects.Object_Id)
     return Tagged_Component_Access;

   function Fetch_Component
     (Oid : access PolyORB.Objects.Object_Id)
     return Tagged_Component_Access
   is
      use Sub_Component_Allocator_Lists;
      use type PolyORB.Types.Unsigned_Long;

      It : Iterator := First (Sub_Component_Allocators);

      Result : Tagged_Component_Access;
      Policy : Policy_Value;

   begin
      while not Last (It) loop
         Policy := Value (It).all (Oid);

         if Policy.P_Type /= Invalid_Policy_Type then
            if Result = null then
               Result := new TC_Policies;
            end if;

            Policy_Value_Seq.Append
              (TC_Policies (Result.all).Policies,
               Policy);
         end if;
         Next (It);
      end loop;

      return Result;
   end Fetch_Component;

   --------------
   -- Marshall --
   --------------

   procedure Marshall (C : access TC_Policies; Buffer : access Buffer_Type) is
      use Policy_Value_Seq;

      It : Iterator := First (C.Policies);

   begin

      --  Length of Policy_Value_Seq

      Marshall (Buffer, PolyORB.Types.Unsigned_Long (Length (C.Policies)));

      --  Marshall Policy_Value_Seq elements

      while not Last (It) loop
         Marshall (Buffer, Value (It).P_Type);
         Marshall (Buffer, Value (It).P_Value.all);

         Next (It);
      end loop;
   end Marshall;

   ----------------
   -- Unmarshall --
   ----------------

   procedure Unmarshall
     (C      : access TC_Policies;
      Buffer : access Buffer_Type)
   is
      use Ada.Streams;
      use Policy_Value_Seq;

      Length : PolyORB.Types.Unsigned_Long;

      Temp_Policy_Value : Policy_Value;
   begin

      Length := Unmarshall (Buffer);

      for J in 1 .. Length loop
         Temp_Policy_Value.P_Type := Unmarshall (Buffer);
         Temp_Policy_Value.P_Value
           := new Stream_Element_Array'(Unmarshall (Buffer));

         Append (C.Policies, Temp_Policy_Value);
      end loop;
   end Unmarshall;

   ----------------------
   -- Release_Contents --
   ----------------------

   procedure Release_Contents (C : access TC_Policies) is
      procedure Free is
         new Ada.Unchecked_Deallocation (TC_Policies, TC_Policies_Access);

      procedure Free is
         new Ada.Unchecked_Deallocation (Encapsulation, Encapsulation_Access);

      use Policy_Value_Seq;

      CC : TC_Policies_Access := TC_Policies_Access (C);
      It : Iterator := First (C.Policies);

   begin
      while not Last (It) loop
         Free (Value (It).P_Value);
         Remove (C.Policies, It);
      end loop;

      Deallocate (C.Policies);
      Free (CC);
   end Release_Contents;

   --------------
   -- Register --
   --------------

   procedure Register
     (Fetch_Sub_Component : Fetch_Sub_Component_Func_Access)
   is
      use Sub_Component_Allocator_Lists;

   begin
      Append (Sub_Component_Allocators, Fetch_Sub_Component);
   end Register;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize;

   procedure Initialize is
   begin
      Register (Tag_Policies,
                Create_Empty_Component'Access,
                Fetch_Component'Access);
   end Initialize;

   use PolyORB.Initialization;
   use PolyORB.Utils.Strings;

begin
   Register_Module
     (Module_Info'
      (Name      => +"tagged_components.policies",
       Conflicts => PolyORB.Initialization.String_Lists.Empty,
       Depends   => PolyORB.Initialization.String_Lists.Empty,
       Provides  => PolyORB.Initialization.String_Lists.Empty,
       Implicit  => False,
       Init      => Initialize'Access));
end PolyORB.GIOP_P.Tagged_Components.Policies;
