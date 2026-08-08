------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                POLYORB.GIOP_P.TAGGED_COMPONENTS.POLICIES                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2003-2012, Free Software Foundation, Inc.          --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.                                               --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

pragma Ada_2012;

with Ada.Streams;
with PolyORB.Utils.Unchecked_Deallocation;

with PolyORB.Initialization;

with PolyORB.Utils.Strings;

package body PolyORB.GIOP_P.Tagged_Components.Policies is

   use Ada.Streams;
   use PolyORB.Representations.CDR.Common;

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

   ---------------
   -- Duplicate --
   ---------------

   overriding function Duplicate
     (C : TC_Policies)
     return Tagged_Component_Access
   is
      Result : constant Tagged_Component_Access := new TC_Policies;
      Iter : Policy_Value_Seq.Iterator := Policy_Value_Seq.First (C.Policies);

   begin
      while not Policy_Value_Seq.Last (Iter) loop
         Policy_Value_Seq.Append
           (TC_Policies (Result.all).Policies,
            Policy_Value'
            (Policy_Value_Seq.Value (Iter).P_Type,
             new Encapsulation'(Policy_Value_Seq.Value (Iter).P_Value.all)));
         Policy_Value_Seq.Next (Iter);
      end loop;

      return Result;
   end Duplicate;

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

      It : Sub_Component_Allocator_Lists.Iterator
        := First (Sub_Component_Allocators);

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

   -----------------------------
   -- Marshall_Component_Data --
   -----------------------------

   overriding procedure Marshall_Component_Data
     (C : access TC_Policies;
      Buffer : access Buffer_Type)
   is
      use Policy_Value_Seq;

      It : Policy_Value_Seq.Iterator := First (C.Policies);
      Temp_Buf : Buffer_Access := new Buffer_Type;

   begin
      --  The body of a Tag_Policy component is an encapsulation

      Start_Encapsulation (Temp_Buf);

      --  Length of Policy_Value_Seq

      Marshall (Temp_Buf, PolyORB.Types.Unsigned_Long (Length (C.Policies)));

      --  Marshall Policy_Value_Seq elements

      while not Last (It) loop
         Marshall (Temp_Buf, Value (It).P_Type);
         Marshall (Temp_Buf, Value (It).P_Value.all);

         Next (It);
      end loop;

      Marshall (Buffer, Encapsulate (Temp_Buf));
      Release (Temp_Buf);
   end Marshall_Component_Data;

   -------------------------------
   -- Unmarshall_Component_Data --
   -------------------------------

   overriding procedure Unmarshall_Component_Data
     (C      : access TC_Policies;
      Buffer : access Buffer_Type;
      Error  : out PolyORB.Errors.Error_Container)
   is
      use Policy_Value_Seq;
      use PolyORB.Errors;

      Length : PolyORB.Types.Unsigned_Long;

      Temp_Policy_Value : Policy_Value;

      Tag_Body : aliased Encapsulation := Unmarshall (Buffer);

      Temp_Buf : Buffer_Access := new Buffer_Type;
   begin
      Decapsulate (Tag_Body'Access, Temp_Buf);
      Length := Unmarshall (Temp_Buf);

      for J in 1 .. Length loop
         Temp_Policy_Value.P_Type := Unmarshall (Temp_Buf);
         Temp_Policy_Value.P_Value
           := new Stream_Element_Array'(Unmarshall (Temp_Buf));

         Append (C.Policies, Temp_Policy_Value);
      end loop;

      pragma Assert (Remaining (Temp_Buf) = 0);
      Release (Temp_Buf);

   exception
      when others =>
         Release (Temp_Buf);
         Throw (Error,
                Bad_Param_E,
                System_Exception_Members'(10, Completed_No));
   end Unmarshall_Component_Data;

   ----------------------
   -- Release_Contents --
   ----------------------

   overriding procedure Release_Contents (C : access TC_Policies) is
      procedure Free is
         new PolyORB.Utils.Unchecked_Deallocation.Free

        (Object => Encapsulation,

         Name   => Encapsulation_Access);

      use Policy_Value_Seq;

      It : Policy_Value_Seq.Iterator := First (C.Policies);

   begin
      while not Last (It) loop
         Free (Value (It).P_Value);
         Next (It);
      end loop;

      Deallocate (C.Policies);
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
       Init      => Initialize'Access,
       Shutdown  => null));
end PolyORB.GIOP_P.Tagged_Components.Policies;
