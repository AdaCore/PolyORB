------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--    P O L Y O R B . C O R B A _ P . P O L I C Y _ M A N A G E M E N T     --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2004-2012, Free Software Foundation, Inc.          --
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

with PolyORB.Exceptions;

package body PolyORB.CORBA_P.Policy_Management is

   type Policy_Info is record
      Registered          : Boolean := False;
      POA_Level           : Boolean;
      Domain_Level        : Boolean;
      ORB_Level           : Boolean;
      Thread_Level        : Boolean;
      Reference_Level     : Boolean;
      Factory             : Policy_Factory;
      Compatibility_Check : Compatibility_Check_Proc;
      Reconciliation      : Reconciliation_Proc;
      System_Default      : CORBA.Policy.Ref;
   end record;

   Policy_Registry : array (CORBA.PolicyType range 1 .. 60) of Policy_Info;

   --------------------------
   -- Add_Policy_Overrides --
   --------------------------

   procedure Add_Policy_Overrides
     (To       : in out Policy_List;
      Policies : CORBA.Policy.PolicyList;
      Level    : Policy_Override_Level)
   is
      use CORBA.Policy.IDL_SEQUENCE_Policy;

      The_Type : CORBA.PolicyType;
      Defined  : array (Policy_List'Range) of Boolean := (others => False);

   begin
      for J in 1 .. Length (Policies) loop
         The_Type := CORBA.Policy.Get_Policy_Type (Get_Element (Policies, J));

         if Defined (The_Type) then
            CORBA.Raise_Bad_Param
              (CORBA.System_Exception_Members'
               (Minor     => 30,
                Completed => CORBA.Completed_No));
         else
            Defined (The_Type) := True;
         end if;

         case Level is
            when POA_Level =>
               if not Is_POA_Policy (The_Type) then
                  CORBA.Raise_No_Permission (CORBA.Default_Sys_Member);
               end if;

            when ORB_Level =>
               if not Is_ORB_Policy (The_Type) then
                  CORBA.Raise_No_Permission (CORBA.Default_Sys_Member);
               end if;

            when Thread_Level =>
               if not Is_Thread_Policy (The_Type) then
                  CORBA.Raise_No_Permission (CORBA.Default_Sys_Member);
               end if;

            when Reference_Level =>
               if not Is_Reference_Policy (The_Type) then
                  CORBA.Raise_No_Permission (CORBA.Default_Sys_Member);
               end if;

            when Domain_Level =>
               raise Program_Error;

         end case;

         To (The_Type) := Get_Element (Policies, J);
      end loop;
   end Add_Policy_Overrides;

   -------------------------
   -- Check_Compatibility --
   -------------------------

   procedure Check_Compatibility
     (Policies : Policy_List;
      Indexes  :    out CORBA.Unsigned_Short)
   is
      use type CORBA.Unsigned_Short;

   begin
      Indexes := 0;

      for J in Policies'Range loop
         if not CORBA.Policy.Is_Null (Policies (J))
           and then Policy_Registry (J).Compatibility_Check /= null
         then
            Policy_Registry (J).Compatibility_Check
              (Policies (J),
               Policies,
               Indexes);

            if Indexes /= 0 then
               return;
            end if;
         end if;
      end loop;
   end Check_Compatibility;

   ------------------------
   -- Get_Policy_Factory --
   ------------------------

   function Get_Policy_Factory
     (The_Type : CORBA.PolicyType)
     return Policy_Factory
   is
   begin
      pragma Assert (Policy_Registry (The_Type).Registered);

      return Policy_Registry (The_Type).Factory;
   end Get_Policy_Factory;

   --------------------------
   -- Get_Policy_Overrides --
   --------------------------

   function Get_Policy_Overrides
     (From : Policy_List;
      TS   : CORBA.Policy.PolicyTypeSeq)
     return CORBA.Policy.PolicyList
   is
      use CORBA.Policy.IDL_SEQUENCE_Policy;
      use CORBA.Policy.IDL_SEQUENCE_PolicyType;

      Result : CORBA.Policy.PolicyList;

   begin
      if Length (TS) = 0 then
         for J in From'Range loop
            if not CORBA.Policy.Is_Null (From (J)) then
               Append (Result, From (J));
            end if;
         end loop;

      else
         for J in 1 .. Length (TS) loop
            if not CORBA.Policy.Is_Null (From (Get_Element (TS, J))) then
               Append (Result, From (Get_Element (TS, J)));
            end if;
         end loop;
      end if;

      return Result;
   end Get_Policy_Overrides;

   ----------------------
   -- Is_Domain_Policy --
   ----------------------

   function Is_Domain_Policy
     (The_Type : CORBA.PolicyType)
     return Boolean
   is
   begin
      pragma Assert (Policy_Registry (The_Type).Registered);

      return Policy_Registry (The_Type).Domain_Level;
   end Is_Domain_Policy;

   -------------------
   -- Is_ORB_Policy --
   -------------------

   function Is_ORB_Policy
     (The_Type : CORBA.PolicyType)
     return Boolean
   is
   begin
      pragma Assert (Policy_Registry (The_Type).Registered);

      return Policy_Registry (The_Type).ORB_Level;
   end Is_ORB_Policy;

   -------------------
   -- Is_POA_Policy --
   -------------------

   function Is_POA_Policy
     (The_Type : CORBA.PolicyType)
     return Boolean
   is
   begin
      pragma Assert (Policy_Registry (The_Type).Registered);

      return Policy_Registry (The_Type).POA_Level;
   end Is_POA_Policy;

   -------------------------
   -- Is_Reference_Policy --
   -------------------------

   function Is_Reference_Policy
     (The_Type : CORBA.PolicyType)
     return Boolean
   is
   begin
      pragma Assert (Policy_Registry (The_Type).Registered);

      return Policy_Registry (The_Type).Reference_Level;
   end Is_Reference_Policy;

   -------------------
   -- Is_Registered --
   -------------------

   function Is_Registered (The_Type : CORBA.PolicyType) return Boolean is
   begin
      return Policy_Registry (The_Type).Registered;
   end Is_Registered;

   ----------------------
   -- Is_Thread_Policy --
   ----------------------

   function Is_Thread_Policy
     (The_Type : CORBA.PolicyType)
     return Boolean
   is
   begin
      pragma Assert (Policy_Registry (The_Type).Registered);

      return Policy_Registry (The_Type).Thread_Level;
   end Is_Thread_Policy;

   ---------------------------------
   -- Policy_System_Default_Value --
   ---------------------------------

   function Policy_System_Default_Value
     (The_Type : CORBA.PolicyType)
     return CORBA.Policy.Ref
   is
   begin
      pragma Assert (Policy_Registry (The_Type).Registered);

      return Policy_Registry (The_Type).System_Default;
   end Policy_System_Default_Value;

   -----------------------
   -- Raise_PolicyError --
   -----------------------

   procedure Raise_PolicyError
     (Members : CORBA.PolicyError_Members)
   is
   begin
      PolyORB.Exceptions.User_Raise_Exception
        (CORBA.PolicyError'Identity,
         Members);
   end Raise_PolicyError;

   --------------
   -- Register --
   --------------

   procedure Register
     (The_Type            : CORBA.PolicyType;
      POA_Level           : Boolean                  := False;
      ORB_Level           : Boolean                  := False;
      Thread_Level        : Boolean                  := False;
      Reference_Level     : Boolean                  := False;
      Domain_Level        : Boolean                  := False;
      Factory             : Policy_Factory           := null;
      Compatibility_Check : Compatibility_Check_Proc := null;
      Reconciliation      : Reconciliation_Proc      := null;
      System_Default      : CORBA.Policy.Ref         := Null_Policy)
   is
   begin
      Policy_Registry (The_Type) :=
        (Registered          => True,
         POA_Level           => POA_Level,
         Domain_Level        => Domain_Level,
         ORB_Level           => ORB_Level,
         Thread_Level        => Thread_Level,
         Reference_Level     => Reference_Level,
         Factory             => Factory,
         Compatibility_Check => Compatibility_Check,
         Reconciliation      => Reconciliation,
         System_Default      => System_Default);
   end Register;

end PolyORB.CORBA_P.Policy_Management;
