------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--    P O L Y O R B . C O R B A _ P . P O L I C Y _ M A N A G E M E N T     --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2004 Free Software Foundation, Inc.             --
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

with PolyORB.Exceptions;

package body PolyORB.CORBA_P.Policy_Management is

   type Policy_Info is record
      Registered          : Boolean := False;
      POA_Level           : Boolean;
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
      Policies : in     CORBA.Policy.PolicyList;
      Level    : in     Policy_Override_Level)
   is
      use CORBA.Policy.IDL_Sequence_Policy;

      The_Type : CORBA.PolicyType;
      Defined  : array (Policy_List'Range) of Boolean := (others => False);

   begin
      for J in 1 .. Length (Policies) loop
         The_Type := CORBA.Policy.Get_Policy_Type (Element_Of (Policies, J));

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

         end case;

         To (The_Type) := Element_Of (Policies, J);
      end loop;
   end Add_Policy_Overrides;

   -------------------------
   -- Check_Compatibility --
   -------------------------

   procedure Check_Compatibility
     (Policies : in     Policy_List;
      Indexes  :    out CORBA.Short)
   is
      use type CORBA.Short;

   begin
      Indexes := 0;

      for J in Policies'Range loop
         if not CORBA.Policy.Is_Null (Policies (J))
           and then Policy_Registry (J).Compatibility_Check /= null
         then
            Policy_Registry (J).Compatibility_Check
              (Policies (J),
               Policies,
               CORBA.Unsigned_Short (Indexes));

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
     (The_Type : in CORBA.PolicyType)
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
     (From : in Policy_List;
      TS   : in CORBA.Policy.PolicyTypeSeq)
     return CORBA.Policy.PolicyList
   is
      use CORBA.Policy.IDL_Sequence_Policy;
      use CORBA.Policy.IDL_Sequence_PolicyType;

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
            if not CORBA.Policy.Is_Null (From (Element_Of (TS, J))) then
               Append (Result, From (Element_Of (TS, J)));
            end if;
         end loop;
      end if;

      return Result;
   end Get_Policy_Overrides;

   -------------------
   -- Is_ORB_Policy --
   -------------------

   function Is_ORB_Policy
     (The_Type : in CORBA.PolicyType)
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
     (The_Type : in CORBA.PolicyType)
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
     (The_Type : in CORBA.PolicyType)
     return Boolean
   is
   begin
      pragma Assert (Policy_Registry (The_Type).Registered);

      return Policy_Registry (The_Type).Reference_Level;
   end Is_Reference_Policy;

   -------------------
   -- Is_Registered --
   -------------------

   function Is_Registered (The_Type : in CORBA.PolicyType) return Boolean is
   begin
      return Policy_Registry (The_Type).Registered;
   end Is_Registered;

   ----------------------
   -- Is_Thread_Policy --
   ----------------------

   function Is_Thread_Policy
     (The_Type : in CORBA.PolicyType)
     return Boolean
   is
   begin
      pragma Assert (Policy_Registry (The_Type).Registered);

      return Policy_Registry (The_Type).Thread_Level;
   end Is_Thread_Policy;

   -----------------------
   -- Raise_PolicyError --
   -----------------------

   procedure Raise_PolicyError
     (Members : in CORBA.PolicyError_Members)
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
     (The_Type            : in CORBA.PolicyType;
      POA_Level           : in Boolean                  := False;
      ORB_Level           : in Boolean                  := False;
      Thread_Level        : in Boolean                  := False;
      Reference_Level     : in Boolean                  := False;
      Factory             : in Policy_Factory           := null;
      Compatibility_Check : in Compatibility_Check_Proc := null;
      Reconciliation      : in Reconciliation_Proc      := null;
      System_Default      : in CORBA.Policy.Ref         := Null_Policy)
   is
   begin
      Policy_Registry (The_Type) :=
        (Registered          => True,
         POA_Level           => POA_Level,
         ORB_Level           => ORB_Level,
         Thread_Level        => Thread_Level,
         Reference_Level     => Reference_Level,
         Factory             => Factory,
         Compatibility_Check => Compatibility_Check,
         Reconciliation      => Reconciliation,
         System_Default      => System_Default);
   end Register;

end PolyORB.CORBA_P.Policy_Management;
