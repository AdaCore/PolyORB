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
