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
      Registered  : Boolean := False;
      Default     : CORBA.Policy.Ref;
      Factory     : Policy_Factory;
      Client_Side : Boolean;
      Server_Side : Boolean;
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

   ---------------------------
   -- Is_Client_Side_Policy --
   ---------------------------

   function Is_Client_Side_Policy
     (The_Type : in CORBA.PolicyType)
     return Boolean
   is
   begin
      pragma Assert (Policy_Registry (The_Type).Registered);

      return Policy_Registry (The_Type).Client_Side;
   end Is_Client_Side_Policy;

   -------------------
   -- Is_Registered --
   -------------------

   function Is_Registered (The_Type : in CORBA.PolicyType) return Boolean is
   begin
      return Policy_Registry (The_Type).Registered;
   end Is_Registered;

   ---------------------------
   -- Is_Server_Side_Policy --
   ---------------------------

   function Is_Server_Side_Policy
     (The_Type : in CORBA.PolicyType)
     return Boolean
   is
   begin
      pragma Assert (Policy_Registry (The_Type).Registered);

      return Policy_Registry (The_Type).Server_Side;
   end Is_Server_Side_Policy;

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
     (The_Type    : in CORBA.PolicyType;
      Default     : in CORBA.Policy.Ref;
      Factory     : in Policy_Factory;
      Client_Side : in Boolean;
      Server_Side : in Boolean)
   is
   begin
      Policy_Registry (The_Type) :=
        (True,
         Default,
         Factory,
         Client_Side,
         Server_Side);
   end Register;

end PolyORB.CORBA_P.Policy_Management;
