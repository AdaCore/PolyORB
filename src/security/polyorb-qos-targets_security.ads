------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--         P O L Y O R B . Q O S . T A R G E T S _ S E C U R I T Y          --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2005-2012, Free Software Foundation, Inc.          --
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

pragma Ada_2005;

--  Target Side CORBA CSI Version 2 Compound Mechanism Configuration

with PolyORB.Annotations;
with PolyORB.Security.Authentication_Mechanisms;
with PolyORB.Security.Authority_Mechanisms;
with PolyORB.Security.Backward_Trust_Evaluators;
with PolyORB.Security.Credentials;
with PolyORB.Security.Forward_Trust_Evaluators;
with PolyORB.Security.Transport_Mechanisms;
with PolyORB.Security.Types;
with PolyORB.Utils.Chained_Lists;

package PolyORB.QoS.Targets_Security is

   type Target_Mechanism is limited record
      Transport                :
        PolyORB.Security.Transport_Mechanisms.
          Target_Transport_Mechanism_Access;

      --  Authentication layer

      Authentication_Mechanism :
        PolyORB.Security.Authentication_Mechanisms.
          Target_Authentication_Mechanism_Access;
      Authentication_Required  : Boolean;

      --  Attribute layer

      Authorities              :
        PolyORB.Security.Authority_Mechanisms.
          Target_Authority_Mechanism_Lists.List;

      Forward_Trust_Evaluator  :
        PolyORB.Security.Forward_Trust_Evaluators.
          Forward_Trust_Evaluator_Access;
      Backward_Trust_Evaluator :
        PolyORB.Security.Backward_Trust_Evaluators.
          Backward_Trust_Evaluator_Access;
      Naming_Mechanisms        : PolyORB.Security.Types.OID_Lists.List;
      Identity_Types           : PolyORB.Security.Types.Identity_Token_Type;
      Delegation_Required      : Boolean;

      Credentials              : PolyORB.Security.Credentials.Credentials_Ref;

      Notepad                  : PolyORB.Annotations.Notepad;
   end record;

   type Target_Mechanism_Access is access all Target_Mechanism;

   function Is_Protected (Mechanism : Target_Mechanism) return Boolean;

   function Target_Supports
     (Mechanism : Target_Mechanism)
      return PolyORB.Security.Types.Association_Options;

   function Target_Requires
     (Mechanism : Target_Mechanism)
      return PolyORB.Security.Types.Association_Options;

   procedure Set_Accepting_Credentials
     (Mechanism   : in out Target_Mechanism;
      Credentials :        PolyORB.Security.Credentials.Credentials_Ref);

   package Target_Mechanism_Lists is
     new PolyORB.Utils.Chained_Lists (Target_Mechanism_Access);

   type QoS_Target_Security_Parameter is
     new QoS_Parameter (Compound_Security) with
   record
      Stateful            : Boolean;
      Disable_Unprotected : Boolean;
      Mechanisms          : Target_Mechanism_Lists.List;
      --  List of available compound security mechanisms.
   end record;

   type QoS_Target_Security_Parameter_Access is
     access all QoS_Target_Security_Parameter;

   overriding procedure Release_Contents
     (QoS : access QoS_Target_Security_Parameter);

end PolyORB.QoS.Targets_Security;
