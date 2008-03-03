------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--          POLYORB.GIOP_P.TAGGED_COMPONENTS.SSL_SEC_TRANS.PRINT            --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2005-2008, Free Software Foundation, Inc.          --
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

with Ada.Strings.Unbounded;

with Common;
with Output;

package body PolyORB.GIOP_P.Tagged_Components.SSL_Sec_Trans.Print is

   use Ada.Strings.Unbounded;
   use Common;
   use Output;

   function Image (Options : Association_Options) return String;

   function Image (Options : Association_Options) return String is
      Result : Unbounded_String
        := To_Unbounded_String (Association_Options'Image (Options));

   begin
      if Is_Set (No_Protection, Options) then
         Append (Result, " NoProtection");
      end if;

      if Is_Set (Integrity, Options) then
         Append (Result, " Integrity");
      end if;

      if Is_Set (Confidentiality, Options) then
         Append (Result, " Confidentiality");
      end if;

      if Is_Set (Detect_Replay, Options) then
         Append (Result, " DetectReplay");
      end if;

      if Is_Set (Detect_Misordering, Options) then
         Append (Result, " DetectMisordering");
      end if;

      if Is_Set (Establish_Trust_In_Target, Options) then
         Append (Result, " EstablishTrustInTarget");
      end if;

      if Is_Set (Establish_Trust_In_Client, Options) then
         Append (Result, " EstablishTrustInClient");
      end if;

      if Is_Set (No_Delegation, Options) then
         Append (Result, " NoDelegation");
      end if;

      if Is_Set (Simple_Delegation, Options) then
         Append (Result, " SimpleDelegation");
      end if;

      if Is_Set (Composite_Delegation, Options) then
         Append (Result, " CompositeDelegation");
      end if;

      if Is_Set (Identity_Assertion, Options) then
         Append (Result, " IdentityAssertion");
      end if;

      if Is_Set (Delegation_By_Client, Options) then
         Append (Result, " DelegationByClient");
      end if;

      return To_String (Result);
   end Image;

   ---------------
   -- Output_TC --
   ---------------

   procedure Output_TC
     (TC              : TC_SSL_Sec_Trans;
      Primary_Address : Utils.Sockets.Socket_Name)
   is

      procedure Output_Support_State
        (Option : Association_Options;
         Name   : String);

      procedure Output_Support_State
        (Option : Association_Options;
         Name   : String)
      is
      begin
         if not Is_Set (Option, TC.Target_Supports) then
            Put_Line (Name, "Not Supported");

         elsif not Is_Set (Option, TC.Target_Requires) then
            Put_Line (Name, "Supported");

         else
            Put_Line (Name, "Required");
         end if;
      end Output_Support_State;

      Aux : Utils.Sockets.Socket_Name := Primary_Address;

   begin
      Inc_Indent;

      Aux.Port := TC.Port;
      Output_Address_Information (Aux);

      Put_Line ("Target Supports", Image (TC.Target_Supports));
      Put_Line ("Target Requires", Image (TC.Target_Requires));

      Inc_Indent;
      Put_Line ("Target Summary", "");
      Inc_Indent;
      Output_Support_State
        (No_Protection,
         "No Protection            ");
      Output_Support_State
        (Integrity,
         "Integrity                ");
      Output_Support_State
        (Confidentiality,
         "Confidentiality          ");
      Output_Support_State
        (Detect_Replay,
         "Detect Replay            ");
      Output_Support_State
        (Detect_Misordering,
         "Detect Misordering       ");
      Output_Support_State
        (Establish_Trust_In_Target,
         "Establish Trust In Target");
      Output_Support_State
        (Establish_Trust_In_Client,
         "Establish Trust In Client");
      Output_Support_State
        (No_Delegation,
         "No Delegation            ");
      Output_Support_State
        (Simple_Delegation,
         "Simple Delegation        ");
      Output_Support_State
        (Composite_Delegation,
         "Composite Delegation     ");
      Output_Support_State
        (Identity_Assertion,
         "Identity Assertion       ");
      Output_Support_State
        (Delegation_By_Client,
         "Delegation By Client     ");
      Dec_Indent;
      Dec_Indent;
      Dec_Indent;
   end Output_TC;

end PolyORB.GIOP_P.Tagged_Components.SSL_Sec_Trans.Print;
