------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--               P O L Y O R B . S E C U R I T Y . T Y P E S                --
--                                                                          --
--                                 S p e c                                  --
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

with Ada.Streams;

with PolyORB.ASN1;
with PolyORB.Types;
with PolyORB.Utils.Chained_Lists;
with PolyORB.Utils.Simple_Flags;

package PolyORB.Security.Types is

   --  Association Options from CORBA CSIv2 and Security Specification

   type Association_Options is new PolyORB.Types.Unsigned_Short;

   No_Protection             : constant Association_Options :=    1;
   Integrity                 : constant Association_Options :=    2;
   Confidentiality           : constant Association_Options :=    4;
   Detect_Replay             : constant Association_Options :=    8;
   Detect_Misordering        : constant Association_Options :=   16;
   Establish_Trust_In_Target : constant Association_Options :=   32;
   Establish_Trust_In_Client : constant Association_Options :=   64;
   No_Delegation             : constant Association_Options :=  128;
   Simple_Delegation         : constant Association_Options :=  256;
   Composite_Delegation      : constant Association_Options :=  512;
   Identity_Assertion        : constant Association_Options := 1024;
   Delegation_By_Client      : constant Association_Options := 2048;

   function Is_Set
     (Flag_To_Test : Association_Options;
      In_Flags     : Association_Options)
      return Boolean;
   --  Test if Flag_To_Test has been set in In_Flags
   --  Flag_To_Test is a mask

   function Set
     (Flag_To_Set : Association_Options;
      In_Flags    : Association_Options)
      return Association_Options;
   --  Set Flag_To_Set in In_Flags
   --  Flag_To_Set is a mask

   --  Identity Token Types from CORBA CSIv2 Specification

   type Identity_Token_Type is new PolyORB.Types.Unsigned_Long;

   ITT_Absent             : constant Identity_Token_Type := 0;
   ITT_Anonymous          : constant Identity_Token_Type := 1;
   ITT_Principal_Name     : constant Identity_Token_Type := 2;
   ITT_X509_Cert_Chain    : constant Identity_Token_Type := 4;
   ITT_Distinguished_Name : constant Identity_Token_Type := 8;

   function Is_Set
     (Flag_To_Test : Identity_Token_Type;
      In_Flags     : Identity_Token_Type)
      return Boolean;

   function Set
     (Flag_To_Set : Identity_Token_Type;
      In_Flags    : Identity_Token_Type)
      return Identity_Token_Type;

   --  ASN.1 OBJECT IDENTIFIER list

   package OID_Lists is
     new PolyORB.Utils.Chained_Lists (PolyORB.ASN1.Object_Identifier,
                                      PolyORB.ASN1."=");

   function Duplicate (Item : OID_Lists.List) return OID_Lists.List;

   --  Access to stream element array. Widely used for represent different
   --  security tokens in encoded form.

   type Stream_Element_Array_Access is
     access all Ada.Streams.Stream_Element_Array;

   --  Security Context Identifier

   type Context_Id is new PolyORB.Types.Unsigned_Long_Long;

   --  OIDs for well known security mechanisms

--  KRB5MechOID                : constant String := "oid:1.2.840.113554.1.2.2";
--  GSS_NT_Export_Name_OID     : constant String := "oid:1.3.6.1.5.6.4";
--  GSS_NT_Scoped_Username_OID : constant String := "oid:2.23.130.1.2.1";
   GSSUPMechOID               : constant String := "oid:2.23.130.1.1.1";

private

   package Association_Options_Flags is
     new PolyORB.Utils.Simple_Flags (Association_Options, Shift_Left);

   function Is_Set
     (Flag_To_Test : Association_Options;
      In_Flags     : Association_Options)
      return Boolean
      renames Association_Options_Flags.Is_Set;

   function Set
     (Flag_To_Set : Association_Options;
      In_Flags    : Association_Options)
      return Association_Options
      renames Association_Options_Flags.Set;

   package Identity_Token_Type_Flags is
     new PolyORB.Utils.Simple_Flags (Identity_Token_Type, Shift_Left);

   function Is_Set
     (Flag_To_Test : Identity_Token_Type;
      In_Flags     : Identity_Token_Type)
      return Boolean
      renames Identity_Token_Type_Flags.Is_Set;

   function Set
     (Flag_To_Set : Identity_Token_Type;
      In_Flags    : Identity_Token_Type)
      return Identity_Token_Type
      renames Identity_Token_Type_Flags.Set;

end PolyORB.Security.Types;
