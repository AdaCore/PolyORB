------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--             POLYORB.GIOP_P.TAGGED_COMPONENTS.SSL_SEC_TRANS               --
--                                                                          --
--                                 S p e c                                  --
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

with PolyORB.Sockets;
with PolyORB.Utils.Simple_Flags;
pragma Elaborate_All (PolyORB.Utils.Simple_Flags);

package PolyORB.GIOP_P.Tagged_Components.SSL_Sec_Trans is

   --  Association Options from CORBA CSIv2 and Security Specification

   type Association_Options is new Types.Unsigned_Short;

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

   type TC_SSL_Sec_Trans is
     new Tagged_Component (Tag => Tag_SSL_Sec_Trans, At_Most_Once => False)
     with record
        Target_Supports : Association_Options;
        Target_Requires : Association_Options;
        Port            : Sockets.Port_Type;
     end record;
   --  Note: the at-most-once semantics of this component is not
   --  specified in the Security specification, par. 3.7.3, use
   --  default value.

   procedure Marshall_Component_Data
     (C      : access TC_SSL_Sec_Trans;
      Buffer : access Buffer_Type);

   procedure Unmarshall_Component_Data
     (C      : access TC_SSL_Sec_Trans;
      Buffer : access Buffer_Type;
      Error  : out PolyORB.Errors.Error_Container);

   procedure Release_Contents (C : access TC_SSL_Sec_Trans);

   function Duplicate (C : TC_SSL_Sec_Trans) return Tagged_Component_Access;

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

end PolyORB.GIOP_P.Tagged_Components.SSL_Sec_Trans;
