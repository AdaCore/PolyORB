------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                                  I O P                                   --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2004-2005 Free Software Foundation, Inc.           --
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
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

with CORBA.Sequences.Unbounded;

package IOP is

   --  Implementation Notes: this type temporary replaces CORBA::OctetSeq type.

   package IDL_Sequence_Octet is new CORBA.Sequences.Unbounded (CORBA.Octet);

   --  ProfileId type and constants

   type ProfileId is new CORBA.Unsigned_Long;

   Tag_Internet_IOP        : constant ProfileId := 0;
   Tag_Multiple_Components : constant ProfileId := 1;
   Tag_SCCP_IOP            : constant ProfileId := 2;

   --  TaggedProfile type

   type TaggedProfile is record
      Tag          : ProfileId;
      Profile_Data : IDL_Sequence_Octet.Sequence;
   end record;

   --  IOR type

   package IDL_Sequence_IOP_TaggedProfile is
     new CORBA.Sequences.Unbounded (TaggedProfile);

   type IOR is record
      Type_Id  : CORBA.String;
      Profiles : IDL_Sequence_IOP_TaggedProfile.Sequence;
   end record;

   --  ComponentId type and constants

   type ComponentId is new CORBA.Unsigned_Long;

   Tag_ORB_Type                 : constant ComponentId := 0;
   Tag_Code_Sets                : constant ComponentId := 1;
   Tag_Policies                 : constant ComponentId := 2;
   Tag_Alternate_IIOP_Address   : constant ComponentId := 3;
   Tag_Association_Options      : constant ComponentId := 13;
   Tag_Sec_Name                 : constant ComponentId := 14;
   Tag_SPKM_1_Sec_Mech          : constant ComponentId := 15;
   Tag_SPKM_2_Sec_Mech          : constant ComponentId := 16;
   Tag_KerberosV5_Sec_Mech      : constant ComponentId := 17;
   Tag_CSI_ECMA_Secret_Sec_Mech : constant ComponentId := 18;
   Tag_CSI_ECMA_Hybrid_Sec_Mech : constant ComponentId := 19;
   Tag_SSL_Sec_Trans            : constant ComponentId := 20;
   Tag_CSI_ECMA_Public_Sec_Mech : constant ComponentId := 21;
   Tag_Generic_Sec_Mech         : constant ComponentId := 22;
   Tag_Firewall_Trans           : constant ComponentId := 23;
   Tag_SCCP_Contact_Info        : constant ComponentId := 24;
   Tag_Java_Codebase            : constant ComponentId := 25;
   Tag_Transaction_Policy       : constant ComponentId := 26;
   Tag_Message_Router           : constant ComponentId := 30;
   Tag_OTS_Policy               : constant ComponentId := 31;
   Tag_Inv_Policy               : constant ComponentId := 32;
   Tag_CSI_Sec_Mech_List        : constant ComponentId := 33;
   Tag_Null_Tag                 : constant ComponentId := 34;
   Tag_SECIOP_Sec_Trans         : constant ComponentId := 35;
   Tag_TLS_Sec_Trans            : constant ComponentId := 36;
   Tag_Activity_Policy          : constant ComponentId := 37;
   Tag_Complete_Object_Key      : constant ComponentId := 5;
   Tag_Endpoint_Id_Position     : constant ComponentId := 6;
   Tag_Location_Policy          : constant ComponentId := 12;
   Tag_DCE_String_Binding       : constant ComponentId := 100;
   Tag_DCE_Binding_Name         : constant ComponentId := 101;
   Tag_DCE_No_Pipes             : constant ComponentId := 102;
   Tag_DCE_Sec_Mech             : constant ComponentId := 103;
   Tag_INet_Sec_Trans           : constant ComponentId := 123;

   --  TaggedComponent type

   type TaggedComponent is record
      Tag            : ComponentId;
      Component_Data : IDL_Sequence_Octet.Sequence;
   end record;

   --  TaggedComponentSeq sequence

   package IDL_Sequence_IOP_TaggedComponent is
     new CORBA.Sequences.Unbounded (TaggedComponent);

   type TaggedComponentSeq is new IDL_Sequence_IOP_TaggedComponent.Sequence;

   --  MultipleComponentProfile type

   type MultipleComponentProfile is
     new IDL_Sequence_IOP_TaggedComponent.Sequence;

   --  ServiceId type and constants

   type ServiceId is new CORBA.Unsigned_Long;

   TransactionService       : constant ServiceId := 0;
   CodeSets                 : constant ServiceId := 1;
   ChainBypassCheck         : constant ServiceId := 2;
   ChainBypassInfo          : constant ServiceId := 3;
   LogicalThreadId          : constant ServiceId := 4;
   Bi_Dir_IIOP              : constant ServiceId := 5;
   SendingContextRunTime    : constant ServiceId := 6;
   Invocation_Policies      : constant ServiceId := 7;
   Forwarded_Identity       : constant ServiceId := 8;
   UnknownExceptionInfo     : constant ServiceId := 9;
   RTCorbaPriority          : constant ServiceId := 10;
   RTCorbaPriorityRange     : constant ServiceId := 11;
   FT_Group_Version         : constant ServiceId := 12;
   FT_Request               : constant ServiceId := 13;
   ExceptionDetailMessage   : constant ServiceId := 14;
   SecurityAttributeService : constant ServiceId := 15;
   ActivityService          : constant ServiceId := 16;

   --  ServiceContext type

   type ServiceContext is record
      Context_Id   : ServiceId;
      Context_Data : IDL_Sequence_Octet.Sequence;
   end record;

   --  ServiceContextList sequence

   package IDL_Sequence_IOP_ServiceContext is
     new CORBA.Sequences.Unbounded (ServiceContext);

   type ServiceContextList is new IDL_Sequence_IOP_ServiceContext.Sequence;

   --  EncodingFormat type and constants

   type EncodingFormat is new CORBA.Short;

   Encoding_CDR_Encaps : constant IOP.EncodingFormat := 0;

   --  Encoding type

   type Encoding is record
      Format        : EncodingFormat;
      Major_Version : CORBA.Octet;
      Minor_Version : CORBA.Octet;
   end record;

   --  Repository Ids

   Repository_Id                          : constant Standard.String
     := "IDL:omg.org/IOP:1.0";

   ComponentId_Repository_Id              : constant Standard.String
     := "IDL:omg.org/IOP/ComponentId:1.0";

   Encoding_Repository_Id                 : constant Standard.String
     := "IDL:omg.org/IOP/Encoding:1.0";

   EncodingFormat_Repository_Id           : constant Standard.String
     := "IDL:omg.org/IOP/EncodingFormat:1.0";

   IOR_Repository_Id                      : constant Standard.String
     := "IDL:omg.org/IOP/IOR:1.0";

   MultipleComponentProfile_Repository_Id : constant Standard.String
     := "IDL:omg.org/IOP/MultipleComponentProfile:1.0";

   ProfileId_Repository_Id                : constant Standard.String
     := "IDL:omg.org/IOP/ProfileId:1.0";

   ServiceContext_Repository_Id           : constant Standard.String
     := "IDL:omg.org/IOP/ServiceContext:1.0";

   ServiceContextList_Repository_Id       : constant Standard.String
     := "IDL:omg.org/IOP/ServiceContextList:1.0";

   ServiceId_Repository_Id                : constant Standard.String
     := "IDL:omg.org/IOP/ServiceId:1.0";

   TaggedComponent_Repository_Id          : constant Standard.String
     := "IDL:omg.org/IOP/TaggedComponent:1.0";

   TaggedComponentSeq_Repository_Id       : constant Standard.String
     := "IDL:omg.org/IOP/TaggedComponentSeq:1.0";

   TaggedProfile_Repository_Id            : constant Standard.String
     := "IDL:omg.org/IOP/TaggedProfile:1.0";

end IOP;
