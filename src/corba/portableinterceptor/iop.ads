------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                                  I O P                                   --
--                                                                          --
--                                 S p e c                                  --
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

--  $Id$

with CORBA.Sequences.Unbounded;

package IOP is

   --  Implementation Notes: this type temporary replaces CORBA::OctetSeq type.

   package IDL_Sequence_Octet is new CORBA.Sequences.Unbounded (CORBA.Octet);

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

   Repository_Id                : constant Standard.String
     := "IDL:omg.org/IOP:1.0";

   Encoding_Repository_Id       : constant Standard.String
     := "IDL:omg.org/IOP/Encoding:1.0";

   EncodingFormat_Repository_Id : constant Standard.String
     := "IDL:omg.org/IOP/EncodingFormat:1.0";

   ServiceContext_Repository_Id : constant Standard.String
     := "IDL:omg.org/IOP/ServiceContext:1.0";

   ServiceId_Repository_Id      : constant Standard.String
     := "IDL:omg.org/IOP/ServiceId:1.0";

end IOP;
