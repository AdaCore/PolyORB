------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--                           B R O C A . I I O P                            --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1999-2000 ENST Paris University, France.          --
--                                                                          --
-- AdaBroker is free software; you  can  redistribute  it and/or modify it  --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. AdaBroker  is distributed  in the hope that it will be  useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with AdaBroker; see file COPYING. If  --
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
--             AdaBroker is maintained by ENST Paris University.            --
--                     (email: broker@inf.enst.fr)                          --
--                                                                          --
------------------------------------------------------------------------------

with Interfaces.C;
with CORBA;

with Broca.Sequences;
with Broca.Buffers;

with Broca.IOP;
pragma Elaborate_All (Broca.IOP);

package Broca.IIOP is

   type Version_Type is
      record
         Major : CORBA.Octet;
         Minor : CORBA.Octet;
      end record;

   IIOP_Version : constant Version_Type
     := (Major => 1, Minor => 0);
   --  The version of IIOP implemented by this ORB.

   type Strand_Type;
   type Strand_Access is access Strand_Type;

   type Strand_List_Type;
   type Strand_List_Access is access Strand_List_Type;

   type Strand_Type is
      record
         Next   : Strand_Access;
         List   : Strand_List_Access;
         Socket : Interfaces.C.int;
      end record;
   --  A strand is a connection with associated data.

   type Strand_List_Type is
      record
         Head : Strand_Access;
         Tail : Strand_Access;
      end record;

   type Profile_IIOP_Type is new IOP.Profile_Type with
      record
         Version : Version_Type := IIOP_Version;
         Host    : CORBA.String;
         Port    : CORBA.Unsigned_Short;
         ObjKey  : Broca.Sequences.Octet_Sequence;
         Strands : Strand_List_Access := null;
      end record;
   --  Most of the fields come from the IOR itself. Socket_Addr is
   --  built from Host and Port. Strands is the list of strands.

   --  FIXME: Rename to Profile_IIOP_Ptr. Make classwide.
   type Profile_IIOP_Access is access all Profile_IIOP_Type;

   function Find_Connection
     (Profile : access Profile_IIOP_Type)
     return IOP.Connection_Ptr;
   --  Find a free connection (or create a new one) to communicate
   --  with an OBJECT via PROFILE.

   function Get_Profile_Tag
     (Profile : Profile_IIOP_Type)
     return IOP.Profile_Tag;

   procedure Create_Profile
     (Buffer : access Broca.Buffers.Buffer_Type;
      Profile : out IOP.Profile_Ptr);

private

   function Get_Object_Key
     (Profile : Profile_IIOP_Type)
     return Broca.Sequences.Octet_Sequence;

end Broca.IIOP;
