------------------------------------------------------------------------------
--                                                                          --
--                        ADABROKER COMPONENTS                              --
--                                                                          --
--                        A D A B R O K E R . I O P                         --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 1.2 $
--                                                                          --
--         Copyright (C) 1999-2000 ENST Paris University, France.           --
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

--  This package corresponds to the C class IOP defined in file IOP.h. It
--  provides the type Tagged_Profile_List and some methods to marshall and
--  unmarshall it.

with System;
with Interfaces.C;

with AdaBroker; use AdaBroker;
with AdaBroker.NetBufferedStream;
with AdaBroker.MemBufferedStream;

with CORBA;

package AdaBroker.IOP is

   type Tagged_Profile_List is new System.Address;
   --  Corresponds to IOP::TaggedProfileList* (see IOP.h) This object is
   --  never used in Ada (just taken from a C function and given to another
   --  one) so it is not right implemented.  We just keep the system
   --  Address of the object.


   procedure Marshall
     (A : in IOP.Tagged_Profile_List;
      S : in out NetBufferedStream.Object'Class);

   pragma Import
     (CPP, Marshall,
      "marshall__FPt25_CORBA_Unbounded_Sequence" &
      "1ZQ23IOP13TaggedProfileR21Ada_netBufferedStream");
   --  Wrapper around Ada_IOP method marshall (see Ada_IOP.h) Marshalls a
   --  Tagged_Profile_List into a NetBufferedStream

   procedure Unmarshall
     (A : in out IOP.Tagged_Profile_List;
      S : in out NetBufferedStream.Object'Class);

   pragma Import
     (CPP, Unmarshall,
      "unmarshall__FRPt25_CORBA_Unbounded_Sequence" &
      "1ZQ23IOP13TaggedProfileR21Ada_netBufferedStream");
   --  Wrapper around Ada_IOP method marshall (see Ada_IOP.h) Unmarshalls a
   --  Tagged_Profile_List from a NetBufferedStream

   procedure Marshall
     (A : in IOP.Tagged_Profile_List;
      S : in out MemBufferedStream.Object'Class);
   --  Marshalls a Tagged_Profile_List into a MemBufferedStream

   procedure Unmarshall
     (A : in out IOP.Tagged_Profile_List;
      S : in out MemBufferedStream.Object'Class);
   --  Unmarshalls a Tagged_Profile_List from a MemBufferedStream

   function Align_Size
     (A             : in IOP.Tagged_Profile_List;
      Initialoffset : in CORBA.Unsigned_Long)
      return CORBA.Unsigned_Long;
   --  Computes the size needed to marshall a Tagged_Profile_List and add
   --  it to the Initialoffset

   function Length
     (A : in IOP.Tagged_Profile_List)
      return CORBA.Unsigned_Long;
   --  Computes the length of a Tagged_Profile_List

end AdaBroker.IOP;
