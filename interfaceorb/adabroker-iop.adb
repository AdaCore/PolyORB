------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--                        A D A B R O K E R . I O P                         --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.4 $
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

--  This package corresponds to the C class IOP defined in file IOP.h and
--  wrapped around in ada_Iop.hh. It provides the type Tagged_Profile_List
--  and some methods to marshall and unmarshall it.

package body AdaBroker.IOP is

   ---------------
   -- Marshall2 --
   ---------------

   procedure Marshall2
     (A : in IOP.Tagged_Profile_List;
      S : in out MemBufferedStream.Object'Class);

   pragma Import
     (CPP, Marshall2,
      "marshall__FPt25_CORBA_Unbounded_Sequence1ZQ23IOP13" &
      "TaggedProfileR21Ada_memBufferedStream");
   --  Wrapper around Ada_Iop method marshall (see Ada_Iop.h) name was
   --  changed to avoid conflict

   --------------
   -- Marshall --
   --------------

   procedure Marshall
     (A : in IOP.Tagged_Profile_List;
      S : in out MemBufferedStream.Object'Class) is
   begin
      Marshall2 (A, S);
   end Marshall;

   -----------------
   -- UnMarshall2 --
   -----------------

   procedure UnMarshall2
     (A : in out IOP.Tagged_Profile_List;
      S : in out MemBufferedStream.Object'Class);

   pragma Import
     (CPP, UnMarshall2, "unmarshall__FRPt25_CORBA_Unbounded_Sequence" &
      "1ZQ23IOP13TaggedProfileR21Ada_memBufferedStream");
   --  Wrapper around Ada_Iop method unmarshall (see Ada_Iop.h) name was
   --  changed to avoid conflict

   ----------------
   -- Unmarshall --
   ----------------

   procedure Unmarshall
     (A : in out IOP.Tagged_Profile_List;
      S : in out MemBufferedStream.Object'Class) is
   begin
      UnMarshall2 (A, S);
   end Unmarshall;

   ----------------------
   -- C_NP_AlignedSize --
   ----------------------

   function C_NP_AlignedSize
     (A             : in IOP.Tagged_Profile_List;
      Initialoffset : in Interfaces.C.unsigned_long)
      return Interfaces.C.unsigned_long;

   pragma Import
     (CPP, C_NP_AlignedSize,
      "NP_alignedSize__FPt25_CORBA_Unbounded_Sequence" &
      "1ZQ23IOP13TaggedProfileUi");
   --  Wrapper around Ada_Iop method NP_AlignedSize (see Ada_Iop.h) called
   --  by the Ada equivalent : Align_Size

   ----------------
   -- Align_Size --
   ----------------

   function Align_Size
     (A             : in IOP.Tagged_Profile_List;
      Initialoffset : in CORBA.Unsigned_Long)
      return CORBA.Unsigned_Long
   is
      C_Initialoffset : Interfaces.C.unsigned_long;
      C_Result : Interfaces.C.unsigned_long;
   begin
      --  Transform the arguments in a C type ...
      C_Initialoffset := Interfaces.C.unsigned_long (Initialoffset);

      --  Call  the C function ...
      C_Result := C_NP_AlignedSize (A, C_Initialoffset);

      --  Transform the result in an Ada type
      return  CORBA.Unsigned_Long (C_Result);
   end Align_Size;

   --------------
   -- C_Length --
   --------------

   function C_Length
     (A : in IOP.Tagged_Profile_List)
      return Interfaces.C.unsigned_long;

   pragma Import
     (CPP, C_Length,
      "length__FPt25_CORBA_Unbounded_Sequence" &
      "1ZQ23IOP13TaggedProfile");
   --  Wrapper around Ada_Iop method length (see Ada_Iop.h) called by the
   --  Ada equivalent : Length

   ------------
   -- Length --
   ------------

   function Length
     (A : in IOP.Tagged_Profile_List)
      return CORBA.Unsigned_Long
   is
      C_Result : Interfaces.C.unsigned_long;
   begin
      --  Call the C function ...
      C_Result := C_Length (A);

      --  Transform the result in an Ada type
      return  CORBA.Unsigned_Long (C_Result);
   end Length;

end AdaBroker.IOP;
