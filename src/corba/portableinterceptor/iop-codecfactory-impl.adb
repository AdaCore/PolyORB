------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                I O P . C O D E C F A C T O R Y . I M P L                 --
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

with CORBA.Impl;
with PolyORB.Representations.CDR;
with PolyORB.Types;

with IOP.Codec.Impl;
with IOP.CodecFactory.Helper;

package body IOP.CodecFactory.Impl is

   use PolyORB.Representations.CDR;

   ------------------
   -- Create_Codec --
   ------------------

   function Create_Codec
     (Self : access Object;
      Enc  : in     Encoding)
     return IOP.Codec.Local_Ref
   is
      pragma Unreferenced (Self);

      Representation : CDR_Representation_Access;
      Ptr            : IOP.Codec.Impl.Object_Ptr;
      Result         : IOP.Codec.Local_Ref;

   begin
      case Enc.Format is
         when Encoding_CDR_Encaps =>
            Representation :=
              Create_Representation
                (PolyORB.Types.Octet (Enc.Major_Version),
                 PolyORB.Types.Octet (Enc.Minor_Version));

            if Representation /= null then
               Ptr := new IOP.Codec.Impl.Object;
               IOP.Codec.Impl.Init (Ptr, Representation);
               IOP.Codec.Set (Result, CORBA.Impl.Object_Ptr (Ptr));
               return Result;
            end if;

         when others =>
            null;
      end case;

      Helper.Raise_UnknownEncoding_From_Any
        (CORBA.Internals.To_PolyORB_Any (Helper.To_Any ((null record))));
   end Create_Codec;

   ----------
   -- Is_A --
   ----------

   function Is_A
     (Self            : access Object;
      Logical_Type_Id : in     Standard.String)
     return Boolean
   is
      pragma Unreferenced (Self);

   begin
      return CORBA.Is_Equivalent
        (Logical_Type_Id,
         IOP.CodecFactory.Repository_Id)
        or else CORBA.Is_Equivalent
          (Logical_Type_Id,
           "IDL:omg.org/CORBA/Object:1.0");
   end Is_A;

end IOP.CodecFactory.Impl;
