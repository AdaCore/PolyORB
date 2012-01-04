------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                       I O P . C O D E C . I M P L                        --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2004-2012, Free Software Foundation, Inc.          --
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

with CORBA.Local;
with PolyORB.Representations.CDR;

package IOP.Codec.Impl is

   type Object is new CORBA.Local.Object with private;

   type Object_Ptr is access all Object'Class;

   function Encode
     (Self : access Object;
      Data : CORBA.Any)
     return CORBA.IDL_SEQUENCES.OctetSeq;

   function Decode
     (Self : access Object;
      Data : CORBA.IDL_SEQUENCES.OctetSeq)
     return CORBA.Any;

   function Encode_Value
     (Self : access Object;
      Data : CORBA.Any)
     return CORBA.IDL_SEQUENCES.OctetSeq;

   function Decode_Value
     (Self : access Object;
      Data : CORBA.IDL_SEQUENCES.OctetSeq;
      TC   : CORBA.TypeCode.Object)
     return CORBA.Any;

   procedure Init
     (Self           : access Object;
      Representation :
        PolyORB.Representations.CDR.CDR_Representation_Access);
   --  Internal initialization subprogram

private

   type Object is new CORBA.Local.Object with record
      Representation : PolyORB.Representations.CDR.CDR_Representation_Access;
   end record;

   function Is_A
     (Self            : not null access Object;
      Logical_Type_Id : Standard.String) return Boolean;

   --  Derived from PolyORB.Smart_Pointers.Entity

   procedure Finalize (Self : in out Object);

end IOP.Codec.Impl;
