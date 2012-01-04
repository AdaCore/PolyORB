------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--             T E S T . I O R I N T E R C E P T O R . I M P L              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2005-2012, Free Software Foundation, Inc.          --
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

with CORBA.ORB;
with IOP.Codec;
with IOP.CodecFactory.Helper;
with PortableInterceptor.Interceptor;

package body Test.IORInterceptor.Impl is

   --------------------------
   -- Establish_Components --
   --------------------------

   procedure Establish_Components
     (Self : access Object;
      Info :        PortableInterceptor.IORInfo.Local_Ref)
   is
      pragma Unreferenced (Self);

      Factory : IOP.CodecFactory.Local_Ref;
      Codec   : IOP.Codec.Local_Ref;
      Version : constant CORBA.Any
        := CORBA.To_Any (CORBA.Unsigned_Long'(123456789));

   begin
      Factory :=
        IOP.CodecFactory.Helper.To_Local_Ref
          (CORBA.ORB.Resolve_Initial_References
           (CORBA.ORB.To_CORBA_String ("CodecFactory")));

      Codec :=
        IOP.CodecFactory.Create_Codec
        (Factory, (IOP.Encoding_CDR_Encaps, 1, 2));

      PortableInterceptor.IORInfo.Add_IOR_Component
        (Info,
         (IOP.Tag_ORB_Type,
          IOP.ComponentData
          (IOP.Codec.Encode_Value (Codec, Version))));
   end Establish_Components;

   ----------
   -- Is_A --
   ----------

   function Is_A
     (Self            : not null access Object;
      Logical_Type_Id : Standard.String) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return
        CORBA.Is_Equivalent
          (Logical_Type_Id, Test.IORInterceptor.Repository_Id)
          or else
        CORBA.Is_Equivalent
          (Logical_Type_Id, PortableInterceptor.IORInterceptor.Repository_Id)
          or else
        CORBA.Is_Equivalent
          (Logical_Type_Id, PortableInterceptor.Interceptor.Repository_Id)
          or else
        CORBA.Is_Equivalent
          (Logical_Type_Id, "IDL:omg.org/CORBA/Object:1.0");
   end Is_A;

end Test.IORInterceptor.Impl;
