------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--          T E S T . C L I E N T I N T E R C E P T O R . I M P L           --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2005-2023, Free Software Foundation, Inc.          --
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

with Ada.Exceptions;
with Ada.Text_IO;

with CONV_FRAME.Helper;
with CORBA.IDL_SEQUENCES;
with CORBA.ORB;
with IOP.Codec;
with IOP.CodecFactory.Helper;
with PortableInterceptor.Interceptor;

with PolyORB.Utils.Report;

package body Test.ClientInterceptor.Impl is

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
          (Logical_Type_Id, Test.ClientInterceptor.Repository_Id)
          or else
        CORBA.Is_Equivalent
          (Logical_Type_Id,
           PortableInterceptor.ClientRequestInterceptor.Repository_Id)
          or else
        CORBA.Is_Equivalent
          (Logical_Type_Id, PortableInterceptor.Interceptor.Repository_Id)
          or else
        CORBA.Is_Equivalent
          (Logical_Type_Id, "IDL:omg.org/CORBA/Object:1.0");
   end Is_A;

   ------------------
   -- Send_Request --
   ------------------

   procedure Send_Request
     (Self : access Object;
      RI   :        PortableInterceptor.ClientRequestInfo.Local_Ref)
   is
      pragma Unreferenced (Self);

      use type CORBA.Unsigned_Long;

      Factory : IOP.CodecFactory.Local_Ref;
      Codec   : IOP.Codec.Local_Ref;

   begin
      Factory :=
        IOP.CodecFactory.Helper.To_Local_Ref
          (CORBA.ORB.Resolve_Initial_References
           (CORBA.ORB.To_CORBA_String ("CodecFactory")));

      Codec :=
        IOP.CodecFactory.create_codec
        (Factory, (IOP.ENCODING_CDR_ENCAPS, 1, 2));

      begin
         PolyORB.Utils.Report.Output
           ("Added tagged component present in IOR",
            CORBA.Unsigned_Long'
             (CORBA.From_Any
              (IOP.Codec.decode_value
               (Codec,
                CORBA.IDL_SEQUENCES.OctetSeq
                (PortableInterceptor.ClientRequestInfo.get_effective_component
                 (RI, IOP.TAG_ORB_TYPE).component_data),
                CORBA.TC_Unsigned_Long)))
            = 123456789);

      exception
         when E : others =>
            PolyORB.Utils.Report.Output
              ("Added tagged component present in IOR", False);
            Ada.Text_IO.Put_Line
              (Ada.Exceptions.Exception_Information (E));
      end;

      declare
         Info : CONV_FRAME.CodeSetComponentInfo;
         pragma Warnings (Off, Info);

      begin
         Info :=
           CONV_FRAME.Helper.From_Any
           (IOP.Codec.decode_value
            (Codec,
             CORBA.IDL_SEQUENCES.OctetSeq
             (PortableInterceptor.ClientRequestInfo.get_effective_component
              (RI, IOP.TAG_CODE_SETS).component_data),
             CONV_FRAME.Helper.TC_CodeSetComponentInfo));

         PolyORB.Utils.Report.Output
           ("TAG_CODE_SETS component present and unmarshalled", True);

      exception
         when E : others =>
            PolyORB.Utils.Report.Output
              ("TAG_CODE_SETS component present and unmarshalled ",
               False);
            Ada.Text_IO.Put_Line
              (Ada.Exceptions.Exception_Information (E));

      end;
   end Send_Request;

end Test.ClientInterceptor.Impl;
