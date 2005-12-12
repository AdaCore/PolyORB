------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--          T E S T . C L I E N T I N T E R C E P T O R . I M P L           --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2005 Free Software Foundation, Inc.             --
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

with CORBA.ORB;
with IOP.Codec;
with IOP.CodecFactory.Helper;
with PolyORB.Utils.Report;
with PortableInterceptor.Interceptor;

package body Test.ClientInterceptor.Impl is

   ----------
   -- Is_A --
   ----------

   function Is_A
     (Self            : access Object;
      Logical_Type_Id :        Standard.String)
      return Boolean
   is
      pragma Unreferenced (Self);

   begin
      return CORBA.Is_Equivalent
        (Logical_Type_Id,
         Test.ClientInterceptor.Repository_Id)
        or else CORBA.Is_Equivalent
        (Logical_Type_Id,
         "IDL:omg.org/CORBA/Object:1.0")
        or else CORBA.Is_Equivalent
        (Logical_Type_Id,
         PortableInterceptor.ClientRequestInterceptor.Repository_Id)
        or else CORBA.Is_Equivalent
        (Logical_Type_Id,
         PortableInterceptor.Interceptor.Repository_Id);
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
        IOP.CodecFactory.Create_Codec
        (Factory, (IOP.Encoding_CDR_Encaps, 1, 2));

      PolyORB.Utils.Report.Output
        ("Added tagged component present in IOR",
         CORBA.Unsigned_Long'
          (CORBA.From_Any
           (IOP.Codec.Decode_Value
            (Codec,
             PortableInterceptor.ClientRequestInfo.Get_Effective_Component
             (RI, IOP.Tag_ORB_Type).Component_Data,
             CORBA.TC_Unsigned_Long)))
          = 123456789);

   exception
      when others =>
         PolyORB.Utils.Report.Output
           ("Added tagged component present in IOR", False);
   end Send_Request;

end Test.ClientInterceptor.Impl;
