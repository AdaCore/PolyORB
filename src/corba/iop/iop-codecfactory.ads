------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                     I O P . C O D E C F A C T O R Y                      --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2004-2006, Free Software Foundation, Inc.          --
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

with Ada.Exceptions;

with CORBA.Object;

with IOP.Codec;

package IOP.CodecFactory is

   type Local_Ref is new CORBA.Object.Ref with null record;

   --  UnknownEncoding exception

   UnknownEncoding : exception;

   type UnknownEncoding_Members is
     new CORBA.IDL_Exception_Members with null record;

   procedure Get_Members
     (From : Ada.Exceptions.Exception_Occurrence;
      To   :    out UnknownEncoding_Members);

   --  CodecFactory API

   function Create_Codec
     (Self : Local_Ref;
      Enc  : Encoding)
     return IOP.Codec.Local_Ref;

   --  Repository Ids

   Repository_Id                 : constant Standard.String
     := "IDL:omg.org/IOP/CodecFactory:1.0";

   Create_Codec_Repository_Id    : constant Standard.String
     := "IDL:omg.org/IOP/CodecFactory/create_codec:1.0";

   UnknownEncoding_Repository_Id : constant Standard.String
     := "IDL:omg.org/IOP/CodecFactory/UnknownEncoding:1.0";

end IOP.CodecFactory;
