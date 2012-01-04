------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                P O L Y O R B . M I O P _ P . G R O U P S                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2003-2012, Free Software Foundation, Inc.          --
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

with PolyORB.Buffers;
with PolyORB.Representations.CDR.Common;

package body PolyORB.MIOP_P.Groups is

   use PolyORB.Types;

   -----------
   -- Image --
   -----------

   function Image
     (G_I : Group_Info)
     return String is
   begin
      return
        "Domain Id : "
        & To_Standard_String (G_I.Group_Domain_Id)
        & ", Object Group Id :"
        & G_I.Object_Group_Id'Img
        & ", Object Group Ref Version :"
        & G_I.Object_Group_Ref_Version'Img;
   end Image;

   ------------------
   -- To_Object_Id --
   ------------------

   function To_Object_Id
     (G_I : Group_Info)
     return PolyORB.Objects.Object_Id_Access
   is
      use PolyORB.Buffers;
      use PolyORB.Objects;
      use PolyORB.Representations.CDR.Common;

      Buffer : Buffer_Access := new Buffer_Type;
      Oid : PolyORB.Objects.Object_Id_Access;
   begin
      Marshall (Buffer, G_I.Object_Group_Id);
      Marshall (Buffer, G_I.Object_Group_Ref_Version);
      Marshall (Buffer, Types.Identifier (G_I.Group_Domain_Id));
      Oid := new Object_Id'(Object_Id (To_Stream_Element_Array (Buffer.all)));
      Release (Buffer);
      return Oid;
   end To_Object_Id;

   -------------------
   -- To_Group_Info --
   -------------------

   function To_Group_Info
     (Oid : PolyORB.Objects.Object_Id_Access)
     return Group_Info
   is
      use PolyORB.Buffers;
      use PolyORB.Representations.CDR.Common;

      Buffer : Buffer_Access := new Buffer_Type;
      G_I : Group_Info;
   begin
      Initialize_Buffer
        (Buffer,
         Oid'Last,
         Oid (Oid'First)'Address,
         Host_Order,
         0);
      G_I.Object_Group_Id := Unmarshall (Buffer);
      G_I.Object_Group_Ref_Version := Unmarshall (Buffer);
      G_I.Group_Domain_Id :=
        Types.String (Types.Identifier'(Unmarshall (Buffer)));
      Release (Buffer);
      return G_I;
   end To_Group_Info;

end PolyORB.MIOP_P.Groups;
