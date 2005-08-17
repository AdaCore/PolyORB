------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                P O L Y O R B . M I O P _ P . G R O U P S                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2003-2005 Free Software Foundation, Inc.          --
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
      Oid := new Object_Id'(Object_Id (To_Stream_Element_Array (Buffer)));
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
