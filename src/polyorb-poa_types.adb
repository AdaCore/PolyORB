------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                    P O L Y O R B . P O A _ T Y P E S                     --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                Copyright (C) 2001 Free Software Fundation                --
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
--              PolyORB is maintained by ENST Paris University.             --
--                                                                          --
------------------------------------------------------------------------------

--  $Id$

with PolyORB.Representations.CDR; use PolyORB.Representations.CDR;
with PolyORB.Buffers;             use PolyORB.Buffers;
with PolyORB.Types;
with Ada.Streams;

package body PolyORB.POA_Types is

   use Ada.Streams;
   use CORBA;

   ----------
   -- Left --
   ----------

   function "="
     (Left, Right : in Unmarshalled_Oid)
     return Standard.Boolean
   is
   begin
      if Left.Id = Right.Id
        and then Left.System_Generated = Right.System_Generated
        and then Left.Persistency_Flag = Right.Persistency_Flag
      then
         return True;
      end if;
      return False;
   end "=";

   -----------
   -- Image --
   -----------

   function Image
     (Oid : Object_Id)
     return Types.String
   is
   begin
      return To_PolyORB_String
        (PolyORB.Objects.To_String (PolyORB.Objects.Object_Id (Oid)));
   end Image;

   ---------------
   -- Create_Id --
   ---------------

   function Create_Id
     (Name             : in Types.String;
      System_Generated : in CORBA.Boolean;
      Persistency_Flag : in Time_Stamp;
      Creator          : in Types.String)
     return Unmarshalled_Oid_Access
   is
      U_Oid : Unmarshalled_Oid_Access;
   begin
      U_Oid := new Unmarshalled_Oid'(Name,
                                     System_Generated,
                                     Persistency_Flag,
                                     Creator);
      return U_Oid;
   end Create_Id;

   ---------------
   -- Create_Id --
   ---------------

   function Create_Id
     (Name             : in Types.String;
      System_Generated : in CORBA.Boolean;
      Persistency_Flag : in Time_Stamp;
      Creator          : in Types.String)
     return Object_Id_Access
   is
      U_Oid : Unmarshalled_Oid_Access;
   begin
      U_Oid := Create_Id (Name, System_Generated, Persistency_Flag, Creator);
      return U_Oid_To_Oid (U_Oid);
   end Create_Id;

   ------------------
   -- Oid_To_U_Oid --
   ------------------

   function Oid_To_U_Oid
     (Oid : Object_Id_Access)
     return Unmarshalled_Oid_Access
   is
      U_Oid            : Unmarshalled_Oid_Access;
      Stream           : aliased Stream_Element_Array
        := Stream_Element_Array (Oid.all);
      Buffer           : aliased Buffer_Type;

      Id               : PolyORB.Types.String;
      System_Generated : PolyORB.Types.Boolean;
      Persistency_Flag : PolyORB.Types.Unsigned_Long;
      Creator          : PolyORB.Types.String;
   begin

      Decapsulate (Stream'Access, Buffer'Access);
      Id               := Unmarshall (Buffer'Access);
      System_Generated := Unmarshall (Buffer'Access);
      Persistency_Flag := Unmarshall (Buffer'Access);
      Creator          := Unmarshall (Buffer'Access);
      Release_Contents (Buffer);

      U_Oid := new Unmarshalled_Oid'
        (Id,
         CORBA.Boolean (System_Generated),
         Time_Stamp (Persistency_Flag),
         Creator);
      return U_Oid;
   end Oid_To_U_Oid;

   ------------------
   -- Oid_To_U_Oid --
   ------------------

   function Oid_To_U_Oid
     (Oid : Object_Id)
     return Unmarshalled_Oid_Access
   is
      Oid_Access : Object_Id_Access;
      U_Oid      : Unmarshalled_Oid_Access;
   begin
      Oid_Access := new Object_Id'(Oid);
      --  Oid_Access.all := Oid;
      U_Oid := Oid_To_U_Oid (Oid_Access);
      Free (Oid_Access);
      return U_Oid;
      --  ??? Does this work? Not tested yet.
   end Oid_To_U_Oid;

   ------------------
   -- U_Oid_To_Oid --
   ------------------

   function U_Oid_To_Oid
     (U_Oid : Unmarshalled_Oid_Access)
     return Object_Id_Access
   is
      Buffer             : Buffer_Access := new Buffer_Type;
      Oid                : Object_Id_Access;
   begin

      Start_Encapsulation (Buffer);
      Marshall (Buffer, PolyORB.Types.String (U_Oid.Id));
      Marshall (Buffer, PolyORB.Types.Boolean (U_Oid.System_Generated));
      Marshall (Buffer, PolyORB.Types.Unsigned_Long (U_Oid.Persistency_Flag));
      Marshall (Buffer, PolyORB.Types.String (U_Oid.Creator));

      Oid := new Object_Id'(Object_Id (Encapsulate (Buffer)));
      Release (Buffer);
      return Oid;
   end U_Oid_To_Oid;

end PolyORB.POA_Types;
