------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--               P O L Y O R B . R E F E R E N C E S . I O R                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                Copyright (C) 2001 Free Software Fundation                --
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
--              PolyORB is maintained by ENST Paris University.             --
--                                                                          --
------------------------------------------------------------------------------

--  $Id$

with Ada.Streams; use Ada.Streams;

with PolyORB.Sequences.Unbounded;

with PolyORB.Binding_Data;
with PolyORB.Buffers; use PolyORB.Buffers;
with PolyORB.Log;
with PolyORB.Representations.CDR;
with PolyORB.Types;
with PolyORB.Utils;

package body PolyORB.References.IOR is

   use PolyORB.Log;
   use PolyORB.Utils;
   use PolyORB.Representations.CDR;
   use PolyORB.Binding_Data;
   use Profile_Record_Seq;

   package L is new PolyORB.Log.Facility_Log ("polyorb.references.ior");
   procedure O (Message : in String; Level : Log_Level := Debug)
     renames L.Output;

   ------------------
   -- Marshall_IOR --
   ------------------

   procedure Marshall_IOR
     (Buffer : access Buffer_Type;
      Value  : in IOR_Type)
   is
      use PolyORB.Types;
      use Profile_Seqs;

   begin
      pragma Debug (O ("Marshall: enter"));

      if Is_Nil (Value) then
         Marshall
           (Buffer, PolyORB.Types.String'(To_PolyORB_String ("")));
         Marshall (Buffer, Types.Unsigned_Long'(0));
      else
         declare
            Profs    : constant Profile_Array
              := Profiles_Of (Value);

            Last_Callback  : Integer := 0;
            Callback_Index : Integer;
            Valid_Callbacks : array (1 .. Length (Callbacks))
              of Profile_Record;
         begin

            for Profile_Index in Profs'Range loop
               pragma Debug
                 (O ("Considering profile with tag"
                     & Get_Profile_Tag
                     (Profs (Profile_Index).all)'Img));

               for I in 1 .. Length (Callbacks) loop
                  pragma Assert (Profs (Profile_Index) /= null);

                  declare
                     T : constant Profile_Tag
                       := Get_Profile_Tag
                       (Profs (Profile_Index).all);

                     Info : constant Profile_Record
                       := Element_Of (Callbacks, I);
                  begin
                     pragma Debug
                       (O ("... with callback" & I'Img
                           & " whose tag is "
                           & Profile_Tag'Image (Info.Tag)));

                     if T = Info.Tag then
                        Last_Callback := Last_Callback + 1;
                        Valid_Callbacks (Last_Callback) := Info;
                     end if;
                  end;
               end loop;
            end loop;

            Marshall
              (Buffer, PolyORB.Types.String'
               (To_PolyORB_String (Type_Id_Of (Value))));
            Marshall (Buffer, Types.Unsigned_Long (Last_Callback));

            Callback_Index := Valid_Callbacks'First;
            for Profile_Index in Profs'Range loop
               exit when Callback_Index > Last_Callback;

               if Get_Profile_Tag (Profs (Profile_Index).all)
                 = Valid_Callbacks (Callback_Index).Tag
               then
                  Marshall
                    (Buffer, Types.Unsigned_Long
                     (Valid_Callbacks (Callback_Index).Tag));

                  Valid_Callbacks
                    (Callback_Index).Marshall_Profile_Body
                    (Buffer, Profs (Profile_Index));
                  Callback_Index := Callback_Index + 1;
               end if;
            end loop;
         end;
      end if;
      pragma Debug (O ("Marshall: Leave"));
   end Marshall_IOR;

   --------------------
   -- Unmarshall_IOR --
   --------------------

   function Unmarshall_IOR
     (Buffer : access Buffer_Type)
   return  IOR_Type
   is
      use PolyORB.Types;
      use Profile_Seqs;

      Result     : IOR_Type;

      PolyORB_Type_Id : constant Types.String
        := Types.String (Types.String'(Unmarshall (Buffer)));
      Type_Id : constant String
        := To_Standard_String (PolyORB_Type_Id);

      N_Profiles : constant Types.Unsigned_Long
        := Unmarshall (Buffer);

      Profs   : Profile_Array := (1 .. Integer (N_Profiles) => null);
      Last_Profile : Integer := Profs'First - 1;
   begin

      pragma Debug
        (O ("Decapsulate_IOR: type " & Type_Id
            & " (" & N_Profiles'Img & " profiles)."));

      All_Profiles :
      for N in 1 .. N_Profiles loop
         declare
            Temp_Tag : constant Types.Unsigned_Long := Unmarshall (Buffer);
            Tag      : constant Profile_Tag := Profile_Tag (Temp_Tag);
            Known    : Boolean := False;
         begin
            pragma Debug (O ("Considering profile with tag"
                             & Profile_Tag'Image (Tag)));

            All_Callbacks :
            for I in 1 .. Length (Callbacks) loop
               pragma Debug
                 (O ("... with callback" & I'Img & " whose tag is "
                     & Profile_Tag'Image (Element_Of (Callbacks, I).Tag)));
               if Element_Of (Callbacks, I).Tag = Tag then
                  Last_Profile := Last_Profile + 1;
                  Profs (Last_Profile) := Element_Of (Callbacks, I).
                    Unmarshall_Profile_Body (Buffer);
                  Known := True;
                  --  Profiles dynamically allocated here
                  --  will be freed when the returned
                  --  reference is finalised.
               end if;
            end loop All_Callbacks;

            if not Known then

               --  No callback matches this tag.

               declare
                  pragma Warnings (Off);
                  Discarded_Body : constant Encapsulation
                    := Unmarshall (Buffer);
                  --  Consider the profile body as an encapsulation
                  --  (our best bet).
                  pragma Unreferenced (Discarded_Body);
                  pragma Warnings (On);
               begin
                  null;
                  --  XXX
                  --  Actually we should keep the tag and encapsulation
                  --  as an 'unsupported profile'.
               end;

            end if;
         end;
      end loop All_Profiles;

      if Last_Profile >= Profs'First then
         Create_Reference
           (Profs (Profs'First .. Last_Profile), Type_Id,
            References.Ref (Result));
      end if;

      return Result;
   end Unmarshall_IOR;

   ----------------------
   -- Object_To_Opaque --
   ----------------------

   function Object_To_Opaque (IOR : IOR_Type)
     return Stream_Element_Array
   is
      Buf : Buffer_Access := new Buffer_Type;
   begin
      Start_Encapsulation (Buf);
      Representations.CDR.Marshall (Buf, IOR);

      declare
         Octets : constant Encapsulation := Encapsulate (Buf);
      begin
         Release (Buf);
         return Stream_Element_Array (Octets);
      end;
   end Object_To_Opaque;

   ----------------------
   -- Opaque_To_Object --
   ----------------------

   function Opaque_To_Object
     (Opaque : access Ada.Streams.Stream_Element_Array)
     return IOR_Type
   is
      Buf : aliased Buffer_Type;
   begin
      Decapsulate (Opaque, Buf'Access);
      return Unmarshall (Buf'Access);
   end Opaque_To_Object;

   ----------------------
   -- Object_To_String --
   ----------------------

   function Object_To_String (IOR : IOR_Type)
     return Types.String is
   begin
      return Types.To_PolyORB_String
        ("IOR:" & To_String (Object_To_Opaque (IOR)));
   end Object_To_String;

   ----------------------
   -- String_To_Object --
   ----------------------

   function String_To_Object (Str : Types.String)
     return IOR_Type
   is
      use PolyORB.Buffers;
      S       : constant String
        := Types.To_Standard_String (Str);
      Length  : constant Natural := S'Length;
   begin
      if Length <= 4
        or else Length mod 2 /= 0
        or else S (S'First .. S'First + 3) /= "IOR:"
      then
         raise Constraint_Error;
      end if;

      declare
         Octets : aliased Stream_Element_Array
           := To_Stream_Element_Array (S (S'First + 4 .. S'Last));
      begin
         return Opaque_To_Object (Octets'Access);
      end;
   end String_To_Object;

   --------------
   -- Register --
   --------------

   procedure Register
     (Profile     : in Profile_Tag;
      Marshall_Profile_Body   : in Marshall_Profile_Body_Type;
      Unmarshall_Profile_Body : in Unmarshall_Profile_Body_Type)
   is
      Elt : constant Profile_Record
        := (Profile, Marshall_Profile_Body,
            Unmarshall_Profile_Body);
   begin
      Append (Callbacks, Elt);
   end Register;

end PolyORB.References.IOR;
