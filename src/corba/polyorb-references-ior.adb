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

with CORBA;
with PolyORB.Buffers; use PolyORB.Buffers;
with PolyORB.Log;
pragma Elaborate_All (PolyORB.Log);

with PolyORB.Utils;
with PolyORB.Representations.CDR;
with PolyORB.CORBA_P.Exceptions;
with Sequences.Unbounded;
with PolyORB.Binding_Data;
with PolyORB.Types;

package body PolyORB.References.IOR is

   use PolyORB.Log;
   use PolyORB.Utils;
   use PolyORB.Representations.CDR;
   use PolyORB.CORBA_P.Exceptions;
   use PolyORB.Binding_Data;
   use Profile_Record_Seq;

   package L is new PolyORB.Log.Facility_Log ("polyorb.references.ior");
   procedure O (Message : in String; Level : Log_Level := Debug)
     renames L.Output;

   Hexa_Digits : constant array (0 .. 15) of Character
     := "0123456789abcdef";


   --------------
   -- Marshall --
   --------------

   procedure Marshall
     (Buffer : access Buffer_Type;
      Value  : in IOR_Type)
   is
      use PolyORB.Types;
      use Profile_Seqs;
      Profs    : constant Profile_Array := Profiles_Of (Value.Ref);
      Counter  : Integer := 0;

   begin
      Marshall
        (Buffer,
         CORBA.String'(CORBA.To_CORBA_String (Type_Id_Of (Value.Ref))));
      Marshall (Buffer, Types.Unsigned_Long (Length (Callbacks)));

      pragma Debug (O ("Marshall: enter"));

      for N in Profs'Range loop
         for I in 1 .. Length (Callbacks) loop
            pragma Assert (Profs (N) /= null);

            declare
               T : constant Profile_Tag
                 := Get_Profile_Tag (Profs (N).all);
            begin
               if T = Element_Of (Callbacks, I).Tag then
                  Marshall (Buffer, Types.Unsigned_Long (T));

                  Element_Of
                    (Callbacks, I).Marshall_Profile_Body
                    (Buffer, Profs (N));
                  Counter := Counter + 1;
               end if;
            end;
         end loop;
      end loop;

      pragma Debug (O ("Marshall Profile : Leave"));
   end Marshall;

   ----------------
   -- Unmarshall --
   ----------------

   function Unmarshall
     (Buffer : access Buffer_Type)
   return  IOR_Type
   is
      use PolyORB.Types;
      use CORBA;
      use Profile_Seqs;

      Result     : IOR_Type;

      CORBA_Type_Id : constant CORBA.String
        := CORBA.String (Types.String'(Unmarshall (Buffer)));
      Type_Id : constant String
        := CORBA.To_Standard_String (CORBA_Type_Id);

      N_Profiles : constant Types.Unsigned_Long
        := Unmarshall (Buffer);

      Profs   : Profile_Array := (1 .. Integer (N_Profiles) => null);
   begin

      pragma Debug
        (O ("Decapsulate_IOR: type " & Type_Id
            & " (" & N_Profiles'Img & " profiles)."));

      for N in Profs'Range loop
         declare
            Temp_Tag : Types.Unsigned_Long := Unmarshall (Buffer);
            Tag      : constant Profile_Tag := Profile_Tag (Temp_Tag);
         begin

            for I in 1 .. Length (Callbacks) loop
               if Element_Of (Callbacks, I).Tag = Tag then
                  Profs (N) := Element_Of (Callbacks, I).
                    Unmarshall_Profile_Body (Buffer);
                  --  Profiles dynamically allocated here
                  --  will be freed when the returned
                  --  reference is finalised.
               end if;
            end loop;
         end;
      end loop;

      Create_Reference (Profs, Type_Id, Result.Ref);

      return Result;
   end Unmarshall;


   -------------------------------------------------------
   --  Stringfiecation of an IOR: from an Object to String
   -------------------------------------------------------

   function Object_To_String
     (IOR : IOR_Type)
     return CORBA.String
   is
      use CORBA;
      Buf : Buffer_Access := new Buffer_Type;

   begin

      pragma Debug (O ("Object to string : Enter"));

      Start_Encapsulation (Buf);
      Marshall (Buf, IOR);

      declare
            Octets : constant Encapsulation := Encapsulate (Buf);
            S      : constant String :=  "IOR:"
              & To_String (Stream_Element_Array (Octets));
      begin
         pragma Debug (O ("Object to string : Leave"));
         Release (Buf);
         return CORBA.To_CORBA_String (S);
      end;

   end Object_To_String;



   -----------------------------------------------------------
   --  Destringfiecation of an IOR: from an String to an Object
   -----------------------------------------------------------

   function String_To_Object
     (Str : CORBA.String)
      return IOR_Type
   is
      use CORBA;
      use PolyORB.Buffers;
      Buf     : Buffer_Access := new Buffer_Type;
      IOR     : IOR_Type;
      S       : constant String
        := CORBA.To_Standard_String (Str);
      Length  : constant Natural := S'Length;

   begin

      if Length <= 4
        or else Length mod 2 /= 0
        or else S (S'First .. S'First + 3) /= "IOR:" then
         CORBA_P.Exceptions.Raise_Bad_Param;
      end if;

      declare
         Octets : aliased Encapsulation
           := Encapsulation
           (To_Stream_Element_Array (S (S'First + 4 .. S'Last)));
      begin
         Decapsulate (Octets'Access, Buf);
         IOR := Unmarshall (Buf);
         return IOR;
      end;

   end String_To_Object;


   --------------
   -- Register --
   --------------


   procedure Register
     (Profile     : in Profile_Tag;
      Marshall_Profile_Body   : in Marshall_Profile_Body_Type;
      Unmarshall_Profile_Body : in Unmarshall_Profile_Body_Type) is

      Elt : constant Profile_Record := (Profile, Marshall_Profile_Body,
                                        Unmarshall_Profile_Body);
   begin
      Append (Callbacks, Elt);
   end Register;


end PolyORB.References.IOR;
