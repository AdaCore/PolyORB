------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--     P O L Y O R B . G I O P _ P . T A G G E D _ C O M P O N E N T S      --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2003-2004 Free Software Foundation, Inc.           --
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

--  Implementation of CORBA IOR Tagged components

with Ada.Streams;
with Ada.Unchecked_Deallocation;

with PolyORB.Log;
with PolyORB.Representations.CDR;

package body PolyORB.GIOP_P.Tagged_Components is

   use Ada.Streams;

   use PolyORB.Log;
   use PolyORB.Representations.CDR;

   package L is
      new PolyORB.Log.Facility_Log ("polyorb.giop_p.tagged_components");
   procedure O (Message : in Standard.String; Level : Log_Level := Debug)
     renames L.Output;

   type Bind_Tag is record
      Tag  : Tag_Value;
      New_Empty_Component : New_Empty_Component_Func_Access;
      Fetch_Component : Fetch_Component_Func_Access;
   end record;

   Binding_List : array (1 .. 10) of Bind_Tag;
   --  XXX augment array size if there is more components types

   Bind_Index   : Natural := 0;

   function Get_New_Empty_Component
     (Tag : Tag_Value)
     return Tagged_Component_Access;
   --  Return new empty tagged component whith tag Tag

   ----------------------
   -- Release_Contents --
   ----------------------

   procedure Release_Contents
     (List : Tagged_Component_List) is
   begin
      for J in 1 .. Length (List) loop
         declare
            C : constant Tagged_Component_Access := Element_Of (List, J);

         begin
            Release_Contents (C);
         end;
      end loop;
   end Release_Contents;

   --------------
   -- Register --
   --------------

   procedure Register
     (Tag             : Tag_Value;
      New_Empty_Component   : New_Empty_Component_Func_Access;
      Fetch_Component : Fetch_Component_Func_Access)
   is
      use type PolyORB.Types.Unsigned_Long;

   begin
      --  Check if this Tag has already been registered

      for J in 1 .. Bind_Index loop
         if Binding_List (J).Tag = Tag then

            --  FATAL ERROR: This tag has already been registered

            raise Program_Error;
         end if;
      end loop;

      Bind_Index := Bind_Index + 1;

      --  Register tag

      Binding_List (Bind_Index)
        := Bind_Tag'(Tag => Tag,
                     New_Empty_Component => New_Empty_Component,
                     Fetch_Component => Fetch_Component);
   end Register;

   -------------------------------
   -- Marshall_Tagged_Component --
   -------------------------------

   procedure Marshall_Tagged_Component
     (Buffer     : access Buffer_Type;
      Components :        Tagged_Component_List)
   is
      use Component_Seq;

      C : Tagged_Component_Access;

   begin
      pragma Debug (O ("Marshall"
                       & Integer'Image (Length (Components))
                       & " component()s"));

      Marshall (Buffer, Types.Unsigned_Long (Length (Components)));

      for J in 1 .. Length (Components) loop
         C := Element_Of (Components, J);

         if C.all in TC_Unknown_Component then
            Marshall (C, Buffer);

         else
            declare
               Temp_Buf : Buffer_Access := new Buffer_Type;
            begin
               Marshall (Buffer, Types.Unsigned_Long (C.all.Tag));
               Start_Encapsulation (Temp_Buf);
               Marshall (C, Temp_Buf);
               Marshall (Buffer, Encapsulate (Temp_Buf));
               Release_Contents (Temp_Buf.all);
               Release (Temp_Buf);
            end;
         end if;
      end loop;
   end Marshall_Tagged_Component;

   -----------------------------
   -- Get_New_Empty_Component --
   -----------------------------

   function Get_New_Empty_Component
     (Tag : Tag_Value)
     return Tagged_Component_Access
   is
      use type PolyORB.Types.Unsigned_Long;

   begin
      pragma Debug (O ("Search for tag :" & Tag'Img));

      for J in 1 .. Bind_Index loop
         if Binding_List (J).Tag = Tag then
            return Binding_List (J).New_Empty_Component.all;
         end if;
      end loop;

      pragma Debug (O ("Tag not found, return unknown component"));

      return new TC_Unknown_Component;
   end Get_New_Empty_Component;

   ---------------------------------
   -- Unmarshall_Tagged_Component --
   ---------------------------------

   function Unmarshall_Tagged_Component
     (Buffer : access Buffer_Type)
     return Tagged_Component_List
   is
      use type PolyORB.Types.Unsigned_Long;
      use Component_Seq;

      Components : Component_Seq.Sequence := Null_Sequence;
      Len        : Types.Unsigned_Long;
      Tag        : Tag_Value;
      Temp_Tag   : Types.Unsigned_Long;

      Temp_Buf : Buffer_Access := new Buffer_Type;
      C        : Tagged_Component_Access;
   begin
      Len := Unmarshall (Buffer);
      pragma Debug (O ("Unmarshall"
                       & Len'Img
                       & " component(s)"));

      for J in 1 .. Len loop
         Temp_Tag  := Unmarshall (Buffer);
         Tag := Tag_Value (Temp_Tag);
         C := Get_New_Empty_Component (Tag);
         if C.all in TC_Unknown_Component then
            TC_Unknown_Component (C.all).Unknown_Tag := Tag;
            Unmarshall (C, Buffer);
         else
            declare
               Tag_Body   : aliased Encapsulation := Unmarshall (Buffer);
            begin
               Decapsulate (Tag_Body'Access, Temp_Buf);
               Unmarshall (C, Temp_Buf);
               pragma Assert (Remaining (Temp_Buf) = 0);
               Release_Contents (Temp_Buf.all);
            end;
         end if;
         Append (Components, C);
      end loop;

      Release (Temp_Buf);

      return Tagged_Component_List (Components);
   end Unmarshall_Tagged_Component;

   -------------------
   -- Get_Component --
   -------------------

   function Get_Component
     (List : Tagged_Component_List;
      Tag  : Tag_Value)
     return Tagged_Component_Access
   is
      use type PolyORB.Types.Unsigned_Long;
      use Component_Seq;

   begin
      if Tag = Tag_Value'Last then
         return null;
      end if;

      for J in 1 .. Length (List) loop
         if Element_Of (List, J).Tag = Tag then
            return Element_Of (List, J);
         end if;
      end loop;

      return null;
   end Get_Component;

   ----------------------
   -- Fetch_Components --
   ----------------------

   function Fetch_Components
     (Oid : access PolyORB.Objects.Object_Id)
     return Tagged_Component_List
   is
      Result : Tagged_Component_List;
      New_Component : Tagged_Component_Access;

   begin
      for J in 1 .. Bind_Index loop
         if Binding_List (J).Fetch_Component /= null then
            New_Component := Binding_List (J).Fetch_Component.all (Oid);
            if New_Component /= null then
               Append (Result, New_Component);
            end if;
         end if;
      end loop;

      return Result;
   end Fetch_Components;

   ---------
   -- Add --
   ---------

   procedure Add
     (List : in out Tagged_Component_List;
      C    :        Tagged_Component_Access) is
   begin
      pragma Debug (O ("Add component to list with tag :"
                       & PolyORB.Types.Unsigned_Long'Image
                       (PolyORB.Types.Unsigned_Long (C.Tag))));

      Append (List, C);
   end Add;

   -----------------------
   -- Unknown Component --
   -----------------------

   --------------
   -- Marshall --
   --------------

   procedure Marshall
     (C      : access TC_Unknown_Component;
      Buffer : access Buffer_Type) is
   begin
      pragma Debug (O ("Marshall unknown component, tag = "
                       & PolyORB.Types.Unsigned_Long'Image
                       (PolyORB.Types.Unsigned_Long (C.Unknown_Tag))));

      Marshall (Buffer, Types.Unsigned_Long (C.Unknown_Tag));
      Marshall (Buffer, C.Data.all);
   end Marshall;

   ----------------
   -- Unmarshall --
   ----------------

   procedure Unmarshall
     (C      : access TC_Unknown_Component;
      Buffer : access Buffer_Type) is
   begin
      pragma Debug (O ("Unmarshall unknown component"));

      C.Data := new Stream_Element_Array'(Unmarshall (Buffer));
   end Unmarshall;

   ----------------------
   -- Release_Contents --
   ----------------------

   procedure Release_Contents
     (C : access TC_Unknown_Component)
   is
      procedure Free is new
        Ada.Unchecked_Deallocation (Stream_Element_Array, Octet_Access);

   begin
      Free (C.Data);
   end Release_Contents;

end PolyORB.GIOP_P.Tagged_Components;
