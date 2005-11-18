------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--     P O L Y O R B . G I O P _ P . T A G G E D _ C O M P O N E N T S      --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2003-2005 Free Software Foundation, Inc.           --
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
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

--  Implementation of CORBA IOR Tagged components

with Ada.Streams;
with Ada.Unchecked_Deallocation;

with PolyORB.Log;
with PolyORB.Representations.CDR.Common;

package body PolyORB.GIOP_P.Tagged_Components is

   use Ada.Streams;

   use PolyORB.Log;
   use PolyORB.Representations.CDR.Common;

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
   --  Return new empty tagged component with tag Tag

   --------------------------------------------
   -- Create_QoS_GIOP_Tagged_Components_List --
   --------------------------------------------

   function Create_QoS_GIOP_Tagged_Components_List
     (Components : Tagged_Component_List)
      return PolyORB.QoS.Tagged_Components.GIOP_Tagged_Component_Lists.List
   is
      use PolyORB.QoS.Tagged_Components;

      C      : Tagged_Component_Access;
      It     : Iterator := First (Components);
      Result : PolyORB.QoS.Tagged_Components.GIOP_Tagged_Component_Lists.List;

   begin
      while not Last (It) loop
         C := Value (It).all;

         if C.all in TC_Unknown_Component then
            PolyORB.QoS.Tagged_Components.GIOP_Tagged_Component_Lists.Append
              (Result,
               (Tag =>
                  Types.Unsigned_Long
                  (TC_Unknown_Component (C.all).Unknown_Tag),
                Data =>
                  new Ada.Streams.Stream_Element_Array'
                  (TC_Unknown_Component (C.all).Data.all)));

         else
            declare
               Temp_Buf : Buffer_Access := new Buffer_Type;

            begin
               Marshall (C, Temp_Buf);

               PolyORB.QoS.Tagged_Components.GIOP_Tagged_Component_Lists.Append
                 (Result,
                  (Tag => Types.Unsigned_Long (C.Tag),
                   Data =>
                     new Ada.Streams.Stream_Element_Array'
                     (Encapsulate (Temp_Buf))));

               Release (Temp_Buf);
            end;
         end if;

         Next (It);
      end loop;

      return Result;
   end Create_QoS_GIOP_Tagged_Components_List;

   ------------------------------
   -- Create_Unknown_Component --
   ------------------------------

   function Create_Unknown_Component
     (Unknown_Tag : Tag_Value;
      Data        : Octet_Access)
      return Tagged_Component_Access
   is
   begin
      return
        new TC_Unknown_Component'
        (Tag => Tag_Value'Last, Unknown_Tag => Unknown_Tag, Data => Data);
   end Create_Unknown_Component;

   ----------------------
   -- Release_Contents --
   ----------------------

   procedure Release_Contents (List : in out Tagged_Component_List) is
      procedure Free is new Ada.Unchecked_Deallocation
        (Tagged_Component'Class, Tagged_Component_Access);

      Component : Tagged_Component_Access;

   begin
      while List /= Null_Tagged_Component_List loop
         Extract_First (List, Component);
         Release_Contents (Component);
         Free (Component);
      end loop;
   end Release_Contents;

   --------------
   -- Register --
   --------------

   procedure Register
     (Tag : Tag_Value;
      New_Empty_Component : New_Empty_Component_Func_Access;
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
      It : Iterator := First (Components);

   begin
      pragma Debug (O ("Marshall"
                       & Integer'Image (Length (Components))
                       & " component()s"));

      Marshall (Buffer, Types.Unsigned_Long (Length (Components)));

      while not Last (It) loop
         Marshall (Value (It).all, Buffer);
         Next (It);
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

      pragma Debug (O ("Tag not found, return a TC_Unknown_Component"));

      declare
         C : constant Tagged_Component_Access := new TC_Unknown_Component;
      begin
         TC_Unknown_Component (C.all).Unknown_Tag := Tag;
         return C;
      end;
   end Get_New_Empty_Component;

   ---------------------------------
   -- Unmarshall_Tagged_Component --
   ---------------------------------

   function Unmarshall_Tagged_Component
     (Buffer : access Buffer_Type)
     return Tagged_Component_List
   is
      Components : Tagged_Component_List := Null_Tagged_Component_List;
      Length     : Types.Unsigned_Long;

   begin
      Length := Unmarshall (Buffer);
      pragma Debug (O ("Unmarshall" & Types.Unsigned_Long'Image (Length)
                       & " component(s)"));

      for J in 1 .. Length loop
         declare
            use PolyORB.Errors;

            TC : Tagged_Component_Access;
            Tag : Tag_Value;
            Error : Error_Container;
         begin
            Tag := Tag_Value (Types.Unsigned_Long'(Unmarshall (Buffer)));
            TC := Get_New_Empty_Component (Tag);
            Unmarshall (TC, Buffer, Error);

            pragma Assert (not Found (Error));
            --  XXX Should properly propagate the error

            Append (Components, TC);
         end;
      end loop;

      return Components;
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

      It : Iterator := First (List);

   begin
      if Tag = Tag_Value'Last then
         return null;
      end if;

      while not Last (It) loop
         if Value (It).all.Tag = Tag then
            return Value (It).all;
         end if;

         Next (It);
      end loop;

      return null;
   end Get_Component;

   --------------------
   -- Get_Components --
   --------------------

   function Get_Components
     (List : Tagged_Component_List;
      Tag  : Tag_Value)
     return Tagged_Component_Array
   is
      It     : Iterator := First (List);
      Result : Tagged_Component_Array (1 .. Length (List));
      RLast  : Natural := 0;

   begin
      if Tag = Tag_Value'Last then
         return Result (1 .. 0);
      end if;

      while not Last (It) loop
         if Value (It).all.Tag = Tag then
            RLast := RLast + 1;
            Result (RLast) := Value (It).all;
         end if;

         Next (It);
      end loop;

      return Result (1 .. RLast);
   end Get_Components;

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

   procedure Add
     (List : in out Tagged_Component_List;
      CL   :        Tagged_Component_List)
   is
   begin
      List := List & CL;
   end Add;

   -------------
   -- Replace --
   -------------

   procedure Replace
     (List : in out Tagged_Component_List;
      C1   :        Tagged_Component_Access;
      C2   :        Tagged_Component_Access)
   is
   begin
      pragma Assert (C1.Tag = C2.Tag);

      for J in 1 .. Length (List) loop
         if Get_Component (List, Tag_Value (J)) = C1 then
            Remove (List, C1, False);
            Append (List, C2);
         end if;
      end loop;
   end Replace;

   ------------
   -- Remove --
   ------------

   procedure Remove
     (List : in out Tagged_Component_List;
      C    :        Tagged_Component_Access)
   is
   begin
      Remove (List, C, False);
   end Remove;

   ---------------
   -- Deep_Copy --
   ---------------

   function Deep_Copy
     (List : Tagged_Component_List)
     return Tagged_Component_List
   is
      Result : Tagged_Component_List;
      Iter : Iterator := First (List);

   begin
      while not Last (Iter) loop
         Append (Result, Duplicate (Value (Iter).all.all));
         Next (Iter);
      end loop;

      return Result;
   end Deep_Copy;

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
      Buffer : access Buffer_Type;
      Error  : out PolyORB.Errors.Error_Container)
   is
      pragma Unreferenced (Error);

   begin
      pragma Debug (O ("Unmarshall unknown component"));

      C.Data := new Stream_Element_Array'(Unmarshall (Buffer));
   end Unmarshall;

   ---------------
   -- Duplicate --
   ---------------

   function Duplicate
     (C : TC_Unknown_Component)
     return Tagged_Component_Access
   is
      Result : constant Tagged_Component_Access := new TC_Unknown_Component;

   begin
      TC_Unknown_Component (Result.all).Data
        := new Stream_Element_Array'(C.Data.all);

      return Result;
   end Duplicate;

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
