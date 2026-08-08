------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--     P O L Y O R B . G I O P _ P . T A G G E D _ C O M P O N E N T S      --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2003-2017, Free Software Foundation, Inc.          --
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

pragma Ada_2012;

--  Implementation of CORBA IOR Tagged components

with PolyORB.Utils.Unchecked_Deallocation;
with Ada.Tags;

with PolyORB.Log;
with PolyORB.Representations.CDR.Common;

package body PolyORB.GIOP_P.Tagged_Components is

   use Ada.Streams;

   use PolyORB.Log;
   use PolyORB.Representations.CDR.Common;

   package L is
      new PolyORB.Log.Facility_Log ("polyorb.giop_p.tagged_components");
   procedure O (Message : Standard.String; Level : Log_Level := Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
     renames L.Enabled;

   type Bind_Tag is record
      Tag                 : Tag_Value;
      New_Empty_Component : New_Empty_Component_Func_Access;
      Fetch_Component     : Fetch_Component_Func_Access;
   end record;

   Binding_List : array (1 .. 10) of Bind_Tag;
   --  XXX augment array size if there is more components types

   Bind_Index   : Natural := 0;

   function Get_New_Empty_Component
     (Tag : Tag_Value)
     return Tagged_Component_Access;
   --  Return new empty tagged component with tag Tag

   procedure Free is new PolyORB.Utils.Unchecked_Deallocation.Free
     (Object => Tagged_Component'Class,
      Name => Tagged_Component_Access);

   --------------------------------------------
   -- Create_QoS_GIOP_Tagged_Components_List --
   --------------------------------------------

   function Create_QoS_GIOP_Tagged_Components_List
     (Components : Tagged_Component_List)
      return PolyORB.QoS.Tagged_Components.GIOP_Tagged_Component_Lists.List
   is
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
               Marshall_Component_Data (C, Temp_Buf);
               Rewind (Temp_Buf);
               --  XXX Should remove this occurrence of rewind

               PolyORB.QoS.Tagged_Components.GIOP_Tagged_Component_Lists.Append
                 (Result,
                  (Tag => Types.Unsigned_Long (C.Tag),
                   Data =>
                     new Ada.Streams.Stream_Element_Array'
                     (Unmarshall (Temp_Buf))));

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
      TC : constant Tagged_Component_Access := new TC_Unknown_Component;

   begin
      TC_Unknown_Component (TC.all).Unknown_Tag := Unknown_Tag;
      TC_Unknown_Component (TC.all).Data := Data;

      return TC;
   end Create_Unknown_Component;

   ----------------------
   -- Release_Contents --
   ----------------------

   procedure Release_Contents (List : in out Tagged_Component_List) is
      Component : Tagged_Component_Access;

   begin
      while not Is_Empty (List) loop
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
        := Bind_Tag'(Tag                 => Tag,
                     New_Empty_Component => New_Empty_Component,
                     Fetch_Component     => Fetch_Component);
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
      pragma Debug (C, O ("Marshall"
                       & Integer'Image (Length (Components))
                       & " component()s"));

      Marshall (Buffer, Types.Unsigned_Long (Length (Components)));

      while not Last (It) loop
         pragma Debug (C, O (Ada.Tags.External_Tag (Value (It).all'Tag)));

         Marshall_Tagged_Component (Buffer, Value (It).all);
         Next (It);
      end loop;
   end Marshall_Tagged_Component;

   procedure Marshall_Tagged_Component
     (Buffer    : access Buffer_Type;
      Component :        Tagged_Component_Access)
   is
   begin
      if Component.Tag /= Tag_Unknown then
         Marshall (Buffer, Types.Unsigned_Long (Component.Tag));

      else
         Marshall (Buffer,
                   Types.Unsigned_Long
                   (TC_Unknown_Component (Component.all).Unknown_Tag));
      end if;

      Marshall_Component_Data (Component, Buffer);
   end Marshall_Tagged_Component;

   -----------------------------
   -- Get_New_Empty_Component --
   -----------------------------

   function Get_New_Empty_Component
     (Tag : Tag_Value)
     return Tagged_Component_Access
   is
   begin
      pragma Debug (C, O ("Search for tag :" & Tag'Img));

      for J in 1 .. Bind_Index loop
         if Binding_List (J).Tag = Tag then
            return Binding_List (J).New_Empty_Component.all;
         end if;
      end loop;

      pragma Debug (C, O ("Tag not found, return a TC_Unknown_Component"));

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
     (Buffer : access Buffer_Type) return Tagged_Component_List
   is
      Components : Tagged_Component_List := Null_Tagged_Component_List;
      Length     : Types.Unsigned_Long;

   begin
      Length := Unmarshall (Buffer);
      pragma Debug (C, O ("Unmarshall" & Types.Unsigned_Long'Image (Length)
                       & " component(s)"));

      for J in 1 .. Length loop
         declare
            use PolyORB.Errors;

            TC    : Tagged_Component_Access;
            Tag   : Tag_Value;
            Error : Error_Container;
         begin
            Tag := Tag_Value (Types.Unsigned_Long'(Unmarshall (Buffer)));
            TC := Get_New_Empty_Component (Tag);
            Unmarshall_Component_Data (TC, Buffer, Error);

            pragma Assert (not Found (Error));
            --  XXX Should properly propagate the error

            if TC.At_Most_Once then
               declare
                  It : Iterator := First (Components);
               begin
                  while not Last (It) loop
                     if Value (It).all.Tag = TC.Tag then
                        Release_Contents (TC);
                        Free (TC);

                        Throw (Error,
                               Bad_Param_E,
                               System_Exception_Members'(10, Completed_No));
                        --  XXX error to be returned ?
                        return Components;
                     end if;
                     Next (It);
                  end loop;
               end;
            end if;

            Append (Components, TC);
         end;
      end loop;

      return Components;
   end Unmarshall_Tagged_Component;

   procedure Unmarshall_Tagged_Component
     (Buffer : access Buffer_Type;
      C      :    out Tagged_Component_Access;
      Error  :    out PolyORB.Errors.Error_Container)
   is
      Tag : Tag_Value;

   begin
      Tag := Tag_Value (Types.Unsigned_Long'(Unmarshall (Buffer)));
      C := Get_New_Empty_Component (Tag);
      Unmarshall_Component_Data (C, Buffer, Error);
   end Unmarshall_Tagged_Component;

   -------------------
   -- Get_Component --
   -------------------

   function Get_Component
     (List : Tagged_Component_List;
      Tag  : Tag_Value) return Tagged_Component_Access
   is
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
      Tag  : Tag_Value) return Tagged_Component_Array
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
     (Oid : access PolyORB.Objects.Object_Id) return Tagged_Component_List
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
      Comp :        Tagged_Component_Access) is
   begin
      pragma Debug (C, O ("Add component to list with tag :"
                       & PolyORB.Types.Unsigned_Long'Image
                           (PolyORB.Types.Unsigned_Long (Comp.Tag))));

      Append (List, Comp);
   end Add;

   procedure Add
     (List : in out Tagged_Component_List;
      CL   :        Tagged_Component_List)
   is
      It : Iterator := First (CL);
   begin
      while not Last (It) loop
         Append (List, Value (It).all);
         Next (It);
      end loop;
   end Add;

   ------------
   -- Remove --
   ------------

   procedure Remove
     (List : in out Tagged_Component_List;
      Comp : Tagged_Component_Access)
   is
   begin
      Remove_Occurrences (List, Comp, All_Occurrences => False);
   end Remove;

   ---------------
   -- Deep_Copy --
   ---------------

   function Deep_Copy
     (List : Tagged_Component_List) return Tagged_Component_List
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

   -----------------------------
   -- Marshall_Component_Data --
   -----------------------------

   overriding procedure Marshall_Component_Data
     (Comp   : access TC_Unknown_Component;
      Buffer : access Buffer_Type) is
   begin
      pragma Debug (C, O ("Marshall unknown component, tag = "
                       & PolyORB.Types.Unsigned_Long'Image
                       (PolyORB.Types.Unsigned_Long (Comp.Unknown_Tag))));

      Marshall (Buffer, Comp.Data.all);
   end Marshall_Component_Data;

   -------------------------------
   -- Unmarshall_Component_Data --
   -------------------------------

   overriding procedure Unmarshall_Component_Data
     (Comp   : access TC_Unknown_Component;
      Buffer : access Buffer_Type;
      Error  : out PolyORB.Errors.Error_Container)
   is
      pragma Unreferenced (Error);

   begin
      pragma Debug (C, O ("Unmarshall unknown component"));

      Comp.Data := new Stream_Element_Array'(Unmarshall (Buffer));
      pragma Debug (C, O ("done"));
   end Unmarshall_Component_Data;

   ---------------
   -- Duplicate --
   ---------------

   overriding function Duplicate
     (Comp : TC_Unknown_Component) return Tagged_Component_Access
   is
      Result : constant Tagged_Component_Access := new TC_Unknown_Component;

   begin
      TC_Unknown_Component (Result.all).Data
        := new Stream_Element_Array'(Comp.Data.all);
      return Result;
   end Duplicate;

   ----------------------
   -- Release_Contents --
   ----------------------

   procedure Free is new PolyORB.Utils.Unchecked_Deallocation.Free
     (Object => Stream_Element_Array,
      Name => Octet_Access);

   overriding procedure Release_Contents
     (Comp : access TC_Unknown_Component)
   is
   begin
      Free (Comp.Data);
   end Release_Contents;

end PolyORB.GIOP_P.Tagged_Components;
