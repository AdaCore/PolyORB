------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                    M O M A . D E S T I N A T I O N S                     --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2002-2004 Free Software Foundation, Inc.           --
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

--  $Id$

with MOMA.Types;

with PolyORB.Any.ObjRef;
with PolyORB.Initialization;
pragma Elaborate_All (PolyORB.Initialization); --  WAG:3.15

with PolyORB.References;
with PolyORB.Types;
with PolyORB.Utils.Strings;

package body MOMA.Destinations is

   use MOMA.Types;

   use PolyORB.Any.ObjRef;
   use PolyORB.Types;

   procedure Set_Kind
     (Self : in out Destination;
      Kind :        MOMA.Types.Destination_Type);
   pragma Inline (Set_Kind);


   ---------
   -- "=" --
   ---------

   function "=" (Dest1 : Destination; Dest2 : Destination)
                return Boolean is
   begin
      return Get_Name (Dest1) = Get_Name (Dest2);
   end "=";

   ------------------------
   -- Create_Destination --
   ------------------------

   function Create_Destination
     (Name    : MOMA.Types.String;
      Ref     : MOMA.Types.Ref;
      Kind    : MOMA.Types.Destination_Type := MOMA.Types.Unknown)
     return Destination
   is
      Dest : MOMA.Destinations.Destination;
   begin
      Set_Name (Dest, Name);
      Set_Ref  (Dest, Ref);
      Set_Kind (Dest, Kind);

      return Dest;
   end Create_Destination;

   function Create_Destination
     return Destination is
   begin
      return Create_Destination (To_MOMA_String ("null"),
                                 MOMA.Types.Nil_Ref,
                                 MOMA.Types.Unknown);
   end Create_Destination;

   ----------------------
   -- Create_Temporary --
   ----------------------

   function Create_Temporary
     return Destination is
   begin
      raise PolyORB.Not_Implemented;
      pragma Warnings (Off);
      return Create_Temporary;
      pragma Warnings (On);
   end Create_Temporary;

   ------------
   -- Delete --
   ------------

   procedure Delete is
   begin
      null;
   end Delete;

   --------------
   -- From_Any --
   --------------

   function From_Any
     (Self : PolyORB.Any.Any)
     return MOMA.Destinations.Destination
   is
      Kind     : MOMA.Types.Destination_Type := MOMA.Types.Unknown;
      Name     : MOMA.Types.String;
      Ref      : MOMA.Types.Ref;
   begin
      Name := From_Any
        (PolyORB.Any.Get_Aggregate_Element
         (Self,
          PolyORB.Any.TypeCode.TC_String,
          PolyORB.Types.Unsigned_Long (0)));

      Ref := From_Any
        (PolyORB.Any.Get_Aggregate_Element
         (Self,
          PolyORB.Any.TypeCode.TC_Object,
          PolyORB.Types.Unsigned_Long (1)));

      Kind := MOMA.Types.From_Any
        (PolyORB.Any.Get_Aggregate_Element
         (Self,
          MOMA.Types.TC_Destination_Type,
          PolyORB.Types.Unsigned_Long (2)));

      return Create_Destination (Name, Ref, Kind);
   end From_Any;

   --------------
   -- Get_Kind --
   --------------

   function Get_Kind
     (Self : Destination)
     return MOMA.Types.Destination_Type is
   begin
      return Self.Kind;
   end Get_Kind;

   --------------
   -- Get_Name --
   --------------

   function Get_Name
     (Self : Destination)
     return MOMA.Types.String is
   begin
      return Self.Name;
   end Get_Name;

   -------------
   -- Get_Ref --
   -------------

   function Get_Ref
     (Self : Destination)
     return MOMA.Types.Ref is
   begin
      return Self.Ref;
   end Get_Ref;

   -----------
   -- Image --
   -----------

   function Image
     (Self : Destination)
     return String is
   begin
      return "<name: " & MOMA.Types.To_Standard_String (Self.Name)
        & ",kind: " & MOMA.Types.Destination_Type'Image (Self.Kind)
        & ",ref: " & PolyORB.References.Image (Self.Ref)
        & ">";
   end Image;

   --------------
   -- Set_Name --
   --------------

   procedure Set_Name
     (Self : in out Destination;
      Name :        MOMA.Types.String) is
   begin
      Self.Name := Name;
   end Set_Name;

   -------------
   -- Set_Ref --
   -------------

   procedure Set_Ref
     (Self : in out Destination;
      Ref  :        MOMA.Types.Ref) is
   begin
      Self.Ref := Ref;
   end Set_Ref;

   --------------
   -- Set_Kind --
   --------------

   procedure Set_Kind
     (Self  : in out Destination;
      Kind  :        MOMA.Types.Destination_Type) is
   begin
      Self.Kind := Kind;
   end Set_Kind;

   ------------
   -- To_Any --
   ------------

   function To_Any
     (Self : Destination)
     return MOMA.Types.Any
   is
      Result : MOMA.Types.Any
        := PolyORB.Any.Get_Empty_Any_Aggregate (TC_MOMA_Destination);

   begin
      PolyORB.Any.Add_Aggregate_Element
        (Result,
         PolyORB.Any.To_Any (Self.Name));

      PolyORB.Any.Add_Aggregate_Element
        (Result,
         PolyORB.Any.ObjRef.To_Any (Self.Ref));

      PolyORB.Any.Add_Aggregate_Element
        (Result,
         MOMA.Types.To_Any (Self.Kind));

      return Result;
   end To_Any;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize;

   procedure Initialize
   is
      use PolyORB.Utils.Strings;
      use PolyORB.Types;

      T : PolyORB.Any.TypeCode.Object := PolyORB.Any.TypeCode.TC_Object;

   begin
      PolyORB.Any.TypeCode.Add_Parameter
        (TC_MOMA_Destination,
         To_Any (To_PolyORB_String ("moma_destination")));

      PolyORB.Any.TypeCode.Add_Parameter
        (TC_MOMA_Destination,
         To_Any (To_PolyORB_String
                 ("MOMA:destinations/moma_destinations:1.0")));

      PolyORB.Any.TypeCode.Add_Parameter
        (TC_MOMA_Destination,
         PolyORB.Any.To_Any (PolyORB.Any.TypeCode.TC_String));

      PolyORB.Any.TypeCode.Add_Parameter
        (TC_MOMA_Destination,
         To_Any (To_PolyORB_String ("name")));

      PolyORB.Any.TypeCode.Add_Parameter
        (T,
         To_Any (To_PolyORB_String ("Object")));

      PolyORB.Any.TypeCode.Add_Parameter
        (T,
         To_Any (To_PolyORB_String ("plop")));

      PolyORB.Any.TypeCode.Add_Parameter
        (TC_MOMA_Destination,
         PolyORB.Any.To_Any (T));

      PolyORB.Any.TypeCode.Add_Parameter
        (TC_MOMA_Destination,
         To_Any (To_PolyORB_String ("ref")));

      PolyORB.Any.TypeCode.Add_Parameter
        (TC_MOMA_Destination,
         PolyORB.Any.To_Any (MOMA.Types.TC_Destination_Type));

      PolyORB.Any.TypeCode.Add_Parameter
        (TC_MOMA_Destination,
         To_Any (To_PolyORB_String ("kind")));
   end Initialize;

begin
   declare
      use PolyORB.Initialization;
      use PolyORB.Initialization.String_Lists;
      use PolyORB.Utils.Strings;

   begin
      Register_Module
        (Module_Info'
         (Name      => +"MOMA.Destinations",
          Conflicts => Empty,
          Depends   => +"MOMA.Types",
          Provides  => Empty,
          Implicit  => False,
          Init      => Initialize'Access));
   end;
end MOMA.Destinations;
