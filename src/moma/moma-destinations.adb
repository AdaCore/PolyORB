------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                    M O M A . D E S T I N A T I O N S                     --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--             Copyright (C) 1999-2002 Free Software Fundation              --
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

with MOMA.Types;

with PolyORB.Any.ObjRef;
with PolyORB.Any.NVList;
with PolyORB.Initialization;
with PolyORB.Requests;
with PolyORB.Types;
with PolyORB.Utils.Strings;

package body MOMA.Destinations is

   use MOMA.Types;

   use PolyORB.Any;
   use PolyORB.Any.ObjRef;
   use PolyORB.Types;

   ------------
   -- Create --
   ------------

   function Create (Name : MOMA.Types.String;
                    Ref  : PolyORB.References.Ref;
                    Kind : MOMA.Types.Destination_Type := MOMA.Types.Unknown)
      return Destination
   is
      Dest : MOMA.Destinations.Destination;
   begin
      Set_Name (Dest, Name);
      Set_Ref  (Dest, Ref);
      Set_Kind (Dest, Kind);
      return Dest;
   end Create;

   function Create
      return Destination
   is
      Dest : MOMA.Destinations.Destination;
   begin
      Set_Name (Dest, To_MOMA_String ("null"));
      Set_Ref  (Dest, PolyORB.References.Nil_Ref);
      Set_Kind (Dest, MOMA.Types.Unknown);
      return Dest;
   end Create;

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

   function From_Any (Self : PolyORB.Any.Any)
                      return MOMA.Destinations.Destination
   is
      Kind : MOMA.Types.Destination_Type := MOMA.Types.Unknown;
      Name : MOMA.Types.String;
      Ref  : PolyORB.References.Ref;
   begin
      Name := From_Any
        (Get_Aggregate_Element (Self,
                                TypeCode.TC_String,
                                PolyORB.Types.Unsigned_Long (0)));

      Ref := From_Any
        (Get_Aggregate_Element (Self,
                                TypeCode.TC_Object,
                                PolyORB.Types.Unsigned_Long (1)));
--      Kind := From_Any
--         (Get_Aggregate_Element (Self,
--                                 TypeCode.TC_Enum,
--                                 PolyORB.Types.Unsigned_Long (2)));
--  XXX To uncomment when it works.

      return Create (Name, Ref, Kind);
   end From_Any;

   --------------
   -- Get_Kind --
   --------------

   function Get_Kind (Self : Destination)
                      return MOMA.Types.Destination_Type is
   begin
      return Self.Kind;
   end Get_Kind;

   --------------
   -- Get_Name --
   --------------

   function Get_Name (Self : Destination)
                      return MOMA.Types.String is
   begin
      return Self.Name;
   end Get_Name;

   -------------
   -- Get_Ref --
   -------------

   function Get_Ref (Self : Destination)
            return PolyORB.References.Ref is
   begin
      return Self.Ref;
   end Get_Ref;

   -----------
   -- Image --
   -----------

   function Image (Self : Destination)
                   return String is
   begin
      return "<name: " & MOMA.Types.To_Standard_String (Self.Name)
             & ",ref: " & PolyORB.References.Image (Self.Ref)
             & ">";
   end Image;

   --------------
   -- Set_Name --
   --------------

   procedure Set_Name (Self : in out Destination;
                       Name : MOMA.Types.String) is
   begin
      Self.Name := Name;
   end Set_Name;

   -------------
   -- Set_Ref --
   -------------

   procedure Set_Ref (Self : in out Destination;
                      Ref  : PolyORB.References.Ref) is
   begin
      Self.Ref := Ref;
   end Set_Ref;

   --------------
   -- Set_Kind --
   --------------

   procedure Set_Kind (Self  : in out Destination;
                       Kind  : MOMA.Types.Destination_Type)
   is
   begin
      Self.Kind := Kind;
   end Set_Kind;

   ---------------
   -- Subscribe --
   ---------------

   procedure Subscribe (Topic : Destination;
                        Pool  : Destination)
   is
      Arg_List : PolyORB.Any.NVList.Ref;
      Request  : PolyORB.Requests.Request_Access;
      Result   : PolyORB.Any.NamedValue;
   begin
      if Get_Kind (Topic) /= MOMA.Types.Topic
      or else Get_Kind (Pool) /= MOMA.Types.Pool then
         raise Program_Error;
      end if;
      PolyORB.Any.NVList.Create (Arg_List);
      PolyORB.Any.NVList.Add_Item (Arg_List,
                                   To_PolyORB_String ("Topic_Id"),
                                   To_Any (Get_Name (Topic)),
                                   PolyORB.Any.ARG_IN);
      PolyORB.Any.NVList.Add_Item (Arg_List,
                                   To_PolyORB_String ("Pool_Ref"),
                                   To_Any (Get_Ref (Pool)),
                                   PolyORB.Any.ARG_IN);
      Result := (Name      => To_PolyORB_String ("Result"),
                 Argument  => PolyORB.Any.Get_Empty_Any (PolyORB.Any.TC_Void),
                 Arg_Modes => 0);
      PolyORB.Requests.Create_Request
        (Target    => Get_Ref (Topic),
         Operation => "Subscribe",
         Arg_List  => Arg_List,
         Result    => Result,
         Req       => Request);
      PolyORB.Requests.Invoke (Request);
      PolyORB.Requests.Destroy_Request (Request);
   end Subscribe;

   ------------
   -- To_Any --
   ------------

   function To_Any (Self : Destination)
                    return PolyORB.Any.Any
   is
      Result : Any := Get_Empty_Any_Aggregate (TC_MOMA_Destination);
   begin
      Add_Aggregate_Element (Result, To_Any (Self.Name));
      Add_Aggregate_Element (Result, To_Any (Self.Ref));
--      Add_Aggregate_Element (Result, To_Any (Self.Kind));
--  To uncomment when it works.
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
      TypeCode.Add_Parameter (TC_MOMA_Destination,
                              To_Any (To_PolyORB_String ("moma_destination")));
      TypeCode.Add_Parameter
        (TC_MOMA_Destination,
         To_Any (To_PolyORB_String
                 ("MOMA:destinations/moma_destinations:1.0")));

      TypeCode.Add_Parameter (TC_MOMA_Destination, To_Any (TC_String));
      TypeCode.Add_Parameter (TC_MOMA_Destination,
                              To_Any (To_PolyORB_String ("name")));

      TypeCode.Add_Parameter (T, To_Any (To_PolyORB_String ("Object")));
      TypeCode.Add_Parameter (T, To_Any (To_PolyORB_String ("plop")));


      TypeCode.Add_Parameter (TC_MOMA_Destination, To_Any (T));
      TypeCode.Add_Parameter (TC_MOMA_Destination,
                              To_Any (To_PolyORB_String ("ref")));
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
          Depends   => +"soft_links",
          Provides  => Empty,
          Init      => Initialize'Access));
   end;

end MOMA.Destinations;
