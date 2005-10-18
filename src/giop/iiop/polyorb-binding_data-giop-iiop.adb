------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--       P O L Y O R B . B I N D I N G _ D A T A . G I O P . I I O P        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2002-2005 Free Software Foundation, Inc.           --
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

--  Binding data concrete implementation for IIOP.

with Ada.Streams;

with PolyORB.Binding_Data.GIOP.INET;
with PolyORB.Binding_Data_QoS;
with PolyORB.GIOP_P.Transport_Mechanisms.IIOP;
with PolyORB.Initialization;
with PolyORB.Log;
with PolyORB.Obj_Adapter_QoS;
with PolyORB.ORB;
with PolyORB.Parameters;
with PolyORB.POA;
with PolyORB.POA_Types;
with PolyORB.QoS.Tagged_Components;
with PolyORB.References.Corbaloc;
with PolyORB.References.IOR;
with PolyORB.Setup;
with PolyORB.Sockets;
with PolyORB.Utils.Strings;

package body PolyORB.Binding_Data.GIOP.IIOP is

   use PolyORB.Binding_Data.GIOP.INET;
   use PolyORB.Binding_Data.GIOP.IIOP;
   use PolyORB.GIOP_P.Tagged_Components;
   use PolyORB.GIOP_P.Transport_Mechanisms;
   use PolyORB.GIOP_P.Transport_Mechanisms.IIOP;
   use PolyORB.Log;
   use PolyORB.Objects;
   use PolyORB.References.Corbaloc;
   use PolyORB.References.IOR;
   use PolyORB.Types;

   package L is new PolyORB.Log.Facility_Log
     ("polyorb.binding_data.giop.iiop");
   procedure O (Message : in Standard.String; Level : Log_Level := Debug)
     renames L.Output;

   IIOP_Corbaloc_Prefix : constant String := "iiop";

   Preference : Profile_Preference;
   --  Global variable: the preference to be returned
   --  by Get_Profile_Preference for IIOP profiles.

   function Profile_To_Corbaloc (P : Profile_Access) return String;
   function Corbaloc_To_Profile (Str : String) return Profile_Access;

   function Get_Primary_IIOP_Address
     (Profile : IIOP_Profile_Type)
     return PolyORB.Sockets.Sock_Addr_Type;
   --  Return primary address of profile (address of the first profile's
   --  transport mechanims)

   -------------------------------------
   -- Add_Transport_Mechanism_Factory --
   -------------------------------------

   procedure Add_Transport_Mechanism_Factory
     (PF : in out IIOP_Profile_Factory;
      MF :        Transport_Mechanism_Factory_Access)
   is
   begin
      Append (PF.Mechanisms, MF);
   end Add_Transport_Mechanism_Factory;

   ---------------------
   -- Get_Profile_Tag --
   ---------------------

   function Get_Profile_Tag (Profile : IIOP_Profile_Type) return Profile_Tag is
      pragma Unreferenced (Profile);

   begin
      return Tag_Internet_IOP;
   end Get_Profile_Tag;

   ----------------------------
   -- Get_Profile_Preference --
   ----------------------------

   function Get_Profile_Preference
     (Profile : IIOP_Profile_Type)
     return Profile_Preference
   is
      pragma Unreferenced (Profile);

   begin
      return Preference;
   end Get_Profile_Preference;

   ------------------------------
   -- Get_Primary_IIOP_Address --
   ------------------------------

   function Get_Primary_IIOP_Address
     (Profile : IIOP_Profile_Type)
     return PolyORB.Sockets.Sock_Addr_Type
   is
   begin
      return
         Primary_Address_Of
         (IIOP_Transport_Mechanism
          (Get_Primary_Transport_Mechanism (Profile).all));
   end Get_Primary_IIOP_Address;

   --------------------
   -- Create_Factory --
   --------------------

   procedure Create_Factory
     (PF  : out IIOP_Profile_Factory;
      TAP :     Transport.Transport_Access_Point_Access;
      ORB :     Components.Component_Access)
   is
      pragma Unreferenced (ORB);

      MF : constant Transport_Mechanism_Factory_Access
        := new IIOP_Transport_Mechanism_Factory;

   begin
      Create_Factory (MF.all, TAP);
      Append (PF.Mechanisms, MF);
   end Create_Factory;

   --------------------
   -- Create_Profile --
   --------------------

   function Create_Profile
     (PF  : access IIOP_Profile_Factory;
      Oid :        Objects.Object_Id)
     return Profile_Access
   is
      use PolyORB.QoS;
      use PolyORB.QoS.Tagged_Components;
      use Transport_Mechanism_Factory_Lists;

      Result  : constant Profile_Access := new IIOP_Profile_Type;
      TResult : IIOP_Profile_Type renames IIOP_Profile_Type (Result.all);

      Iter    : Transport_Mechanism_Factory_Lists.Iterator
        := First (PF.Mechanisms);

   begin
      TResult.Version_Major := IIOP_Version_Major;
      TResult.Version_Minor := IIOP_Version_Minor;
      TResult.Object_Id     := new Object_Id'(Oid);

      --  Create primary transport mechanism

      Append
        (TResult.Mechanisms,
         Create_Transport_Mechanism
         (IIOP_Transport_Mechanism_Factory (Value (Iter).all.all)));

      --  Fetch tagged components for Oid

      TResult.Components := Fetch_Components (TResult.Object_Id);

      --  Create tagged components for additional transport mechanisms

      while not Last (Iter) loop
         Add
           (TResult.Components,
            Create_Tagged_Components (Value (Iter).all.all));

         Next (Iter);
      end loop;

      --  Append tagged components attached to the servant

      declare
         use Ada.Streams;
         use PolyORB.Errors;
         use PolyORB.Obj_Adapter_QoS;
         use PolyORB.POA;
         use PolyORB.POA_Types;

         U_Oid  : Unmarshalled_Oid;
         Obj_OA : PolyORB.POA.Obj_Adapter_Access;
         Error  : Error_Container;
         QoS    : QoS_GIOP_Tagged_Components_Parameter_Access;

      begin
         Oid_To_U_Oid (TResult.Object_Id.all, U_Oid, Error);

         if Found (Error) then
            raise Program_Error;
         end if;

         Find_POA
           (PolyORB.POA.Obj_Adapter_Access (Get_OA (TResult)),
            To_Standard_String (U_Oid.Creator),
            True,
            Obj_OA,
            Error);

         if Found (Error) then
            raise Program_Error;
         end if;

         QoS := QoS_GIOP_Tagged_Components_Parameter_Access
           (Get_Object_Adapter_QoS (Obj_OA, GIOP_Tagged_Components));

         if QoS /= null then
            declare
               use GIOP_Tagged_Component_Lists;

               Iter : GIOP_Tagged_Component_Lists.Iterator
                 := First (QoS.Components);

            begin
               while not Last (Iter) loop
                  Add
                    (TResult.Components,
                     Create_Unknown_Component
                     (Tag_Value (Value (Iter).Tag),
                      new Stream_Element_Array'(Value (Iter).Data.all)));
                  Next (Iter);
               end loop;
            end;
         end if;
      end;

      --  Calculate Profile's Tagged Components QoS

      PolyORB.Binding_Data_QoS.Set_Profile_QoS
        (Result,
         GIOP_Tagged_Components,
         new QoS_GIOP_Tagged_Components_Parameter'
         (GIOP_Tagged_Components,
          Create_QoS_GIOP_Tagged_Components_List (TResult.Components)));

      --  Calculate additional transport mechanisms

      TResult.Mechanisms :=
        TResult.Mechanisms
        & Create_Transport_Mechanisms (TResult.Components, Result);

      return Result;
   end Create_Profile;

   -------------------------------------
   -- Disable_Unprotected_Invocations --
   -------------------------------------

   procedure Disable_Unprotected_Invocations
     (PF : in out IIOP_Profile_Factory)
   is
   begin
      Disable_Transport_Mechanism
        (IIOP_Transport_Mechanism_Factory
         (Element (PF.Mechanisms, 0).all.all));
   end Disable_Unprotected_Invocations;

   -----------------------
   -- Duplicate_Profile --
   -----------------------

   function Duplicate_Profile (P : IIOP_Profile_Type) return Profile_Access is
      Result : constant Profile_Access := new IIOP_Profile_Type;

      TResult : IIOP_Profile_Type
        renames IIOP_Profile_Type (Result.all);

      PP : IIOP_Profile_Type renames P;

   begin
      TResult.Version_Major := PP.Version_Major;
      TResult.Version_Minor := PP.Version_Minor;
      TResult.Object_Id     := new Object_Id'(PP.Object_Id.all);
      TResult.Components    := Deep_Copy (PP.Components);
      TResult.Mechanisms    := Deep_Copy (PP.Mechanisms);

      return Result;
   end Duplicate_Profile;

   --------------------------------
   -- Marshall_IIOP_Profile_Body --
   --------------------------------

   procedure Marshall_IIOP_Profile_Body
     (Buf     : access Buffer_Type;
      Profile :        Profile_Access)
   is
   begin
      Common_Marshall_Profile_Body
        (Buf,
         Profile,
         Get_Primary_IIOP_Address (IIOP_Profile_Type (Profile.all)),
         True);
   end Marshall_IIOP_Profile_Body;

   ----------------------------------
   -- Unmarshall_IIOP_Profile_Body --
   ----------------------------------

   function Unmarshall_IIOP_Profile_Body
     (Buffer : access Buffer_Type)
      return Profile_Access
   is
      use PolyORB.QoS;
      use PolyORB.QoS.Tagged_Components;

      Result  : constant Profile_Access := new IIOP_Profile_Type;
      TResult : IIOP_Profile_Type renames IIOP_Profile_Type (Result.all);
      Address : PolyORB.Sockets.Sock_Addr_Type;

   begin
      Common_Unmarshall_Profile_Body (Buffer, Result, Address, True, False);

      --  Create primary transport mechanism

      Append (TResult.Mechanisms, Create_Transport_Mechanism (Address));

      --  Calculate additional transport mechanisms

      TResult.Mechanisms :=
        TResult.Mechanisms
        & Create_Transport_Mechanisms
        (IIOP_Profile_Type (Result.all).Components,
         Result);

      --  Calculate Profile's Tagged Components QoS

      PolyORB.Binding_Data_QoS.Set_Profile_QoS
        (Result,
         GIOP_Tagged_Components,
         new QoS_GIOP_Tagged_Components_Parameter'
         (GIOP_Tagged_Components,
          Create_QoS_GIOP_Tagged_Components_List (TResult.Components)));

      return Result;
   end Unmarshall_IIOP_Profile_Body;

   -----------
   -- Image --
   -----------

   function Image (Prof : IIOP_Profile_Type) return String is
   begin
      return "Address : "
        & PolyORB.Sockets.Image (Get_Primary_IIOP_Address (Prof))
        & ", Object_Id : "
        & PolyORB.Objects.Image (Prof.Object_Id.all);
   end Image;

   -------------------------
   -- Profile_To_Corbaloc --
   -------------------------

   function Profile_To_Corbaloc (P : Profile_Access) return String is
   begin
      pragma Debug (O ("IIOP Profile to corbaloc"));
      return Common_IIOP_DIOP_Profile_To_Corbaloc
        (P,
         Get_Primary_IIOP_Address (IIOP_Profile_Type (P.all)),
         IIOP_Corbaloc_Prefix);
   end Profile_To_Corbaloc;

   -------------------------
   -- Corbaloc_To_Profile --
   -------------------------

   function Corbaloc_To_Profile (Str : String) return Profile_Access is
      Result  : Profile_Access := new IIOP_Profile_Type;
      Address : Sockets.Sock_Addr_Type;

   begin
      Common_IIOP_DIOP_Corbaloc_To_Profile
        (Str, IIOP_Version_Major, IIOP_Version_Minor, Result, Address);

      --  Create primary transport mechanism

      Append
        (IIOP_Profile_Type (Result.all).Mechanisms,
         Create_Transport_Mechanism (Address));

      return Result;
   end Corbaloc_To_Profile;

   ------------
   -- Get_OA --
   ------------

   function Get_OA
     (Profile : IIOP_Profile_Type)
     return PolyORB.Smart_Pointers.Entity_Ptr
   is
      pragma Unreferenced (Profile);

   begin
      return PolyORB.Smart_Pointers.Entity_Ptr
        (PolyORB.ORB.Object_Adapter (PolyORB.Setup.The_ORB));
   end Get_OA;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize;

   procedure Initialize is
      Preference_Offset : constant String
        := PolyORB.Parameters.Get_Conf
        (Section => "iiop",
         Key     => "polyorb.binding_data.iiop.preference",
         Default => "0");

   begin
      Preference := Preference_Default + Profile_Preference'Value
        (Preference_Offset);
      Register
       (Tag_Internet_IOP,
        Marshall_IIOP_Profile_Body'Access,
        Unmarshall_IIOP_Profile_Body'Access);
      Register
        (Tag_Internet_IOP,
         IIOP_Corbaloc_Prefix,
         Profile_To_Corbaloc'Access,
         Corbaloc_To_Profile'Access);
   end Initialize;

   use PolyORB.Initialization;
   use PolyORB.Initialization.String_Lists;
   use PolyORB.Utils.Strings;

begin
   Register_Module
     (Module_Info'
      (Name      => +"binding_data.iiop",
       Conflicts => Empty,
       Depends   => +"protocols.giop.iiop" & "sockets",
       Provides  => +"binding_factories",
       Implicit  => False,
       Init      => Initialize'Access));
end PolyORB.Binding_Data.GIOP.IIOP;
