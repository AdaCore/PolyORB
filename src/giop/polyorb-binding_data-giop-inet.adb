------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--       P O L Y O R B . B I N D I N G _ D A T A . G I O P . I N E T        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2004-2006, Free Software Foundation, Inc.          --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
-- not, write to the Free Software Foundation, 51 Franklin Street, Fifth    --
-- Floor, Boston, MA 02111-1301, USA.                                       --
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

with Ada.Streams;

with PolyORB.Log;
with PolyORB.Representations.CDR.Common;
with PolyORB.Utils.Sockets;
with PolyORB.Types;

package body PolyORB.Binding_Data.GIOP.INET is

   use Ada.Streams;

   use PolyORB.GIOP_P.Tagged_Components;

   use PolyORB.Buffers;
   use PolyORB.Log;
   use PolyORB.Objects;
   use PolyORB.Representations.CDR.Common;
   use PolyORB.Utils.Sockets;
   use PolyORB.Types;

   package L is
      new PolyORB.Log.Facility_Log
     ("polyorb.binding_data.giop.common_sockets");
   procedure O (Message : Standard.String; Level : Log_Level := Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
     renames L.Enabled;
   pragma Unreferenced (C); --  For conditional pragma Debug

   ------------------------------------------
   -- Common_IIOP_DIOP_Corbaloc_To_Profile --
   ------------------------------------------

   procedure Common_IIOP_DIOP_Corbaloc_To_Profile
     (Str           : String;
      Default_Major : Types.Octet;
      Default_Minor : Types.Octet;
      Profile       : in out Profile_Access;
      Address       :    out Sockets.Sock_Addr_Type)
   is
      use PolyORB.Utils;

      TResult : GIOP_Profile_Type'Class
        renames GIOP_Profile_Type'Class (Profile.all);
      S       : String renames Str;
      Index   : Integer;
      Index2  : Integer;

   begin
      pragma Debug (O ("Common_IIOP_DIOP_Corbaloc_To_Profile: enter"));

      --  Index is at start of iiop_addr

      --  Version present?

      Index := Find (S, S'First, '@');
      if Index in S'First + 1 .. S'Last then
         Index2 := Find (S, S'First, '.');
         if S'First < Index2 and then Index2 < Index then
            TResult.Version_Major := Octet'Value (S (S'First .. Index2 - 1));
            TResult.Version_Minor := Octet'Value (S (Index2 + 1 .. Index - 1));
         else
            Destroy_Profile (Profile);
         end if;
         Index := Index + 1;
      else
         TResult.Version_Major := Default_Major;
         TResult.Version_Minor := Default_Minor;
         Index := S'First;
      end if;

      --  Index at start of host

      declare
         Colon : constant Integer := Find (S, Index, ':');
         Slash : constant Integer := Find (S, Index, '/');
      begin
         if Colon < Slash then
            --  Port number is present
            Index2 := Colon - 1;
         else
            Index2 := Slash - 1;
         end if;

         if Index2 < Index then
            --  Empty host
            Destroy_Profile (Profile);
            return;
         end if;
         pragma Debug (O ("Address = " & S (Index .. Index2)));
         Address.Addr := String_To_Addr (S (Index .. Index2));

         if Colon < Slash then
            if Colon + 1 < Slash then
               pragma Debug (O ("Port = " & S (Colon + 1 .. Slash - 1)));
               Address.Port :=
                  PolyORB.Sockets.Port_Type'Value (S (Colon + 1 .. Slash - 1));
            else
               --  Empty port
               Destroy_Profile (Profile);
               return;
            end if;
         else
            --  No port indication: default to IANA-reserved value
            Address.Port := 2809;
         end if;
         Index := Slash + 1;
      end;

      if Index > S'Last then
         --  Empty key_string
         Destroy_Profile (Profile);
         return;
      end if;

      declare
         Oid_Str : constant String := URI_Decode (S (Index .. S'Last));
         Oid     : Object_Id (Stream_Element_Offset (Oid_Str'First)
                           .. Stream_Element_Offset (Oid_Str'Last));
         pragma Import (Ada, Oid);
         for Oid'Address use Oid_Str (Oid_Str'First)'Address;
      begin
         TResult.Object_Id := new Object_Id'(Oid);
      end;

      if TResult.Object_Id = null then
         Destroy_Profile (Profile);
         return;
      end if;

      pragma Debug (O ("Oid = " & Image (TResult.Object_Id.all)));

      TResult.Components := Null_Tagged_Component_List;
      pragma Debug (O ("Common_IIOP_DIOP_Corbaloc_To_Profile: leave"));
   end Common_IIOP_DIOP_Corbaloc_To_Profile;

   ------------------------------------------
   -- Common_IIOP_DIOP_Profile_To_Corbaloc --
   ------------------------------------------

   function Common_IIOP_DIOP_Profile_To_Corbaloc
     (Profile : Profile_Access;
      Address : Sockets.Sock_Addr_Type;
      Prefix  : String)
     return String
   is
      use PolyORB.Sockets;
      use PolyORB.Utils;

      GIOP_Profile : GIOP_Profile_Type'Class
        renames GIOP_Profile_Type'Class (Profile.all);
      Oid_Str : String (1 .. Profile.Object_Id'Length);
      pragma Import (Ada, Oid_Str);
      for Oid_Str'Address use
        Profile.Object_Id (Profile.Object_Id'First)'Address;
   begin
      pragma Debug (O ("Common_IIOP_DIOP_Profile_To_Corbaloc"));

      return Prefix & ":" &
        Trimmed_Image (Integer (GIOP_Profile.Version_Major)) & "." &
        Trimmed_Image (Integer (GIOP_Profile.Version_Minor)) & "@" &
        Image (Address.Addr) & ":" &
        Trimmed_Image (Integer (Address.Port)) & "/" &
        URI_Encode (Oid_Str, Also_Escape => No_Escape);
   end Common_IIOP_DIOP_Profile_To_Corbaloc;

   ----------------------------------
   -- Common_Marshall_Profile_Body --
   ----------------------------------

   procedure Common_Marshall_Profile_Body
     (Buffer             : access Buffer_Type;
      Profile            : Profile_Access;
      Address            : Sockets.Sock_Addr_Type;
      Marshall_Object_Id : Boolean)
   is
      GIOP_Profile : GIOP_Profile_Type'Class
        renames GIOP_Profile_Type'Class (Profile.all);
      Profile_Body : Buffer_Access := new Buffer_Type;

   begin
      pragma Debug (O ("Common_Marshall_Profile_Body: enter"));

      --  A Profile Body is an encapsulation

      Start_Encapsulation (Profile_Body);

      --  Version

      Marshall (Profile_Body, GIOP_Profile.Version_Major);
      Marshall (Profile_Body, GIOP_Profile.Version_Minor);

      pragma Debug
        (O ("  Version = " & GIOP_Profile.Version_Major'Img & "."
            & GIOP_Profile.Version_Minor'Img));

      --  Marshalling of a Socket

      Marshall_Socket (Profile_Body, Address);
      pragma Debug (O ("  Address = " & Sockets.Image (Address)));

      --  Marshalling the object id

      if Marshall_Object_Id then
         Marshall
           (Profile_Body,
            Stream_Element_Array (GIOP_Profile.Object_Id.all));
      end if;

      --  Marshalling the tagged components

      Marshall_Tagged_Component (Profile_Body, GIOP_Profile.Components);

      --  Marshalling the Profile_Body into IOR

      Marshall (Buffer, Encapsulate (Profile_Body));
      Release (Profile_Body);

      pragma Debug (O ("Common_Marshall_Profile_Body: leave"));
   end Common_Marshall_Profile_Body;

   ------------------------------------
   -- Common_Unmarshall_Profile_Body --
   ------------------------------------

   procedure Common_Unmarshall_Profile_Body
     (Buffer                       : access Buffer_Type;
      Profile                      : Profile_Access;
      Address                      : in out Sockets.Sock_Addr_Type;
      Unmarshall_Object_Id         : Boolean;
      Unmarshall_Tagged_Components : Boolean)
   is
      TResult        : GIOP_Profile_Type'Class
        renames GIOP_Profile_Type'Class (Profile.all);
      Profile_Body   : aliased Encapsulation := Unmarshall (Buffer);
      Profile_Buffer : Buffer_Access := new Buffers.Buffer_Type;
   begin
      pragma Debug (O ("Common_Unmarshall_Profile_Body: enter"));

      --  A Profile Body is an encapsulation

      Decapsulate (Profile_Body'Access, Profile_Buffer);

      TResult.Version_Major := Unmarshall (Profile_Buffer);
      TResult.Version_Minor := Unmarshall (Profile_Buffer);

      pragma Debug
        (O ("  Version = " & TResult.Version_Major'Img & "."
            & TResult.Version_Minor'Img));

      --  Unmarshalling the socket

      Unmarshall_Socket (Profile_Buffer, Address);

      pragma Debug (O ("  Address = " & Sockets.Image (Address)));

      --  Unmarshalling the object id

      if Unmarshall_Object_Id then
         declare
            Str : aliased constant Stream_Element_Array
              := Unmarshall (Profile_Buffer);
         begin
            TResult.Object_Id := new Object_Id'(Object_Id (Str));
         end;
      end if;

      if TResult.Version_Minor /= 0
        or else Unmarshall_Tagged_Components
      then
         TResult.Components :=
           Unmarshall_Tagged_Component (Profile_Buffer);
      end if;

      Release (Profile_Buffer);

      pragma Debug (O ("Common_Unmarshall_Profile_body: leave"));
   end Common_Unmarshall_Profile_Body;

end PolyORB.Binding_Data.GIOP.INET;
