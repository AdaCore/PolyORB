------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--       P O L Y O R B . B I N D I N G _ D A T A . G I O P . I N E T        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2004 Free Software Foundation, Inc.             --
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

with Ada.Streams;

with PolyORB.Buffers;
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
   procedure O (Message : in Standard.String; Level : Log_Level := Debug)
     renames L.Output;

   ------------------------------------------
   -- Common_IIOP_DIOP_Corbaloc_To_Profile --
   ------------------------------------------

   procedure Common_IIOP_DIOP_Corbaloc_To_Profile
     (Str           : in     Types.String;
      Prefix_Length : in     Natural;
      Profile       : in out Profile_Access;
      Address       :    out  Sockets.Sock_Addr_Type)
   is
      use PolyORB.Utils;

      TResult : GIOP_Profile_Type'Class
        renames GIOP_Profile_Type'Class (Profile.all);
      S       : constant String
        := To_Standard_String (Str) (Prefix_Length + 1 .. Length (Str));
      Index   : Integer := S'First;
      Index2  : Integer;

   begin
      pragma Debug (O ("Common_IIOP_DIOP_Corbaloc_To_Profile: enter"));

      Index2 := Find (S, Index, '.');
      if Index2 = S'Last + 1 then
         Destroy_Profile (Profile);
         return;
      end if;
      TResult.Version_Major := Octet'Value (S (Index .. Index2 - 1));
      Index := Index2 + 1;

      Index2 := Find (S, Index, '@');
      if Index2 = S'Last + 1 then
         Destroy_Profile (Profile);
         return;
      end if;
      TResult.Version_Minor := Octet'Value (S (Index .. Index2 - 1));
      Index := Index2 + 1;

      Index2 := Find (S, Index, ':');
      if Index2 = S'Last + 1 then
         Destroy_Profile (Profile);
         return;
      end if;
      pragma Debug (O ("Address = " & S (Index .. Index2 - 1)));
      Address.Addr :=
        String_To_Addr (To_PolyORB_String (S (Index .. Index2 - 1)));
      Index := Index2 + 1;

      Index2 := Find (S, Index, '/');
      if Index2 = S'Last + 1 then
         Destroy_Profile (Profile);
         return;
      end if;
      pragma Debug (O ("Port = " & S (Index .. Index2 - 1)));
      Address.Port :=
        PolyORB.Sockets.Port_Type'Value (S (Index .. Index2 - 1));
      Index := Index2 + 1;

      declare
         Oid_Str : constant String := URI_Decode (S (Index .. S'Last));
         Oid     : Object_Id (Stream_Element_Offset (Index)
                           .. Stream_Element_Offset (S'Last));
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
     (Profile : in Profile_Access;
      Address : in Sockets.Sock_Addr_Type;
      Prefix  : in Types.String)
     return Types.String
   is
      use PolyORB.Sockets;
      use PolyORB.Utils;

      GIOP_Profile : GIOP_Profile_Type'Class
        renames GIOP_Profile_Type'Class (Profile.all);
      Oid_Str : String (Integer (Profile.Object_Id'First)
                     .. Integer (Profile.Object_Id'Last));
      pragma Import (Ada, Oid_Str);
      for Oid_Str'Address use
        Profile.Object_Id (Profile.Object_Id'First)'Address;
   begin
      pragma Debug (O ("Common_IIOP_DIOP_Profile_To_Corbaloc"));

      return Prefix &
        Trimmed_Image (Integer (GIOP_Profile.Version_Major)) & "." &
        Trimmed_Image (Integer (GIOP_Profile.Version_Minor)) & "@" &
        Image (Address.Addr) & ":" &
        Trimmed_Image (Integer (Address.Port)) & "/" &
        URI_Encode (Oid_Str);
   end Common_IIOP_DIOP_Profile_To_Corbaloc;

   ----------------------------------
   -- Common_Marshall_Profile_Body --
   ----------------------------------

   procedure Common_Marshall_Profile_Body
     (Buffer             : access Buffer_Type;
      Profile            : in     Profile_Access;
      Address            : in     Sockets.Sock_Addr_Type;
      Marshall_Object_Id : in     Boolean)
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
      Profile                      : in     Profile_Access;
      Address                      : in out Sockets.Sock_Addr_Type;
      Unmarshall_Object_Id         : in     Boolean;
      Unmarshall_Tagged_Components : in     Boolean)
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

      pragma Debug (O ("Unmarshall_IIOP_Profile_body: leave"));
   end Common_Unmarshall_Profile_Body;

end PolyORB.Binding_Data.GIOP.INET;
