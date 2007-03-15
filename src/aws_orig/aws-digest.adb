------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                           A W S . D I G E S T                            --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2000-2007, Free Software Foundation, Inc.          --
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

--  @@@ uses ada.calendar

with Ada.Calendar;
with Ada.Unchecked_Conversion;
with Ada.Strings.Maps.Constants;

with AWS.Utils;
pragma Elaborate_All (AWS.Utils);

package body AWS.Digest is

   use Ada;

   Private_Key : MD5.Context;

   Nonce_Expiration : constant Duration := 300.0;

   subtype Byte_Array_Of_Integer
      is MD5.Byte_Array (1 .. Integer'Size / MD5.Byte_Array'Component_Size);

   function To_Byte_Array is
      new Ada.Unchecked_Conversion (Integer, Byte_Array_Of_Integer);

   -----------------
   -- Check_Nonce --
   -----------------

   function Check_Nonce (Value : String) return Boolean is
      use Calendar;
      use type MD5.Byte_Array;

      F             : constant Positive := Value'First;

      Now           : constant Time := Clock;
      Nonce_Time    : Time;
      Year_Now      : Year_Number;
      Month_Now     : Month_Number;
      Day_Now       : Day_Number;
      Seconds_Now   : Day_Duration;

      Seconds_Nonce : Natural;
      Ctx           : MD5.Context;
      Digest        : MD5.Fingerprint;
      Sample        : Digest_String;
   begin
      --  Our nonces length is length of Digest plus 5 symbols for
      --  Day durations.

      if Value'Length /= Digest_String'Length + 5 then
         return False;
      end if;

      Split (Now, Year_Now, Month_Now, Day_Now, Seconds_Now);

      declare
         use Ada.Strings.Maps;
      begin
         if not Is_Subset
           (To_Set (Value (F .. F + 4)), Constants.Hexadecimal_Digit_Set)
         then
            return False;
         end if;
      end;

      Seconds_Nonce := Utils.Hex_Value (Value (F .. F + 4));

      Nonce_Time := Time_Of
        (Year_Now, Month_Now, Day_Now, Day_Duration (Seconds_Nonce));

      if Nonce_Time > Now then
         --  Could be next day
         Nonce_Time := Nonce_Time - Day_Duration'Last;

         Split (Nonce_Time,
                Year_Now, Month_Now, Day_Now, Seconds_Now);
      end if;

      if Now - Nonce_Time > Nonce_Expiration then
         return False;
      end if;

      Ctx := Private_Key;

      MD5.Update (Ctx,
                  To_Byte_Array (Year_Now)
                    & To_Byte_Array (Month_Now)
                    & To_Byte_Array (Day_Now)
                    & To_Byte_Array (Seconds_Nonce));

      MD5.Final (Ctx, Digest);

      Sample := MD5.Digest_To_Text (Digest);

      return Value (F + 5 .. Value'Last) = Sample;
   end Check_Nonce;

   -------------------
   -- Create_Digest --
   -------------------

   function Create_Digest
     (Username, Realm, Password : String;
      Nonce, NC, CNonce, QOP    : String;
      Method, URI               : String)
      return Digest_String is
   begin
      return Create_Digest
        (Username => Username,
         Realm    => Realm,
         Password => Password,
         Nonce    => Nonce & ':' & NC & ':' & CNonce & ':' & QOP,
         Method   => Method,
         URI      => URI);
   end Create_Digest;

   function Create_Digest
     (Username, Realm, Password : String;
      Nonce                     : String;
      Method, URI               : String)
      return Digest_String is
   begin
      return Utils.Get_MD5
        (Utils.Get_MD5 (Username & ':' & Realm & ':' & Password)
           & ':' & Nonce & ':'
           & Utils.Get_MD5 (Method & ':' & URI));
   end Create_Digest;

   ------------------
   -- Create_Nonce --
   ------------------

   function Create_Nonce return String is
      use Calendar;

      Year_Now    : Year_Number;
      Month_Now   : Month_Number;
      Day_Now     : Day_Number;
      Seconds_Now : Day_Duration;

      Seconds_Int : Natural;
      Digest      : MD5.Fingerprint;
      Ctx         : MD5.Context;
      Result      : Digest_String;
   begin
      Split (Clock, Year_Now, Month_Now, Day_Now, Seconds_Now);

      Ctx := Private_Key;

      Seconds_Int := Natural (Float'Floor (Float (Seconds_Now)));

      MD5.Update (Ctx, To_Byte_Array (Year_Now));
      MD5.Update (Ctx, To_Byte_Array (Month_Now));
      MD5.Update (Ctx, To_Byte_Array (Day_Now));
      MD5.Update (Ctx, To_Byte_Array (Seconds_Int));
      MD5.Final (Ctx, Digest);

      --  Place the digest string representation into the result variable

      Result := MD5.Digest_To_Text (Digest);

      --  Five hex digits before MD5 digest for the nonce expiration check

      return Utils.Hex (Seconds_Int, Width => 5) & Result;
   end Create_Nonce;

begin
   MD5.Update (Private_Key,
               Utils.Random_Integer'Image (Utils.Random));
end AWS.Digest;
