------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                                  M D 5                                   --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--           Copyright (C) 2006, Free Software Foundation, Inc.             --
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

package MD5 is

   --  ====================================================================
   --   Authors   Rolf Ebert (no known address),
   --             Christoph Grein <Christ-Usch.Grein@T-Online.de>
   --   Version   1.1
   --   Date      16 January 1999
   --  ====================================================================
   --   The Message-Digest MD5 Algorithm of RSA Data Security, Inc.
   --
   --   The official description of this algorithm can be found on the
   --   site of the RSA Data Security, Inc. The following is a quote from
   --   this description.
   --
   --     The algorithm takes as input a message of arbitrary length and
   --     produces as output a 128-bit "fingerprint" or "message digest"
   --     of the input. It is conjectured that it is computationally
   --     infeasible to produce two messages having the same message
   --     digest, or to produce any message having a given prespecified
   --     target message digest. The MD5 algorithm is intended for digital
   --     signature applications, where a large file must be "compressed"
   --     in a secure manner before being encrypted with a private
   --     (secret) key under a public-key cryptosystem such as RSA.
   --
   --   The official description of the MD5 algorithm can be found at
   --     <ftp://ftp.rsa.com/pub/md5.txt>
   --   License is granted by RSA Data Security, Inc. <http://www.rsa.com>
   --   to make and use derivative works provided that such works are
   --   identified as "derived from the RSA Data Security, Inc. MD5
   --   Message-Digest Algorithm" in all material mentioning or referencing
   --   the derived work. (See the copyright notice in the official
   --   description.)
   --
   --   Usage:
   --   - First call Init.
   --   - Call Update (repeatedly) until all input has been digested.
   --     Long messages may be digested in chunks of any length (the
   --     reference implementation uses chunks of 1024 bits).
   --   - Call Final.
   --   For text output, the fingerprint may be converted to a hexadecimal
   --   string.
   --  ====================================================================
   --   History
   --   Author Version   Date    Reason for change
   --    R.E.    1.0  04.06.1997 Original as found in internet
   --    C.G.    1.1  16.01.1999 Minor code changes; commented to make
   --                            publication legal
   --  ====================================================================

   type Byte is mod 2**8;
   type Byte_Array is array (Long_Integer range <>) of Byte;
   pragma Pack (Byte_Array);

   subtype Fingerprint   is Byte_Array (1 .. 16);  --  128 bits
   subtype Digest_String is String     (1 .. 32);  --  Fingerprint in hex

   --  Create a fingerprint

   type Context is private;

   procedure Init   (Ctx :    out Context);
   procedure Update (Ctx : in out Context; Data   : Byte_Array);
   procedure Update (Ctx : in out Context; Data   : String);
   procedure Final  (Ctx : in out Context; Digest :    out Fingerprint);

   --  Conversions

   Malformed : exception;  --  may be raised in Digest_From_Text

   function Digest_From_Text (S : Digest_String) return Fingerprint;
   function Digest_To_Text   (A : Fingerprint)   return Digest_String;

private

   type Word is mod 2**32;
   type Word_Array is array (Long_Integer range <>) of Word;
   pragma Pack (Word_Array);

   subtype ABCD_State is Word_Array (1 .. 4);
   subtype Count_T    is Word_Array (1 .. 2);

   subtype Buffer_T   is Byte_Array (1 .. 64);

   type Context is record
      State  : ABCD_State;
      Count  : Count_T;
      Buffer : Buffer_T;
   end record;

end MD5;
