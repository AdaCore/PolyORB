------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                  POLYORB.REPRESENTATIONS.CDR.GIOP_1_2                    --
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

package body PolyORB.Representations.CDR.GIOP_1_2 is

   --------------
   -- Marshall --
   --------------

   procedure Marshall
     (R      : in     GIOP_1_2_CDR_Representation;
      Buffer : access Buffers.Buffer_Type;
      Data   : in     PolyORB.Types.Wchar;
      Error  : in out Exceptions.Error_Container)
   is
      pragma Unreferenced (R);
      pragma Unreferenced (Error);

   begin
      Marshall (Buffer, Data);
   end Marshall;

   --------------
   -- Marshall --
   --------------

   procedure Marshall
     (R      : in     GIOP_1_2_CDR_Representation;
      Buffer : access Buffers.Buffer_Type;
      Data   : in     PolyORB.Types.Wide_String;
      Error  : in out Exceptions.Error_Container)
   is
      pragma Unreferenced (R);
      pragma Unreferenced (Error);

   begin
      Marshall (Buffer, Data);
   end Marshall;

   ----------------
   -- Unmarshall --
   ----------------

   procedure Unmarshall
     (R      : in     GIOP_1_2_CDR_Representation;
      Buffer : access Buffers.Buffer_Type;
      Data   :    out PolyORB.Types.Wchar;
      Error  : in out Exceptions.Error_Container)
   is
      pragma Unreferenced (R);
      pragma Unreferenced (Error);

   begin
      Data := Unmarshall (Buffer);
   end Unmarshall;

   ----------------
   -- Unmarshall --
   ----------------

   procedure Unmarshall
     (R      : in     GIOP_1_2_CDR_Representation;
      Buffer : access Buffers.Buffer_Type;
      Data   :    out PolyORB.Types.Wide_String;
      Error  : in out Exceptions.Error_Container)
   is
      pragma Unreferenced (R);
      pragma Unreferenced (Error);

   begin
      Data := Unmarshall (Buffer);
   end Unmarshall;

end PolyORB.Representations.CDR.GIOP_1_2;
