------------------------------------------------------------------------------
--                                                                          --
--                        ADABROKER COMPONENTS                              --
--                                                                          --
--             A D A B R O K E R . O M N I R O P E A N D K E Y              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.2 $
--                                                                          --
--         Copyright (C) 1999-2000 ENST Paris University, France.           --
--                                                                          --
-- AdaBroker is free software; you  can  redistribute  it and/or modify it  --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. AdaBroker  is distributed  in the hope that it will be  useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with AdaBroker; see file COPYING. If  --
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
--             AdaBroker is maintained by ENST Paris University.            --
--                     (email: broker@inf.enst.fr)                          --
--                                                                          --
------------------------------------------------------------------------------

package body AdaBroker.OmniRopeAndKey is

   ----------------
   -- C_Get_Rope --
   ----------------

   function C_Get_Rope (Self : in Object'Class) return System.Address;
   pragma Import (CPP, C_Get_Rope, "rope__18Ada_OmniRopeAndKey");
   --  Wrapper around Ada_OmniRopeAndKey function rope (see
   --  Ada_OmniRopeAndKey.hh) called by the Ada equivalent : Get_Rope

   --------------
   -- Get_Rope --
   --------------

   function Get_Rope
     (Self : in Object'Class)
      return Rope.Object is
   begin
      --  Just calls the C function
      return Rope.Object (C_Get_Rope (Self));
   end Get_Rope;

   ----------------
   -- C_Key_Size --
   ----------------

   function C_Key_Size
     (Self : in Object'Class)
      return Interfaces.C.unsigned_long;

   pragma Import (CPP, C_Key_Size, "keysize__18Ada_OmniRopeAndKey");
   --  Wrapper around Ada_OmniRopeAndKey function keysize (see
   --  Ada_OmniRopeAndKey.hh) called by the Ada equivalent : Key_Size

   --------------
   -- Key_Size --
   --------------

   function Key_Size
     (Self : in Object'Class)
      return CORBA.Unsigned_Long
   is
      C_Result : Interfaces.C.unsigned_long;
   begin
      --  Call the C function ...
      C_Result := C_Key_Size (Self);

      --  Transforms the result in Ada type
      return CORBA.Unsigned_Long (C_Result);
   end Key_Size;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Self : in out Controlled_Wrapper) is
   begin
      Init (Self.Real);
   end Initialize;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (Self : in out Controlled_Wrapper) is
   begin
      Free (Self.Real);
   end Finalize;

end AdaBroker.OmniRopeAndKey;
