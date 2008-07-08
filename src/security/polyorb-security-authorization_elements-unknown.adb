------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--             POLYORB.SECURITY.AUTHORIZATION_ELEMENTS.UNKNOWN              --
--                                                                          --
--                                 B o d y                                  --
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

with Ada.Unchecked_Deallocation;

package body PolyORB.Security.Authorization_Elements.Unknown is

   ------------
   -- Create --
   ------------

   function Create
     (The_Type : Element_Type;
      Contents : Ada.Streams.Stream_Element_Array)
      return Authorization_Element_Access
   is
   begin
      return
        new Unknown.Unknown_Authorization_Element_Type'
        (The_Type, new Ada.Streams.Stream_Element_Array'(Contents));
   end Create;

   ------------
   -- Encode --
   ------------

   function Encode
     (Self : access Unknown_Authorization_Element_Type)
      return Ada.Streams.Stream_Element_Array
   is
   begin
      return Self.The_Data.all;
   end Encode;

   ------------------------------------
   -- Get_Authorization_Element_Type --
   ------------------------------------

   function Get_Authorization_Element_Type
     (Self : access Unknown_Authorization_Element_Type)
      return Element_Type
   is
   begin
      return Self.The_Type;
   end Get_Authorization_Element_Type;

   ---------------
   -- Is_Holder --
   ---------------

   function Is_Holder
     (Self     : access Unknown_Authorization_Element_Type;
      Identity :        PolyORB.Security.Identities.Identity_Access)
      return Boolean
   is
      pragma Unreferenced (Self);
      pragma Unreferenced (Identity);

   begin
      return False;
   end Is_Holder;

   ----------------------
   -- Release_Contents --
   ----------------------

   procedure Release_Contents
     (Self : access Unknown_Authorization_Element_Type)
   is

      procedure Free is
        new Ada.Unchecked_Deallocation
        (Ada.Streams.Stream_Element_Array,
         PolyORB.Security.Types.Stream_Element_Array_Access);

   begin
      Free (Self.The_Data);
   end Release_Contents;

end PolyORB.Security.Authorization_Elements.Unknown;
