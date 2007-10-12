------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                 POLYORB.SECURITY.AUTHORIZATION_ELEMENTS                  --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2005-2006, Free Software Foundation, Inc.          --
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

with PolyORB.Security.Authorization_Elements.Unknown;

package body PolyORB.Security.Authorization_Elements is

   type Registry_Record is record
      The_Type    : Element_Type;
      Constructor : Element_Constructor;
   end record;

   package Registry_Lists is
     new PolyORB.Utils.Chained_Lists (Registry_Record);

   Registry : Registry_Lists.List;

   ------------
   -- Create --
   ------------

   function Create
     (The_Type : Element_Type;
      Contents : Ada.Streams.Stream_Element_Array)
      return Authorization_Element_Access
   is
      use Registry_Lists;

      Iter : Iterator := First (Registry);

   begin
      while not Last (Iter) loop
         if Value (Iter).all.The_Type = The_Type then
            return Value (Iter).all.Constructor (Contents);
         end if;

         Next (Iter);
      end loop;

      return Unknown.Create (The_Type, Contents);
   end Create;

   --------------
   -- Register --
   --------------

   procedure Register
     (The_Type    : Element_Type;
      Constructor : Element_Constructor)
   is
   begin
      Registry_Lists.Append (Registry, (The_Type, Constructor));
   end Register;

   ----------------------
   -- Release_Contents --
   ----------------------

   procedure Release_Contents
     (Item : in out Authorization_Element_Lists.List)
   is
      use Authorization_Element_Lists;

      procedure Free is
        new Ada.Unchecked_Deallocation
        (Authorization_Element_Type'Class, Authorization_Element_Access);

      Iter : Iterator := First (Item);

   begin
      while not Last (Iter) loop
         Release_Contents (Value (Iter).all);
         Free (Value (Iter).all);
         Next (Iter);
      end loop;

      Deallocate (Item);
   end Release_Contents;

end PolyORB.Security.Authorization_Elements;
