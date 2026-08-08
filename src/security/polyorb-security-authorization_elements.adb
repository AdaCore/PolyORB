------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                 POLYORB.SECURITY.AUTHORIZATION_ELEMENTS                  --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2005-2012, Free Software Foundation, Inc.          --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.                                               --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

with PolyORB.Utils.Unchecked_Deallocation;

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
        new PolyORB.Utils.Unchecked_Deallocation.Free


        (Object => Authorization_Element_Type'Class,


         Name   => Authorization_Element_Access);

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
