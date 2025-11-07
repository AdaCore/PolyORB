------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--          P O L Y O R B . S E C U R I T Y . I D E N T I T I E S           --
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

with PolyORB.Log;
with PolyORB.Utils.Chained_Lists;

package body PolyORB.Security.Identities is

   use PolyORB.Log;
   use PolyORB.Security.Types;

   package L is new PolyORB.Log.Facility_Log
     ("polyorb.security.identity_tokens");
   procedure O (Message : String; Level : Log_Level := Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
     renames L.Enabled;

   type Registry_Item is record
      Kind        : PolyORB.Security.Types.Identity_Token_Type;
      Constructor : Empty_Identity_Constructor;
   end record;

   package Registry_Item_Lists is
     new PolyORB.Utils.Chained_Lists (Registry_Item);

   Registry : Registry_Item_Lists.List;

   ------------
   -- Create --
   ------------

   procedure Create
     (Kind  :        PolyORB.Security.Types.Identity_Token_Type;
      Item  :        Ada.Streams.Stream_Element_Array;
      Token :    out Identity_Access;
      Error : in out PolyORB.Errors.Error_Container)
   is
      use Registry_Item_Lists;

   begin
      if Kind = ITT_Absent then
         Token := null;

      else
         declare
            Iter : Iterator := First (Registry);

         begin
            while not Last (Iter) loop
               if Value (Iter).Kind = Kind then
                  Token := Value (Iter).Constructor.all;
                  Decode (Token, Item, Error);

                  return;
               end if;

               Next (Iter);
            end loop;
         end;
      end if;

      Token := null;
   end Create;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Item : in out Identity_Access) is

      procedure Free is
        new PolyORB.Utils.Unchecked_Deallocation.Free


        (Object => Identity_Type'Class,


         Name   => Identity_Access);

   begin
      if Item /= null then
         Release_Contents (Item);
         Free (Item);
      end if;
   end Destroy;

   --------------
   -- Register --
   --------------

   procedure Register
     (Kind        : PolyORB.Security.Types.Identity_Token_Type;
      Constructor : Empty_Identity_Constructor)
   is
   begin
      pragma Debug
        (C, O ("Register identity token type:"
         & Identity_Token_Type'Image (Kind)));
      Registry_Item_Lists.Append (Registry, (Kind, Constructor));
   end Register;

end PolyORB.Security.Identities;
