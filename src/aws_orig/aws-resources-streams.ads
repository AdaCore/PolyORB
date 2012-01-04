------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                A W S . R E S O U R C E S . S T R E A M S                 --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2002-2012, Free Software Foundation, Inc.          --
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

package AWS.Resources.Streams is

   use Ada;

   type Stream_Type is abstract tagged limited private;

   type Stream_Access is access all Stream_Type'Class;

   function End_Of_File
     (Resource : Stream_Type)
      return Boolean
      is abstract;

   procedure Read
     (Resource : in out Stream_Type;
      Buffer   :    out Stream_Element_Array;
      Last     :    out Stream_Element_Offset)
      is abstract;

   procedure Close (File : in out Stream_Type)
      is abstract;

   procedure Create
     (File   :    out File_Type;
      Buffer : Stream_Access);
   pragma Inline (Create);
   --  Create the resource from user defined resource.

private

   type Stream_Type is abstract new Resources.File_Tagged with null record;

end AWS.Resources.Streams;
