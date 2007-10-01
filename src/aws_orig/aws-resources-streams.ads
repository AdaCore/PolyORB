------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                A W S . R E S O U R C E S . S T R E A M S                 --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2002-2006, Free Software Foundation, Inc.          --
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
