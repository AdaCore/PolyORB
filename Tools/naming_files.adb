------------------------------------------------------------------------------
--                                                                          --
--                              GLADE TOOLS                                 --
--                                                                          --
--                         N A M I N G _ F I L E S                          --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision$                             --
--                                                                          --
--         Copyright (C) 1996-2000 Free Software Foundation, Inc.           --
--                                                                          --
-- GLADE  is free software;  you can redistribute it and/or modify it under --
-- terms of the  GNU General Public License  as published by the Free Soft- --
-- ware Foundation;  either version 2,  or (at your option)  any later ver- --
-- sion.  GLADE  is distributed  in the hope that  it will be  useful,  but --
-- WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHANTABI- --
-- LITY or  FITNESS FOR A PARTICULAR PURPOSE.  See the  GNU General Public  --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License  distributed  with GLADE;  see file COPYING.  If  --
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
--               GLADE  is maintained by ACT Europe.                        --
--               (email: glade-report@act-europe.fr)                        --
--                                                                          --
------------------------------------------------------------------------------

with GLADE.Objects; use GLADE.Objects;
with GLADE.Naming;  use GLADE.Naming;

package body Naming_Files is

   type File_Ptr is access all File;

   --------------
   -- New_File --
   --------------

   function New_File
     return Object_Ref
   is
      Tmp : File_Ptr := new File;
   begin
      return Tmp.all'Access;
   end New_File;

   ---------------
   -- Get_Image --
   ---------------

   function Get_Image
     (Obj : access File)
     return String
   is
   begin
      return Get_Istring (Obj.Content);
   end Get_Image;

   ---------------
   -- Set_Image --
   ---------------

   procedure Set_Image
     (Obj : access File;
      Img : in String)
   is
   begin
      Set_Istring (Obj.Content, Img);
   end Set_Image;

end Naming_Files;
