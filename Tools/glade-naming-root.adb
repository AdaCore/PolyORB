------------------------------------------------------------------------------
--                                                                          --
--                              GLADE TOOLS                                 --
--                                                                          --
--                    G L A D E . N A M I N G . R O O T                     --
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

with GLADE.Naming; use GLADE.Naming;

package body GLADE.Naming.Root is

   This : Naming_Context_Ptr := new Naming_Context;

   function Ref
     return Naming_Context_Ref
   is
      Ref : Naming_Context_Ref := This.all'Access;
      N   : Name;
      Seq : Name_Component_Sequence (1 .. 1);
   begin
      if This.Img = null then
         This.Img := new String'("");
         Set_Istring (Seq (1).Id, "..");
         Set_Name (N, Seq);
         Bind_Context (Ref, N, Ref);
         Set_Istring (Seq (1).Id, ".");
         Set_Name (N, Seq);
         Bind_Context (Ref, N, Ref);
         Set_Name (N, No_Name_Component_Sequence);
      end if;
      return Ref;
   end Ref;

end GLADE.Naming.Root;
