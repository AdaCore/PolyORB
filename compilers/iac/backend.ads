------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                              B A C K E N D                               --
--                                                                          --
--                                 S p e c                                  --
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

with Types; use Types;

package Backend is

   procedure Set_Current_Language (Language : String);
   --  Reset the current language.

   function Current_Language return String;
   --  Return language previously set. Null string when uninitialized.

   type Generate_Procedure is access procedure (IDL_Spec : Node_Id);
   procedure Generate (IDL_Spec : Node_Id);
   --  Generate code for the current language.

   type Usage_Procedure is access procedure (Indent : Natural);

   procedure Register
     (Generate  : Generate_Procedure;
      Usage     : Usage_Procedure;
      Language  : String;
      Comments  : String);
   --  Register a new language with its code generation procedure, its
   --  name and a comment associated to it (for usage output).
   --  The current language is set to this last language.

   function Is_Valid_Language (L : String) return Boolean;
   --  Return True when there is a backend corresponding to L

   procedure Write_Languages (L, C : Natural);
   --  For each language backend available write at column L the name
   --  and at column C the comments associated to a language.

end Backend;
