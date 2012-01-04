------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                              B A C K E N D                               --
--                                                                          --
--                                 S p e c                                  --
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
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

with Types; use Types;

package Backend is

   procedure Set_Current_Language (Language : String);
   --  Set the current language.

   function Current_Language return String;
   --  Return language previously set. Must not be called until after
   --  Backend.Config.Initialize.

   type Generate_Procedure is access procedure (IDL_Spec : Node_Id);
   procedure Generate (IDL_Spec : Node_Id);
   --  Generate code for the current language.

   type Usage_Procedure is access procedure (Indent : Natural);

   procedure Register_Language
     (Generate  : Generate_Procedure;
      Usage     : Usage_Procedure;
      Language  : String;
      Comments  : String);
   --  Register a new language with its code generation procedure, its
   --  name and a comment associated to it (for usage output).

   function Is_Valid_Language (L : String) return Boolean;
   --  Return True when there is a backend corresponding to L

   procedure Write_Languages (L, C : Natural);
   --  For each language backend available write at column L the name
   --  and at column C the comments associated to a language.

end Backend;
