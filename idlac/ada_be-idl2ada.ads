------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--                       A D A _ B E . I D L 2 A D A                        --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1999-2000 ENST Paris University, France.          --
--                                                                          --
-- AdaBroker is free software; you  can  redistribute  it and/or modify it  --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. AdaBroker  is distributed  in the hope that it will be  useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with AdaBroker; see file COPYING. If  --
-- not, write to the Free Software Foundation, 59 Temple Place - Suite 330, --
-- Boston, MA 02111-1307, USA.                                              --
--                                                                          --
--             AdaBroker is maintained by ENST Paris University.            --
--                     (email: broker@inf.enst.fr)                          --
--                                                                          --
------------------------------------------------------------------------------

--  This package contains one function per node of the parse tree
with Idl_Fe.Types; use Idl_Fe.Types;

package Ada_Be.Idl2Ada is

   procedure Generate
     (Node      : in Node_Id;
      Implement : Boolean := False;
      To_Stdout : Boolean := False);
   --  Generate the Ada mapping of the IDL tree
   --  rooted at Node.
   --  If Implement is true, produce only a template
   --  for the Impl package of each interface, to
   --  be completed by the user.
   --  If To_Stdout is true, all produced source code
   --  is emitted on standard output (e. g. for use
   --  with GNATCHOP).

end Ada_Be.Idl2Ada;
