------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--                         I D L _ F E . D E B U G                          --
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

--  This is a debugging package for AdaBroker.
--
--  with Adabroker.Debug;
--  pragma Elaborate(Adabroker.Debug);
--
--  Flag : constant Natural := Adabroker.Debug.Is_Active ("specific_name");
--  procedure O is new AdaBroker.Debug.Output (Flag);
--
--  and then :
--
--  pragma Debug (O ("debugging info"));
--
--  The output will be done if "adabroker.deb" file contains
--  a line with "specific_name"

package Idl_Fe.Debug is

   function Is_Active (Flag : in String) return Natural;
   --  returns 0 when not active

   generic
      Flag : Natural;
   procedure Output (Message : in String);
   --  Prints Message on standard output when Flag is not 0

end Idl_Fe.Debug;
