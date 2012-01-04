------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                 P O L Y O R B . S E T U P . T L S I O P                  --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2006-2012, Free Software Foundation, Inc.          --
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

with PolyORB.Initialization;
with PolyORB.Utils.Strings;

with PolyORB.GIOP_P.Tagged_Components.TLS_Sec_Trans;
pragma Warnings (Off, PolyORB.GIOP_P.Tagged_Components.TLS_Sec_Trans);

with PolyORB.GIOP_P.Transport_Mechanisms.TLS;
pragma Warnings (Off, PolyORB.GIOP_P.Transport_Mechanisms.TLS);

package body PolyORB.Setup.TLSIOP is

   procedure Initialize;

   procedure Initialize is
   begin
      null;
   end Initialize;

begin
   declare
      use PolyORB.Initialization;
      use PolyORB.Initialization.String_Lists;
      use PolyORB.Utils.Strings;

   begin
      Register_Module
        (Module_Info'
         (Name      =>
          +"polyorb.setup.tlsiop",
          Conflicts => PolyORB.Initialization.String_Lists.Empty,
          Depends   =>
          +"tagged_components.tls_sec_trans?"
          & "giop_p.transport_mechanisms.tls",
          Provides  => PolyORB.Initialization.String_Lists.Empty,
          Implicit  => False,
          Init      => Initialize'Access,
          Shutdown  => null));
   end;
end PolyORB.Setup.TLSIOP;
