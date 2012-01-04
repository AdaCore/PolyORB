------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--             C O S T I M E . T I M E S E R V I C E . I M P L              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2001-2012, Free Software Foundation, Inc.          --
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

with CosTime.TIO;
with TimeBase;
with CosTime.UTO;
with PortableServer;

package CosTime.TimeService.Impl is

   type Object is
     new PortableServer.Servant_Base with null record;

   function universal_time
     (Self : access Object)
     return CosTime.UTO.Ref;

   function secure_universal_time
     (Self : access Object)
     return CosTime.UTO.Ref;

   function new_universal_time
     (Self : access Object;
      time : TimeBase.TimeT;
      inaccuracy : TimeBase.InaccuracyT;
      tdf : TimeBase.TdfT)
     return CosTime.UTO.Ref;

   function uto_from_utc
     (Self : access Object;
      utc : TimeBase.UtcT)
     return CosTime.UTO.Ref;

   function new_interval
     (Self : access Object;
      lower : TimeBase.TimeT;
      upper : TimeBase.TimeT)
     return CosTime.TIO.Ref;

end CosTime.TimeService.Impl;
