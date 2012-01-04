------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                     C O S T I M E . U T O . I M P L                      --
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

with TimeBase;

with PortableServer;

package CosTime.UTO.Impl is

   type Object is new PortableServer.Servant_Base with record
      Time       : TimeBase.TimeT := 0;
      Inaccuracy : TimeBase.InaccuracyT := 0;
      Tdf        : TimeBase.TdfT := 0;
   end record;

   function get_time
     (Self : access Object)
     return TimeBase.TimeT;

   function get_inaccuracy
     (Self : access Object)
     return TimeBase.InaccuracyT;

   function get_tdf
     (Self : access Object)
     return TimeBase.TdfT;

   function get_utc_time
     (Self : access Object)
     return TimeBase.UtcT;

   function absolute_time
     (Self : access Object)
     return Ref'Class;

   function compare_time
     (Self : access Object;
      comparison_type : ComparisonType;
      uto : Ref)
     return TimeComparison;

   function time_to_interval
     (Self : access Object;
      uto : Ref)
     return TIO_Forward.Ref;

   function interval
     (Self : access Object)
     return TIO_Forward.Ref;

end CosTime.UTO.Impl;
