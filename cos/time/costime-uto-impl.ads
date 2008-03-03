------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                     C O S T I M E . U T O . I M P L                      --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2001-2008, Free Software Foundation, Inc.          --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
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
     return Ref;

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
