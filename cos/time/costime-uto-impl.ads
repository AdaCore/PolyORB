------------------------------------------------------------------------------
--                                                                          --
--                           ADABROKER SERVICES                             --
--                                                                          --
--                     C O S T I M E . U T O . I M P L                      --
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
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
--             AdaBroker is maintained by ENST Paris University.            --
--                     (email: broker@inf.enst.fr)                          --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Calendar;
with TimeBase;
with PortableServer;

package CosTime.UTO.Impl is

   type Object is new PortableServer.Servant_Base with record
      Time       : TimeBase.TimeT := 0;
      Inaccuracy : TimeBase.InaccuracyT := 0;
      Tdf        : Timebase.TdfT := 0;
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
      comparison_type : in ComparisonType;
      uto : in Ref)
     return TimeComparison;

   function time_to_interval
     (Self : access Object;
      uto : in Ref)
     return TIO_Forward.Ref;

   function interval
     (Self : access Object)
     return TIO_Forward.Ref;

end CosTime.UTO.Impl;
