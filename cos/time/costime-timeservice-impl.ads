------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--             C O S T I M E . T I M E S E R V I C E . I M P L              --
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
      time : in TimeBase.TimeT;
      inaccuracy : in TimeBase.InaccuracyT;
      tdf : in TimeBase.TdfT)
     return CosTime.UTO.Ref;

   function uto_from_utc
     (Self : access Object;
      utc : in TimeBase.UtcT)
     return CosTime.UTO.Ref;

   function new_interval
     (Self : access Object;
      lower : in TimeBase.TimeT;
      upper : in TimeBase.TimeT)
     return CosTime.TIO.Ref;

end CosTime.TimeService.Impl;
