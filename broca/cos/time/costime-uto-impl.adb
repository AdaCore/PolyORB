------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--                     C O S T I M E . U T O . I M P L                      --
--                                                                          --
--                                 B o d y                                  --
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

with Broca.Basic_Startup;
with CosTime.TIO.Helper;
with CosTime.TIO.Impl;
with CosTime.UTO.Helper;
with CosTime.UTO.Skel;
with Interfaces.C;
with PortableServer;
with System;
with TimeBase;           use TimeBase;
with Time_Utils;         use Time_Utils;

package body CosTime.UTO.Impl is

   type UTO_Ptr is access Object;
   type TIO_Ptr is access CosTime.TIO.Impl.Object;

   -------------------
   -- absolute_time --
   -------------------

   function absolute_time
     (Self : access Object)
     return Ref
   is
      Result : constant UTO_Ptr := new Object;
      R      : CORBA.Object.Ref;
   begin
      Result.Time := Self.Time + Current_Time;
      Result.Inaccuracy := Self.Inaccuracy + Current_Inaccuracy;
      Result.Tdf := Self.Tdf + Current_Tdf;
      Broca.Basic_Startup.Initiate_Servant
        (PortableServer.Servant (Result), R);
      return Helper.To_Ref (R);
   end absolute_time;

   ------------------
   -- compare_time --
   ------------------

   function compare_time
     (Self : access Object;
      comparison_type : in ComparisonType;
      uto : in Ref)
     return TimeComparison
   is
      Other_Time : constant TimeT := get_time (uto);
      Other_Tdf  : constant TdfT  := get_tdf (uto);
   begin
      if Comparison_Type = MidC then
         return Compare (Self.Time + Self.Tdf, Other_Time + Other_Tdf);
      else
         declare
            Other_Inaccuracy : constant InaccuracyT := get_inaccuracy (uto);
            Comp_Low         : constant TimeComparison :=
              Compare (Self.Time - Self.Inaccuracy + Self.Tdf,
                       Other_Time - Other_Inaccuracy + Other_Tdf);
            Comp_High        : constant TimeComparison :=
              Compare (Self.Time + Self.Inaccuracy + Self.Tdf,
                       Other_Time + Other_Inaccuracy + Other_Tdf);
         begin
            if Comp_Low = Comp_High then
               return Comp_Low;
            else
               return TCIndeterminate;
            end if;
         end;
      end if;
   end compare_time;

   --------------------
   -- get_inaccuracy --
   --------------------

   function get_inaccuracy
     (Self : access Object)
     return InaccuracyT
   is
   begin
      return Self.Inaccuracy;
   end get_inaccuracy;

   -------------
   -- get_tdf --
   -------------

   function get_tdf
     (Self : access Object)
     return TdfT
   is
   begin
      return Self.Tdf;
   end get_tdf;

   --------------
   -- get_time --
   --------------

   function get_time
     (Self : access Object)
     return TimeT
   is
   begin
      return Self.Time;
   end get_time;

   ------------------
   -- get_utc_time --
   ------------------

   function get_utc_time
     (Self : access Object)
     return UtcT
   is
   begin
      return (time    => Self.Time,
              inacclo => CORBA.Unsigned_Long (Self.Inaccuracy rem (2 ** 32)),
              inacchi => CORBA.Unsigned_Short (Self.Inaccuracy / (2 ** 32)),
              tdf     => Self.Tdf);
   end get_utc_time;

   --------------
   -- interval --
   --------------

   function interval
     (Self : access Object)
     return TIO_Forward.Ref
   is
      Result : constant TIO_Ptr := new CosTime.TIO.Impl.Object;
      R      : CORBA.Object.Ref;
   begin
      Result.Interval.Lower_Bound := Self.Time - Self.Tdf;
      Result.Interval.Upper_Bound := Self.Time + Self.Tdf;
      Broca.Basic_Startup.Initiate_Servant
        (PortableServer.Servant (Result), R);
      return CosTime.TIO.Convert_Forward.To_Forward
        (CosTime.TIO.Helper.To_Ref (R));
   end interval;

   ----------------------
   -- time_to_interval --
   ----------------------

   function time_to_interval
     (Self : access Object;
      uto : in Ref)
     return TIO_Forward.Ref
   is
      Other_Time : constant TimeT   := get_time (uto);
      Result     : constant TIO_Ptr := new CosTime.TIO.Impl.Object;
      R          : CORBA.Object.Ref;
   begin
      Result.Interval.Lower_Bound := TimeT'Min (Self.Time, Other_Time);
      Result.Interval.Upper_Bound := TimeT'Max (Self.Time, Other_Time);
      Broca.Basic_Startup.Initiate_Servant
        (PortableServer.Servant (Result), R);
      return CosTime.TIO.Convert_Forward.To_Forward
        (CosTime.TIO.Helper.To_Ref (R));
   end time_to_interval;

end CosTime.UTO.Impl;
