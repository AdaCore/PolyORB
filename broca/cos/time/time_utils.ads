with CosTime;
with Timebase;

package Time_Utils is

   function "+"
     (A : TimeBase.TimeT; B : TimeBase.InaccuracyT)
     return TimeBase.TimeT;

   function "-"
     (A : TimeBase.TimeT; B : TimeBase.InaccuracyT)
     return TimeBase.TimeT;

   function "+"
     (A : TimeBase.TimeT; B : TimeBase.TdfT)
     return TimeBase.TimeT;

   function "-"
     (A : TimeBase.TimeT; B : TimeBase.TdfT)
     return TimeBase.TimeT;

   function Compare
     (A : TimeBase.TimeT; B : TimeBase.TimeT)
     return CosTime.TimeComparison;

   function Current_Time return TimeBase.TimeT;
   function Current_Inaccuracy return TimeBase.InaccuracyT;
   function Current_Tdf return TimeBase.TdfT;

end Time_Utils;
