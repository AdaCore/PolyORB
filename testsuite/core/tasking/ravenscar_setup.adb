with System;

with PolyORB.Profiles.Ravenscar;
pragma Elaborate_All (PolyORB.Profiles.Ravenscar);
pragma Warnings (Off, PolyORB.Profiles.Ravenscar);

package body Ravenscar_Setup is

   package Ravenscar_Profile_Instance is
      new PolyORB.Profiles.Ravenscar
     (Number_Of_Application_Tasks => 4,
      Number_Of_System_Tasks      => 20,
      Number_Of_Conditions        => 1_000,
      Number_Of_Mutexes           => 1_000,
      Task_Priority               => System.Default_Priority);

end Ravenscar_Setup;
