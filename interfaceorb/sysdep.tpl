with Interfaces.C;

package AdaBroker.Sysdep is

   pragma Linker_Options ("-ladabroker");
   @STDCPP@pragma Linker_Options ("-lstdc++");
   pragma Linker_Options ("-lomniORB2");
   pragma Linker_Options ("-lomniDynamic2");
   pragma Linker_Options ("-lomnithread");
   pragma Linker_Options ("-lpthread");
   pragma Linker_Options ("-lposix4");
   @SOCKET@pragma Linker_Options ("-lsocket");
   @SOCKET@pragma Linker_Options ("-lnsl");
   pragma Linker_Options ("-ltcpwrapGK");


   type Bool is new @BOOL@;

   True  : Bool := Bool (1);
   False : Bool := Bool (0);
   --  Definition of Bool, the Ada equivalent of the C++ bool
   --  type. Needed to interface C functions in Ada It is supposed here
   --  that the C internal representation of type boolean need one
   --  Bytes. If it is not the case, an error will occur when compiling and
   --  executing Ada_sys_Dep.cc and you will never come to this file.

   function To_Boolean (B : Bool)    return Boolean;
   function To_Bool    (B : Boolean) return Bool;
   --  Conversion of Bool into Boolean and vice versa

end AdaBroker.Sysdep;
