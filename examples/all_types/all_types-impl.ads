with Omniobject ;

package all_types.Impl is

   type Object is new Omniobject.Implemented_Object with private ;
   type Object_Ptr is access all Object ;


   function echoBoolean(Self : access Object; arg : in Corba.Boolean) return Corba.Boolean ;

   function echoShort(Self : access Object; arg : in Corba.Short) return Corba.Short ;

   function echoLong(Self : access Object; arg : in Corba.Long) return Corba.Long ;

   function echoUShort(Self : access Object; arg : in Corba.Unsigned_Short) return Corba.Unsigned_Short ;

   function echoULong(Self : access Object; arg : in Corba.Unsigned_Long) return Corba.Unsigned_Long ;

   function echoFloat(Self : access Object; arg : in Corba.Float) return Corba.Float ;

   function echoDouble(Self : access Object; arg : in Corba.Double) return Corba.Double ;

   function echoChar(Self : access Object; arg : in Corba.Char) return Corba.Char ;

   function echoOctet(Self : access Object; arg : in Corba.Octet) return Corba.Octet ;

   function echoString(Self : access Object; arg : in Corba.String) return Corba.String ;

   procedure simple_exception_test(Self : access Object ) ;
   procedure complexe_exception_test(Self : access Object ) ;
   function echo1(Self : access Object; arg : in example) return example ;

   function echo2(Self : access Object; arg : in simple_struct) return simple_struct ;

   function echo3(Self : access Object; arg : in Color) return Color ;

   function echo4(Self : access Object; arg : in U_string) return U_string ;

   function echo6(Self : access Object; arg : in U_sequence) return U_sequence ;

   function echo7(Self : access Object; arg : in B_sequence) return B_sequence ;

   function Get_R_attribute(Self : access Object) return Color ;

   function Get_N_attribute(Self : access Object) return example ;

   procedure Set_N_attribute(Self : access Object ;
                             To : in example ) ;

private

   -- You may add fields to this record
   type Object is new Omniobject.Implemented_Object with record
      Pd_Col : Color := Blue ;
      Pd_Ex : Example := (Switch => 1, Counter => 23) ;
   end record ;

   --------------------------------------------------
   ----          finalization operators          ----
   --------------------------------------------------
   procedure Initialize(Self : in out Object) ;
   procedure Adjust(Self : in out Object) ;
   procedure Finalize(Self : in out Object) ;

end all_types.Impl ;

