with CORBA.Policy_Values; use CORBA.Policy_Values;
with Droopi.Objects;      use Droopi.Objects;

package CORBA.Policy.Thread_Policy is

   type ThreadPolicy is new Policy with private;
   type ThreadPolicy_Access is access ThreadPolicy;

private
   type ThreadPolicy is new Policy with
      record
         Value       : ThreadPolicyValue;
      end record;

end CORBA.Policy.Thread_Policy;
