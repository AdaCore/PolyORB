with CORBA.Policy_Values; use CORBA.Policy_Values;
with Droopi.Objects;      use Droopi.Objects;

package CORBA.Policy.Thread_Policy is

   type Thread_Policy is new Policy with private;

private
   type Thread_Policy is new Policy with
      record
         Value : ThreadPolicyValue;
      end record;

end CORBA.Policy.Thread_Policy;
