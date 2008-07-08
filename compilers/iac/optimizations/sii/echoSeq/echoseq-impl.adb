
pragma Style_Checks (Off);

with echoSeq.Skel;
pragma Warnings (Off, echoSeq.Skel);

package body echoSeq.Impl is

   function echoUsequence
     (Self : access Object;
      arg : in U_sequence)
     return U_sequence
   is
      pragma Unreferenced (Self);
   begin
      return arg;
   end echoUsequence;

end echoSeq.Impl;
