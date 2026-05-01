(* Minimal Gravity.mod for testing *)

M$ClassesDescription = {
  S[1] -> {
    ClassName -> phi,
    SelfConjugate -> True,
    Mass -> {Mphi, 0},
    Width -> 0
  },

  T[1] -> {
    ClassName -> h,
    SelfConjugate -> True,
    Mass -> {Mh, 0},
    Width -> 0
  }
};

M$CouplingMatrices = {
  C[T[1], S[1], S[1]] -> {1},     (* TSS *)
  C[T[1], T[1], S[1], S[1]] -> {1} (* TTSS *)
};

