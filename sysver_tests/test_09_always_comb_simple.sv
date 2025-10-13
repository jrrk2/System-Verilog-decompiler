
module test_comb(
  input logic a,
  input logic b,
  output logic y
);
  always_comb
    y = a & b;
endmodule
