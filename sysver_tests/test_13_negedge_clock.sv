
module test_negedge(
  input logic clk,
  input logic d,
  output logic q
);
  always_ff @(negedge clk)
    q <= d;
endmodule
