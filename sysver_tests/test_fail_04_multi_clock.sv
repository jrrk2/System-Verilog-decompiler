
module test_multiclk(
  input logic clk1,
  input logic clk2,
  input logic d,
  output logic q
);
  always @(posedge clk1 or posedge clk2)
    q <= d;
endmodule
