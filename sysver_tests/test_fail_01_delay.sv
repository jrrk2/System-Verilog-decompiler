
module test_delay(
  input logic clk,
  input logic d,
  output logic q
);
  always @(posedge clk)
    #10 q <= d;
endmodule
