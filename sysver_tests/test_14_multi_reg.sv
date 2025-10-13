
module test_multi_reg(
  input logic clk,
  input logic [7:0] a,
  input logic [7:0] b,
  output logic [7:0] q1,
  output logic [7:0] q2
);
  always_ff @(posedge clk) begin
    q1 <= a;
    q2 <= b;
  end
endmodule
