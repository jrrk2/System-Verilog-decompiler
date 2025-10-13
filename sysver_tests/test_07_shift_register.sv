
module test_shiftreg(
  input logic clk,
  input logic d,
  output logic [7:0] q
);
  always_ff @(posedge clk) begin
    q <= {q[6:0], d};
  end
endmodule
