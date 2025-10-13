
// Test 8: Latch in sequential block - SHOULD FAIL
module test_latch_in_sequential(
  input  logic clk,
  input  logic en,
  input  logic d,
  output logic q
);
  always_ff @(posedge clk) begin
    if (en)
      q <= d;
    // Missing else in sequential - this should be DFF with enable, not latch!
  end
endmodule
