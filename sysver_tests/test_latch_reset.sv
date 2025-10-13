
// Test 3: Latch with reset (transparent when not in reset)
module test_latch_reset(
  input  logic en,
  input  logic rst,
  input  logic d,
  output logic q
);
  always_latch begin
    if (rst)
      q = 1'b0;
    else if (en)
      q = d;
  end
endmodule
