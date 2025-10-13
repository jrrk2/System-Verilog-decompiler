
// Test 4: D-latch with active-low enable
module test_latch_active_low(
  input  logic en_n,
  input  logic d,
  output logic q
);
  always_latch begin
    if (!en_n)
      q = d;
  end
endmodule
