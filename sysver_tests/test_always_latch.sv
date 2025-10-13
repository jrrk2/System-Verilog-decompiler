// ============================================================================
// test_latch.sv - Test cases for always_latch blocks
// ============================================================================

// Test 1: Intentional transparent latch with always_latch
module test_always_latch(
  input  logic en,
  input  logic d,
  output logic q
);
  always_latch begin
    if (en)
      q = d;
  end
endmodule
