// Test 5: Multiple latches in one block
module test_multi_latch(
  input  logic en,
  input  logic d1,
  input  logic d2,
  output logic q1,
  output logic q2
);
  always_latch begin
    if (en) begin
      q1 = d1;
      q2 = d2;
    end
  end
endmodule
