// Test 2: Multi-bit latch
module test_latch_multibit(
  input  logic en,
  input  logic [7:0] data_in,
  output logic [7:0] data_out
);
  always_latch begin
    if (en)
      data_out = data_in;
  end
endmodule
