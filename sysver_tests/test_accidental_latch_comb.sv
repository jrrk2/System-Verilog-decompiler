// Test 6: Accidental latch (incomplete if in always_comb) - SHOULD FAIL
module test_accidental_latch_comb(
  input  logic sel,
  input  logic [7:0] a,
  output logic [7:0] y
);
  always_comb begin
    if (sel)
      y = a;
    // Missing else - creates unintended latch!
  end
endmodule
