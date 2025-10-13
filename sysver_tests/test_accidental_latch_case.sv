// Test 7: Accidental latch (incomplete case in always) - SHOULD FAIL
module test_accidental_latch_case(
  input  logic [1:0] sel,
  output logic [7:0] y
);
  always @(*) begin
    case (sel)
      2'b00: y = 8'h00;
      2'b01: y = 8'h11;
      // Missing 2'b10, 2'b11 - creates latch!
    endcase
  end
endmodule
