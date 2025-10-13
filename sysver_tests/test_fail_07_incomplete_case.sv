
module test_incomplete_case(
  input logic [1:0] sel,
  output logic [7:0] y
);
  always_comb begin
    case (sel)
      2'b00: y = 8'h00;
      2'b01: y = 8'h11;
    endcase
  end
endmodule
