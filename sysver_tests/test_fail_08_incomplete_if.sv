
module test_incomplete_if(
  input logic sel,
  input logic [7:0] a,
  output logic [7:0] y
);
  always_comb begin
    if (sel)
      y = a;
  end
endmodule
