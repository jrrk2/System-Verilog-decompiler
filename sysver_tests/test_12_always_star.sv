
module test_star(
  input logic a,
  input logic b,
  input logic c,
  output logic y
);
  always @* begin
    y = (a & b) | c;
  end
endmodule
