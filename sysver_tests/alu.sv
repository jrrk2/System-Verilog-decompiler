module alu #(parameter WIDTH=8) (
    input  logic [WIDTH-1:0] a, b,
    input  logic [3:0] op,
    output logic [WIDTH-1:0] y
);
    always_comb begin
        case(op)
            4'd0: y = a + b;
            4'd1: y = a - b;
            4'd2: y = a * b;
            4'd3: y = a / b;
`ifdef INCLUDE_BITWISE
            4'd4: y = a & b;
            4'd5: y = a | b;
`endif
`ifdef INCLUDE_SHIFT
            4'd6: y = a << b;
            4'd7: y = a >> b;
            4'd8: y = a >>> b;
            4'd9: y = a <<< b;
`endif
`ifdef INCLUDE_POW
            4'd10: y = a ** b;
`endif
            default: y = '0;
        endcase
    end
endmodule
