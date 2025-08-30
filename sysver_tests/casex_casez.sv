module casex_casez_test(input logic [3:0] sel, output logic y1, y2);
    always_comb begin
        casex(sel)
            4'b1x0x : y1 = 1;
            default : y1 = 0;
        endcase
        casez(sel)
            4'b10?? : y2 = 1;
            default : y2 = 0;
        endcase
    end
endmodule
