module case_inside_test(input logic [3:0] sel, output logic flag);
    always_comb begin
        case (sel) inside
            4'b1??? : flag = 1;
            4'b01?? : flag = 0;
            default : flag = 'x;
        endcase
    end
endmodule
