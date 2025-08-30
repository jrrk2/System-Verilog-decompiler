module child(input logic a, output logic y);
    assign y = ~a;
endmodule

module parent;
    logic a=0, y;
    child u1(.a(a), .y(y));
    initial begin
        #1 $display("y=%0b", y);
    end
endmodule
