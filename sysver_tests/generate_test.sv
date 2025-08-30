module generate_test #(parameter N=4) (output logic [N-1:0] y);
    genvar i;
    generate
        for (i=0; i<N; i++) begin: genblk
            assign y[i] = i%2;
        end
    endgenerate
endmodule
