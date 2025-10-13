module loop(input mask[7:0], output logic cnt[3:0]);

  always @(mask)
    begin
       cnt = 0;
       for (int i = 0; i < 8; i++) cnt = cnt + mask[i];
    end

endmodule
   
