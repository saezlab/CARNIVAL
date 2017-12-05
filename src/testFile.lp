enter Problem

Minimize
Obj:	1 absDiff3_1 + 0.01 xb4_1 + 0.01 xb5_1 + 0.01 xb6_1 + 0.01 xb7_1 + 0.01 xb8_1 + 0.01 xb9_1
Subject To
c1:	xb3_1 - absDiff3_1 <= 1	 	
c2:	xb3_1 + absDiff3_1 >= 1	 	
c3:	xb10_1 - xb1_1 >= 0	 	
c4:	xb11_1 - xb2_1 >= 0	 	
c5:	xb12_1 - xb3_1 >= 0	 	
c6:	xb13_1 + xb1_1 >= 0	 	
c7:	xb14_1 + xb2_1 >= 0	 	
c8:	xb15_1 + xb3_1 >= 0	 	
c9:	xb10_1 + xb13_1 <= 1	 	
c10:	xb11_1 + xb14_1 <= 1	 	
c11:	xb12_1 + xb15_1 <= 1	 	
c12:	xb10_1 - xb1_1 - xb13_1 <= 0	 	
c13:	xb11_1 - xb2_1 - xb14_1 <= 0	 	
c14:	xb12_1 - xb3_1 - xb15_1 <= 0	 	
c15:	xb13_1 + xb1_1 - xb10_1 <= 0	 	
c16:	xb14_1 + xb2_1 - xb11_1 <= 0	 	
c17:	xb15_1 + xb3_1 - xb12_1 <= 0	 	
c18:	xb5_1 - xb10_1 <= 0	 	
c19:	xb6_1 - xb11_1 <= 0	 	
c20:	xb4_1 - xb12_1 <= 0	 	
c21:	xb8_1 - xb13_1 <= 0	 	
c22:	xb9_1 - xb14_1 <= 0	 	
c23:	xb7_1 - xb15_1 <= 0	 	
c24:	xb4_1 - xb7_1 - xb1_1 = 0	 	
c25:	xb5_1 - xb8_1 - xb2_1 = 0	 	
c26:	xb6_1 - xb9_1 - xb3_1 = 0	 	
c27:	100 xb11_1 + dist_B_A - dist_C_A <= 99	 	
c28:	100 xb14_1 + dist_B_A - dist_C_A <= 99	 	
c29:	dist_B_A <= 100	 	
c30:	dist_C_A <= 100	 	
Bounds
	-1 <= xb1_1 <= 1
	-1 <= xb2_1 <= 1
	-1 <= xb3_1 <= 1
	0 <= xb4_1 <= 1
	0 <= xb5_1 <= 1
	0 <= xb6_1 <= 1
	0 <= xb7_1 <= 1
	0 <= xb8_1 <= 1
	0 <= xb9_1 <= 1
	0 <= xb10_1 <= 1
	0 <= xb11_1 <= 1
	0 <= xb12_1 <= 1
	0 <= xb13_1 <= 1
	0 <= xb14_1 <= 1
	0 <= xb15_1 <= 1
Integers
xb1_1
xb2_1
xb3_1
xb4_1
xb5_1
xb6_1
xb7_1
xb8_1
xb9_1
xb10_1
xb11_1
xb12_1
xb13_1
xb14_1
xb15_1
dist_A_A
dist_B_A
dist_C_A
End
