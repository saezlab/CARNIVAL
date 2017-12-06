=== Ex2 - Case1 - NetPosPos - R ===

enter Problem

Minimize
Obj:	1 absDiff3_1 + 0.01 xb4_1 + 0.01 xb5_1 + 0.01 xb6_1 + 0.01 xb7_1 + 0.01 xb8_1 + 0.01 xb9_1
Subject To
c1:	xb3_1 - absDiff3_1 <= 1	 	
c2:	xb3_1 + absDiff3_1 >= 1	 	
c3:	xb10_1 - xb1_1 >= 0	 	
c4:	xb11_1 - xb2_1 >= 0	 	
c5:	xb12_1 + xb1_1 >= 0	 	
c6:	xb13_1 + xb2_1 >= 0	 	
c7:	xb10_1 + xb12_1 <= 1	 	
c8:	xb11_1 + xb13_1 <= 1	 	
c9:	xb10_1 - xb1_1 - xb12_1 <= 0	 	
c10:	xb11_1 - xb2_1 - xb13_1 <= 0	 	
c11:	xb12_1 + xb1_1 - xb10_1 <= 0	 	
c12:	xb13_1 + xb2_1 - xb11_1 <= 0	 	
c13:	xb5_1 - xb10_1 <= 0	 	
c14:	xb6_1 - xb11_1 <= 0	 	
c15:	xb8_1 - xb12_1 <= 0	 	
c16:	xb9_1 - xb13_1 <= 0	 	
c17:	xb4_1 - xb7_1 - xb1_1 = -1	 	
c18:	xb5_1 - xb8_1 - xb2_1 = 0	 	
c19:	xb6_1 - xb9_1 - xb3_1 = 0	 	
c20:	xb1_1 = 1	 	
c21:	100 xb11_1 + dist_TF_P - dist_G_P <= 99	 	
c22:	100 xb13_1 + dist_TF_P - dist_G_P <= 99	 	
c23:	dist_TF_P <= 100	 	
c24:	dist_G_P <= 100	 	
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
dist_P_P
dist_TF_P
dist_G_P
End
