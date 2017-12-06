=== Ex1 - Case3 - NetNeg - R ===

enter Problem

Minimize
Obj:	1 absDiff2_1 + 0.01 xb3_1 + 0.01 xb4_1 + 0.01 xb5_1 + 0.01 xb6_1
Subject To
c1:	xb2_1 - absDiff2_1 <= -1	 	
c2:	xb2_1 + absDiff2_1 >= -1	 	
c3:	xb7_1 + xb1_1 >= 0	 	
c4:	xb8_1 - xb1_1 >= 0	 	
c5:	xb7_1 + xb8_1 <= 1	 	
c6:	xb7_1 + xb1_1 - xb8_1 >= 0	 	
c7:	xb8_1 - xb1_1 - xb7_1 >= 0	 	
c8:	xb4_1 - xb7_1 <= 0	 	
c9:	xb6_1 - xb8_1 <= 0	 	
c10:	xb3_1 - xb5_1 - xb1_1 = -1	 	
c11:	xb4_1 - xb6_1 - xb2_1 = 0	 	
c12:	xb1_1 = 1	 	
c13:	dist_TF_P <= 100	 	
Bounds
	-1 <= xb1_1 <= 1
	-1 <= xb2_1 <= 1
	0 <= xb3_1 <= 1
	0 <= xb4_1 <= 1
	0 <= xb5_1 <= 1
	0 <= xb6_1 <= 1
	0 <= xb7_1 <= 1
	0 <= xb8_1 <= 1
Integers
xb1_1
xb2_1
xb3_1
xb4_1
xb5_1
xb6_1
xb7_1
xb8_1
dist_P_P
dist_TF_P
End
