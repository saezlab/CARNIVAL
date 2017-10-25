enter Problem

Minimize
obj:	1 absDiff2 + 1 absDiff4 + 0.01 xb5 + 0.01 xb6 + 0.01 xb7 + 0.01 xb8 + 0.01 xb9 + 0.01 xb10 + 0.01 xb11 + 0.01 xb12
Subject To
c1:	xb2 - absDiff2 <= 1	 	
c2:	xb4 - absDiff4 <= -1	 	
c3:	xb2 + absDiff2 >= 1	 	
c4:	xb4 + absDiff4 >= -1	 	
c5:	xb15 + xb1 >= 0	 	
c6:	xb16 + xb3 >= 0	 	
c7:	xb15 + xb1 >= 0	 	
c8:	xb16 + xb3 >= 0	 	
c9:	xb13 + xb15 <= 1	 	
c10:	xb14 + xb16 <= 1	 	
c11:	xb13 - xb1 - xb15 <= 0	 	
c12:	xb14 - xb3 - xb16 <= 0	 	
c13:	xb15 + xb1 - xb13 <= 0	 	
c14:	xb16 + xb3 - xb14 <= 0	 	
c15:	xb6 - xb13 <= 0	 	
c16:	xb8 - xb14 <= 0	 	
c17:	xb10 - xb15 <= 0	 	
c18:	xb12 - xb16 <= 0	 	
c19:	xb5 - xb9 - xb1 = -1	 	
c20:	xb6 - xb10 - xb2 = 0	 	
c21:	xb7 - xb11 - xb3 = 1	 	
c22:	xb8 - xb12 - xb4 = 0	 	
Bounds
	-1 <= xb1 <= 1
	-1 <= xb2 <= 1
	-1 <= xb3 <= 1
	-1 <= xb4 <= 1
	0 <= xb5 <= 1
	0 <= xb6 <= 1
	0 <= xb7 <= 1
	0 <= xb8 <= 1
	0 <= xb9 <= 1
	0 <= xb10 <= 1
	0 <= xb11 <= 1
	0 <= xb12 <= 1
	0 <= xb13 <= 1
	0 <= xb14 <= 1
	0 <= xb15 <= 1
	0 <= xb16 <= 1
Integers
xb1
xb2
xb3
xb4
xb5
xb6
xb7
xb8
xb9
xb10
xb11
xb12
xb13
xb14
xb15
xb16
End
