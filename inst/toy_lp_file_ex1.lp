Minimize
Obj:	 1 aD1 + 1 aD2 + 0.2 nU1 + 0.2 nU2 + 0.2 nU3 + 0.2 nU4 + 0.2 nU5 + 0.2 nU6 + 0.2 nD1 + 0.2 nD2 + 0.2 nD3 + 0.2 nD4 + 0.2 nD5 + 0.2 nD6
Subject To
c1:	n5 - aD1 <= 1	 	
c2:	n6 - aD2 <= 1	 	
c3:	n5 + aD1 >= 1	 	
c4:	n6 + aD2 >= 1	 	
c5:	eU1 - n1 >= 0	 	
c6:	eU2 - n2 >= 0	 	
c7:	eU3 - n2 >= 0	 	
c8:	eU5 - n4 >= 0	 	
c9:	eU6 - n4 >= 0	 	
c10:	eU4 + n3 >= 0	 	
c11:	eD1 + n1 >= 0	 	
c12:	eD2 + n2 >= 0	 	
c13:	eD3 + n2 >= 0	 	
c14:	eD5 + n4 >= 0	 	
c15:	eD6 + n4 >= 0	 	
c16:	eD4 - n3 >= 0	 	
c17:	eU1 + eD1 <= 1	 	
c18:	eU2 + eD2 <= 1	 	
c19:	eU3 + eD3 <= 1	 	
c20:	eU4 + eD4 <= 1	 	
c21:	eU5 + eD5 <= 1	 	
c22:	eU6 + eD6 <= 1	 	
c23:	eU1 - n1 - eD1 <= 0	 	
c24:	eU2 - n2 - eD2 <= 0	 	
c25:	eU3 - n2 - eD3 <= 0	 	
c26:	eU5 - n4 - eD5 <= 0	 	
c27:	eU6 - n4 - eD6 <= 0	 	
c28:	eU4 + n3 - eD4 <= 0	 	
c29:	eD1 + n1 - eU1 <= 0	 	
c30:	eD2 + n2 - eU2 <= 0	 	
c31:	eD3 + n2 - eU3 <= 0	 	
c32:	eD5 + n4 - eU5 <= 0	 	
c33:	eD6 + n4 - eU6 <= 0	 	
c34:	eD4 - n3 - eU4 <= 0	 	
c35:	nU1 <= 0	 	
c36:	nU3 <= 0	 	
c37:	nU5 - eU2 - eU6 <= 0	 	
c38:	nU6 - eU3 - eU5 <= 0	 	
c39:	nU2 - eU1 <= 0	 	
c40:	nU4 - eU4 <= 0	 	
c41:	nD1 <= 0	 	
c42:	nD3 <= 0	 	
c43:	nD5 - eD2 - eD6 <= 0	 	
c44:	nD6 - eD3 - eD5 <= 0	 	
c45:	nD2 - eD1 <= 0	 	
c46:	nD4 - eD4 <= 0	 	
c47:	nU1 - nD1 + nAc1 - n1 = 0	 	
c48:	nU2 - nD2 + nAc2 - n2 = 0	 	
c49:	nU3 - nD3 + nAc3 - n3 = 0	 	
c50:	nU4 - nD4 + nAc4 - n4 = 0	 	
c51:	nU5 - nD5 + nAc5 - n5 = 0	 	
c52:	nU6 - nD6 + nAc6 - n6 = 0	 	
c53:	nAc2 = 0	 	
c54:	nAc4 = 0	 	
c55:	nAc5 = 0	 	
c56:	nAc6 = 0	 	
c57:	n1 = 1	 	
c58:	n3 = 1	 	
c59:	n1 - nAc1 = 0	 	
c60:	n3 - nAc3 = 0	 	
c61:	101 eU2 + nDs2 - nDs5 <= 100	 	
c62:	101 eU6 + nDs4 - nDs5 <= 100	 	
c63:	101 eU3 + nDs2 - nDs6 <= 100	 	
c64:	101 eU5 + nDs4 - nDs6 <= 100	 	
c65:	101 eU1 + nDs1 - nDs2 <= 100	 	
c66:	101 eU4 + nDs3 - nDs4 <= 100	 	
c67:	101 eD2 + nDs2 - nDs5 <= 100	 	
c68:	101 eD6 + nDs4 - nDs5 <= 100	 	
c69:	101 eD3 + nDs2 - nDs6 <= 100	 	
c70:	101 eD5 + nDs4 - nDs6 <= 100	 	
c71:	101 eD1 + nDs1 - nDs2 <= 100	 	
c72:	101 eD4 + nDs3 - nDs4 <= 100	 	
Bounds
	-1 <= n1 <= 1
	-1 <= n2 <= 1
	-1 <= n3 <= 1
	-1 <= n4 <= 1
	-1 <= n5 <= 1
	-1 <= n6 <= 1
	0 <= nU1 <= 1
	0 <= nU2 <= 1
	0 <= nU3 <= 1
	0 <= nU4 <= 1
	0 <= nU5 <= 1
	0 <= nU6 <= 1
	0 <= nD1 <= 1
	0 <= nD2 <= 1
	0 <= nD3 <= 1
	0 <= nD4 <= 1
	0 <= nD5 <= 1
	0 <= nD6 <= 1
	0 <= eU1 <= 1
	0 <= eU2 <= 1
	0 <= eU3 <= 1
	0 <= eU4 <= 1
	0 <= eU5 <= 1
	0 <= eU6 <= 1
	0 <= eD1 <= 1
	0 <= eD2 <= 1
	0 <= eD3 <= 1
	0 <= eD4 <= 1
	0 <= eD5 <= 1
	0 <= eD6 <= 1
	0 <= aD1 <= 2
	0 <= aD2 <= 2
	-1 <= nAc1 <= 1
	-1 <= nAc2 <= 1
	-1 <= nAc3 <= 1
	-1 <= nAc4 <= 1
	-1 <= nAc5 <= 1
	-1 <= nAc6 <= 1
	0 <= nDs1 <= 100
	0 <= nDs2 <= 100
	0 <= nDs3 <= 100
	0 <= nDs4 <= 100
	0 <= nDs5 <= 100
	0 <= nDs6 <= 100
Binaries
nU1
nU2
nU3
nU4
nU5
nU6
nD1
nD2
nD3
nD4
nD5
nD6
eU1
eU2
eU3
eU4
eU5
eU6
eD1
eD2
eD3
eD4
eD5
eD6
Generals
n1
n2
n3
n4
n5
n6
nAc1
nAc2
nAc3
nAc4
nAc5
nAc6
End
