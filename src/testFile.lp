enter Problem

Minimize
Obj:	100 absDiff8_1 + 100 absDiff9_1 + 100 absDiff10_1 + 100 absDiff1_1 + 100 absDiff2_1 + 100 absDiff3_1 + 100 absDiff4_1 + 77.7470846194774 absDiff5_1 + 61.311985226348 absDiff6_1 + 7.38636571913958 absDiff7_1 + 20 xb11_1 + 20 xb12_1 + 20 xb13_1 + 20 xb14_1 + 20 xb15_1 + 20 xb16_1 + 20 xb17_1 + 20 xb18_1 + 20 xb19_1 + 20 xb20_1 + 20 xb21_1 + 20 xb22_1 + 20 xb23_1 + 20 xb24_1 + 20 xb25_1 + 20 xb26_1 + 20 xb27_1 + 20 xb28_1 + 20 xb29_1 + 20 xb30_1
Subject To
c1:	xb8_1 - absDiff8_1 <= -1	 	
c2:	xb9_1 - absDiff9_1 <= -1	 	
c3:	xb10_1 - absDiff10_1 <= 1	 	
c4:	xb8_1 + absDiff8_1 >= -1	 	
c5:	xb9_1 + absDiff9_1 >= -1	 	
c6:	xb10_1 + absDiff10_1 >= 1	 	
c7:	xb31_1 - xb1_1 >= 0	 	
c8:	xb32_1 - xb1_1 >= 0	 	
c9:	xb33_1 - xb8_1 >= 0	 	
c10:	xb34_1 - xb9_1 >= 0	 	
c11:	xb35_1 - xb2_1 >= 0	 	
c12:	xb36_1 - xb10_1 >= 0	 	
c13:	xb37_1 + xb8_1 >= 0	 	
c14:	xb38_1 - xb3_1 >= 0	 	
c15:	xb39_1 + xb4_1 >= 0	 	
c16:	xb40_1 - xb5_1 >= 0	 	
c17:	xb41_1 - xb6_1 >= 0	 	
c18:	xb42_1 + xb7_1 >= 0	 	
c19:	xb43_1 + xb1_1 >= 0	 	
c20:	xb44_1 + xb1_1 >= 0	 	
c21:	xb45_1 + xb8_1 >= 0	 	
c22:	xb46_1 + xb9_1 >= 0	 	
c23:	xb47_1 + xb2_1 >= 0	 	
c24:	xb48_1 + xb10_1 >= 0	 	
c25:	xb49_1 - xb8_1 >= 0	 	
c26:	xb50_1 + xb3_1 >= 0	 	
c27:	xb51_1 - xb4_1 >= 0	 	
c28:	xb52_1 + xb5_1 >= 0	 	
c29:	xb53_1 + xb6_1 >= 0	 	
c30:	xb54_1 - xb7_1 >= 0	 	
c31:	xb31_1 + xb43_1 <= 1	 	
c32:	xb32_1 + xb44_1 <= 1	 	
c33:	xb33_1 + xb45_1 <= 1	 	
c34:	xb34_1 + xb46_1 <= 1	 	
c35:	xb35_1 + xb47_1 <= 1	 	
c36:	xb36_1 + xb48_1 <= 1	 	
c37:	xb37_1 + xb49_1 <= 1	 	
c38:	xb38_1 + xb50_1 <= 1	 	
c39:	xb39_1 + xb51_1 <= 1	 	
c40:	xb40_1 + xb52_1 <= 1	 	
c41:	xb41_1 + xb53_1 <= 1	 	
c42:	xb42_1 + xb54_1 <= 1	 	
c43:	xb31_1 - xb1_1 - xb43_1 <= 0	 	
c44:	xb32_1 - xb1_1 - xb44_1 <= 0	 	
c45:	xb33_1 - xb8_1 - xb45_1 <= 0	 	
c46:	xb34_1 - xb9_1 - xb46_1 <= 0	 	
c47:	xb35_1 - xb2_1 - xb47_1 <= 0	 	
c48:	xb36_1 - xb10_1 - xb48_1 <= 0	 	
c49:	xb37_1 + xb8_1 - xb49_1 >= 0	 	
c50:	xb38_1 - xb3_1 - xb50_1 <= 0	 	
c51:	xb39_1 + xb4_1 - xb51_1 >= 0	 	
c52:	xb40_1 - xb5_1 - xb52_1 <= 0	 	
c53:	xb41_1 - xb6_1 - xb53_1 <= 0	 	
c54:	xb42_1 + xb7_1 - xb54_1 >= 0	 	
c55:	xb43_1 + xb1_1 - xb31_1 <= 0	 	
c56:	xb44_1 + xb1_1 - xb32_1 <= 0	 	
c57:	xb45_1 + xb8_1 - xb33_1 <= 0	 	
c58:	xb46_1 + xb9_1 - xb34_1 <= 0	 	
c59:	xb47_1 + xb2_1 - xb35_1 <= 0	 	
c60:	xb48_1 + xb10_1 - xb36_1 <= 0	 	
c61:	xb49_1 - xb8_1 - xb37_1 >= 0	 	
c62:	xb50_1 + xb3_1 - xb38_1 <= 0	 	
c63:	xb51_1 - xb4_1 - xb39_1 >= 0	 	
c64:	xb52_1 + xb5_1 - xb40_1 <= 0	 	
c65:	xb53_1 + xb6_1 - xb41_1 <= 0	 	
c66:	xb54_1 - xb7_1 - xb42_1 >= 0	 	
c67:	xb13_1 <= 0	 	
c68:	xb15_1 <= 0	 	
c69:	xb16_1 <= 0	 	
c70:	xb17_1 <= 0	 	
c71:	xb19_1 - xb31_1 <= 0	 	
c72:	xb18_1 - xb32_1 - xb38_1 <= 0	 	
c73:	xb12_1 - xb33_1 - xb42_1 <= 0	 	
c74:	xb20_1 - xb34_1 - xb35_1 - xb39_1 <= 0	 	
c75:	xb11_1 - xb36_1 - xb37_1 - xb41_1 <= 0	 	
c76:	xb14_1 - xb40_1 <= 0	 	
c77:	xb23_1 <= 0	 	
c78:	xb25_1 <= 0	 	
c79:	xb26_1 <= 0	 	
c80:	xb27_1 <= 0	 	
c81:	xb29_1 - xb43_1 <= 0	 	
c82:	xb28_1 - xb44_1 - xb50_1 <= 0	 	
c83:	xb22_1 - xb45_1 - xb54_1 <= 0	 	
c84:	xb30_1 - xb46_1 - xb47_1 - xb51_1 <= 0	 	
c85:	xb21_1 - xb48_1 - xb49_1 - xb53_1 <= 0	 	
c86:	xb24_1 - xb52_1 <= 0	 	
c87:	xb11_1 - xb21_1 + B_a_1 - xb1_1 = 0	 	
c88:	xb12_1 - xb22_1 + B_d_1 - xb2_1 = 0	 	
c89:	xb13_1 - xb23_1 + B_f_1 - xb3_1 = 0	 	
c90:	xb14_1 - xb24_1 + B_h_1 - xb4_1 = 0	 	
c91:	xb15_1 - xb25_1 + B_g_1 - xb5_1 = 0	 	
c92:	xb16_1 - xb26_1 + B_j_1 - xb6_1 = 0	 	
c93:	xb17_1 - xb27_1 + B_y_1 - xb7_1 = 0	 	
c94:	xb18_1 - xb28_1 + B_b_1 - xb8_1 = 0	 	
c95:	xb19_1 - xb29_1 + B_c_1 - xb9_1 = 0	 	
c96:	xb20_1 - xb30_1 + B_e_1 - xb10_1 = 0	 	
c97:	B_f_1 = 0	 	
c98:	B_h_1 = 0	 	
c99:	B_g_1 = 0	 	
c100:	B_j_1 = 0	 	
c101:	B_y_1 = 0	 	
c102:	B_b_1 = 0	 	
c103:	B_c_1 = 0	 	
c104:	B_e_1 = 0	 	
c105:	xb1_1 = -1	 	
c107:	xb3_1 - B_f_1 = 0	 	
c108:	xb5_1 - B_g_1 = 0	 	
c109:	xb6_1 - B_j_1 = 0	 	
c110:	xb7_1 - B_y_1 = 0	 	
c111:	101 xb31_1 + dist_a - dist_c <= 100	 	
c112:	101 xb32_1 + dist_a - dist_b <= 100	 	
c113:	101 xb34_1 + dist_c - dist_e <= 100	 	
c114:	101 xb35_1 + dist_d - dist_e <= 100	 	
c115:	101 xb38_1 + dist_f - dist_b <= 100	 	
c116:	101 xb39_1 + dist_h - dist_e <= 100	 	
c117:	101 xb40_1 + dist_g - dist_h <= 100	 	
c118:	101 xb43_1 + dist_a - dist_c <= 100	 	
c119:	101 xb44_1 + dist_a - dist_b <= 100	 	
c120:	101 xb46_1 + dist_c - dist_e <= 100	 	
c121:	101 xb47_1 + dist_d - dist_e <= 100	 	
c122:	101 xb50_1 + dist_f - dist_b <= 100	 	
c123:	101 xb51_1 + dist_h - dist_e <= 100	 	
c124:	101 xb52_1 + dist_g - dist_h <= 100	 	
Bounds
	-1 <= xb1_1 <= 1
	-1 <= xb2_1 <= 1
	-1 <= xb3_1 <= 1
	-1 <= xb4_1 <= 1
	-1 <= xb5_1 <= 1
	-1 <= xb6_1 <= 1
	-1 <= xb7_1 <= 1
	-1 <= xb8_1 <= 1
	-1 <= xb9_1 <= 1
	-1 <= xb10_1 <= 1
	0 <= xb11_1 <= 1
	0 <= xb12_1 <= 1
	0 <= xb13_1 <= 1
	0 <= xb14_1 <= 1
	0 <= xb15_1 <= 1
	0 <= xb16_1 <= 1
	0 <= xb17_1 <= 1
	0 <= xb18_1 <= 1
	0 <= xb19_1 <= 1
	0 <= xb20_1 <= 1
	0 <= xb21_1 <= 1
	0 <= xb22_1 <= 1
	0 <= xb23_1 <= 1
	0 <= xb24_1 <= 1
	0 <= xb25_1 <= 1
	0 <= xb26_1 <= 1
	0 <= xb27_1 <= 1
	0 <= xb28_1 <= 1
	0 <= xb29_1 <= 1
	0 <= xb30_1 <= 1
	0 <= xb31_1 <= 1
	0 <= xb32_1 <= 1
	0 <= xb33_1 <= 1
	0 <= xb34_1 <= 1
	0 <= xb35_1 <= 1
	0 <= xb36_1 <= 1
	0 <= xb37_1 <= 1
	0 <= xb38_1 <= 1
	0 <= xb39_1 <= 1
	0 <= xb40_1 <= 1
	0 <= xb41_1 <= 1
	0 <= xb42_1 <= 1
	0 <= xb43_1 <= 1
	0 <= xb44_1 <= 1
	0 <= xb45_1 <= 1
	0 <= xb46_1 <= 1
	0 <= xb47_1 <= 1
	0 <= xb48_1 <= 1
	0 <= xb49_1 <= 1
	0 <= xb50_1 <= 1
	0 <= xb51_1 <= 1
	0 <= xb52_1 <= 1
	0 <= xb53_1 <= 1
	0 <= xb54_1 <= 1
	-1 <= B_a_1 <= 1
	-1 <= B_d_1 <= 1
	-1 <= B_f_1 <= 1
	-1 <= B_h_1 <= 1
	-1 <= B_g_1 <= 1
	-1 <= B_j_1 <= 1
	-1 <= B_y_1 <= 1
	-1 <= B_b_1 <= 1
	-1 <= B_c_1 <= 1
	-1 <= B_e_1 <= 1
	0 <= dist_a <= 100
	0 <= dist_d <= 100
	0 <= dist_f <= 100
	0 <= dist_h <= 100
	0 <= dist_g <= 100
	0 <= dist_j <= 100
	0 <= dist_y <= 100
	0 <= dist_b <= 100
	0 <= dist_c <= 100
	0 <= dist_e <= 100
	0 <= absDiff8_1 <= 2
	0 <= absDiff9_1 <= 2
	0 <= absDiff10_1 <= 2
	0 <= absDiff1_1 <= 2
	0 <= absDiff2_1 <= 2
	0 <= absDiff3_1 <= 2
	0 <= absDiff4_1 <= 2
	0 <= absDiff5_1 <= 2
	0 <= absDiff6_1 <= 2
	0 <= absDiff7_1 <= 2
	0 <= dist_a <= 100
	0 <= dist_d <= 100
	0 <= dist_f <= 100
	0 <= dist_h <= 100
	0 <= dist_g <= 100
	0 <= dist_j <= 100
	0 <= dist_y <= 100
	0 <= dist_b <= 100
	0 <= dist_c <= 100
	0 <= dist_e <= 100
Binaries
	xb11_1
	xb12_1
	xb13_1
	xb14_1
	xb15_1
	xb16_1
	xb17_1
	xb18_1
	xb19_1
	xb20_1
	xb21_1
	xb22_1
	xb23_1
	xb24_1
	xb25_1
	xb26_1
	xb27_1
	xb28_1
	xb29_1
	xb30_1
	xb31_1
	xb32_1
	xb33_1
	xb34_1
	xb35_1
	xb36_1
	xb37_1
	xb38_1
	xb39_1
	xb40_1
	xb41_1
	xb42_1
	xb43_1
	xb44_1
	xb45_1
	xb46_1
	xb47_1
	xb48_1
	xb49_1
	xb50_1
	xb51_1
	xb52_1
	xb53_1
	xb54_1
Generals
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
	B_a_1
	B_d_1
	B_f_1
	B_h_1
	B_g_1
	B_j_1
	B_y_1
	B_b_1
	B_c_1
	B_e_1
	absDiff8_1
	absDiff9_1
	absDiff10_1
	absDiff1_1
	absDiff2_1
	absDiff3_1
	absDiff4_1
	absDiff5_1
	absDiff6_1
	absDiff7_1
End
