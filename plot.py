import numpy as np
import matplotlib.pyplot as plt

filename="rot_psi500"
T = np.genfromtxt(filename+".txt", dtype=None)

T=T/10**6
x=np.arange(-1000,1020,20)
cp=plt.contour(x,x,T)
plt.xlim(-1000,1000)
plt.ylim(-1000,1000)
plt.clabel(cp, inline=True,fontsize=8)
plt.grid(True)
plt.title("Streamfunction (x10"+'\u2076'+" m"+'\u00b2'+"/s)")
plt.savefig('/media/lucifer/31ada994-e3b3-4ea2-b4b4-8764dc68ea0d/fortran/NWP LAB/L9fig/'+filename+" .png")
plt.show()
