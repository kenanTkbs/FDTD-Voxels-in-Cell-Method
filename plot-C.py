from cycler import cycler
import numpy as np
import matplotlib.pyplot as plt

file1="outfiles-fine-"
file2="outfiles-VIC-"
leg="VIC"
figname="CC-VIC"

plt.style.use(['science','ieee','no-latex'])
plt.rcParams.update({'figure.dpi': '100'})

plt.rcParams['font.sans-serif'] = "Times New Roman"
plt.rcParams['font.family'] = "sans-serif"


plt.rcParams['axes.prop_cycle'] = (cycler('color', ["#000000", "#0000FF", "#00FF00", "#FF0000",
                                                    "#000000", "#0000FF", "#00FF00", "#FF0000"]) + \
                                   cycler('ls', ['-', '-', '-', '-','--', '--', '--', '--'])+\
                                   cycler('lw', [0.6, 0.6, 0.6, 0.6, 1.2, 1.2, 1.2, 1.2]))

S=[file1+'0-0/outE-596-636',\
   file1+'1-0/outE-596-636',\
   file1+'0-1/outE-596-636',\
   file1+'1-1/outE-596-636',\
   file2+'0-0/outE-298-318',\
   file2+'1-0/outE-298-318',\
   file2+'0-1/outE-298-318',\
   file2+'1-1/outE-298-318']

for index, fname in enumerate(S):
    data=np.loadtxt(fname)
    Y=data[:,1]
    X=data[:,2]
    plt.plot(X,Y)
        
plt.xlim((0,600))
plt.ylim((-10,220))
plt.ylabel('Electric Field $E_y$ (V/m)', labelpad=1)
plt.xlabel('Time Step (fine grid)', labelpad=1)
plt.text(60, 200, "$E_y$ at Node C of the Corner")
plt.legend(('Fine Grid S1', 'Fine Grid S2', 'Fine Grid S3','Fine Grid S4', \
            leg+' S1', leg+' S2', leg+' S3',leg+' S4'),
           loc='upper right', labelspacing=0, frameon=True)
plt.tick_params(
    axis='both',         # changes apply to the x-axis
    right=False,      # ticks along the bottom edge are off
    top=False,
     pad=1 ) # labels along the bottom edge are off
plt.minorticks_off()
#plt.tight_layout()
plt.savefig(figname+'.svg')
plt.show() #or

