# -*- coding: utf-8 -*-
# Import traj coordinates from dat-file
#file "traj****.dat" should contain the following
#line0 z_last Ua2 E B0
#line1 x y z rho tag Bx By Bz
#line2 ... 
import numpy as np
import matplotlib.pyplot as plt

class traj():
    #define class for trajectories
    def __init__(self,B,Ipl,E,A2,x,y,z,tag):
        self.B = B
        self.Ipl = Ipl
        self.E = E
        self.A2 = A2
        self.x = x
        self.y = y
        self.z = z
        self.tag = tag
    
def import_trajectories():
    #numbers of trajects
    N_start = 9766
    N_stop = 9999
    # define list for trajs
    trajectories = []
    
    k = 0
    
    for i_No in range(N_start,N_stop+1):
        filename = 'C:\\Kurchatov_Inst\\T15MD.octupol_pristrel\\output\\' + \
                    'TRAJ' + str(i_No) + '.DAT'  
        print(filename)
        try:
            data = np.loadtxt(filename)
            k = k+1
            b = traj(data[0,0],data[0,1],data[0,2],data[0,3],
                     data[1:,0],data[1:,1],data[1:,2],data[1:,3])
            trajectories.append(b)
        except:
            print("TRAJ{} NOT FOUND".format(i_No))
            pass
    
    #    # delete all NaNs from traj
    #    mask = ~np.isnan(traj0.No)
    #    traj0 = traj0[mask]
    return trajectories
    print("\n*** {} trajectories imported ***".format(k))            
    print("\n***Work complete***")   

def plot_traj(traj0):
    #plotting imported trajectories
    filename = 'C:\Kurchatov_Inst\\T15_vessel.txt'
    camera = np.loadtxt(filename)
    filename = 'C:\Kurchatov_Inst\\T15_sep.txt'
    separatrix = np.loadtxt(filename)
    
    fig, (ax1, ax2) = plt.subplots(nrows=1, ncols=2)
    for i in range(len(traj0)):
        mask = traj0[i].tag == 1
        ax1.plot(traj0[i].x[mask],traj0[i].y[mask],color='k')
        ax2.plot(traj0[i].x[mask],traj0[i].z[mask],color='k')
    for i in range(len(traj0)):
        mask = traj0[i].tag == 2
        ax1.plot(traj0[i].x[mask],traj0[i].y[mask],color='r')
        ax2.plot(traj0[i].x[mask],traj0[i].z[mask],color='r')
        
    ax1.plot(camera[:,0],camera[:,1])
    ax1.plot(separatrix[:,0],separatrix[:,1])
    
    ax1.set_xlabel('X (mm)')
    ax1.set_ylabel('Y (mm)')
    ax2.set_xlabel('X (mm)')
    ax2.set_ylabel('Z (mm)')
    ax1.grid(True)
    ax2.grid(True)
#    ax1.axis('equal')
#    ax2.axis('equal')
    ax1.set_title('B = {} T, Ipl = {} MA'.format(traj0[0].B, traj0[0].Ipl))
    
if __name__ == '__main__':
    traj0 = import_trajectories()
    plot_traj(traj0)
