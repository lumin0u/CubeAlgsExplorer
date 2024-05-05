# ################## The code of the server socket which communicates with the client ##################################
# this is modified kociemba's code, see his github: https://github.com/hkociemba/RubiksCube-OptimalSolver

# requires https://pypi.org/project/RubikOptimal/1.0.0/

import socket
import sys
import time
from threading import Thread
import optimal.face as face
import optimal.cubie as cubie
import optimal.coord as coord
import optimal.pruning as pr

def input_fun(conn):
    data = []
    while ord('\n') not in data:
        try:
            a = conn.recv(1024).upper()
            if len(a) == 0:
                conn.close()
                print('Connection closed', flush=True)
                return
        except:
            print('Connection closed', flush=True)
            conn.close()
            return
        
        for b in a:
            data.append(b)
    
    return ''.join(chr(i) for i in data if chr(i) > chr(32))

def client_thread(conn):
    while True:
        in_str = input_fun(conn)

        cubestring = in_str

        fc = face.FaceCube()
        s = fc.from_string(cubestring)  # initialize fc
        if s != cubie.CUBE_OK:
            print(s)  # no valid cubestring, gives invalid facelet cube
        cc = fc.to_cubie_cube()
        s = cc.verify()
        if s != cubie.CUBE_OK:
            print(s)  # no valid facelet cube, gives invalid cubie cube

        coc = coord.CoordCube(cc)

        dist = max(coc.UD_phasex24_depth, coc.RL_phasex24_depth,
                coc.FB_phasex24_depth, pr.corner_depth[coc.corners]) # lower bound for distance to solved
        
        try:
            # replies with 10 when the cube is invalid
            reply = (str(dist) + "\n").encode()
            conn.sendall(reply)
        except Exception as e:
            print('Error while sending data. Connection closed', flush=True)
            conn.close()
            raise e


    print('Connection closed', flush=True)
    conn.close()

s = None
def server_start(port):
    global s
    s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    print('Server socket created')
    try:
        s.bind(('', port))  # bind socket to local host and port
    except socket.error as e:
        print('Server socket bind failed. Error Code : ' + str(e.errno))
        sys.exit()
    s.listen(10)
    print('Server now listening...')

    while 1:
        conn, addr = s.accept()
        print('Connected with ' + addr[0] + ':' + str(addr[1]) + ', ' + time.strftime("%Y.%m.%d  %H:%M:%S"))
        Thread(target=client_thread, args=(conn,)).start()
    s.close()

Thread(target=server_start, args=(8085,)).start()

while input("Press x to stop\n").upper() != "X":
    pass

s.shutdown(0)
