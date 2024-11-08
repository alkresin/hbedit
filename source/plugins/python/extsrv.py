
# Ext
# A set of routines to launch an external application and keep connection with it - server side
#
# Copyright 2024 Alexander S.Kresin <alex@kresin.ru>
# www - http://www.kresin.ru

import json
import os
import sys
import time
import inspect
import traceback

# Constants
GUIS_VERSION = "1.4"
PROTOCOL_VER = "1.1"
BUFFLEN = 2048

# Global variables
cVersion = "1.0"
nClientId = 2
nServerId = 1
nInterval = 20
cn = "\n"
cLogFile = "extserver.log"
mparent = None

def esrv_Init( aparams ):
    global mparent
    nLogOn = 0
    cDir = None
    cFileRoot = "gs"

    for value in aparams:
        if value[:3] == "dir":
            cDir = value[4:]
            if cDir[0] == '"':
                cDir = cDir[1:-1]
        elif value[:3] == "log":
            nLogOn = int(value[4:])
        elif value[:4] == "file":
            cFileRoot = value[5:]

    if not cDir:
        cDir = cDir if cDir else os.path.join(os.environ['TMP'], '')
        #cDir = os.path.dirname(os.path.realpath(__file__)) + '/'
    if not cDir.endswith(os.path.sep):
        cDir += os.path.sep

    h = {
        "log": nLogOn,
        "dir": cDir,
        "end": False,
        "cb": None
    }

    conn_SetVersion(GUIS_VERSION)
    gWritelog(h, "Connect via files {}{}.*".format(cDir, cFileRoot) )
    sRes = conn_Server_Connect(h, "{}{}".format(cDir, cFileRoot))
    if not sRes:
        return None

    caller_frame = inspect.stack()[1]
    mparent = inspect.getmodule(caller_frame[0])

    SendIn(h, '"ok"')
    return h

def esrv_LogLevel(h, nLogLevel):
    n = h["log"]
    if isinstance(nLogLevel, int):
        h["log"] = nLogLevel
    return n

def esrv_Wait(h):
    while not h["end"]:
        time.sleep(nInterval/1000)
        conn_CheckIn(h)
    gWritelog(h, "esrv_Wait: exit")
    return None

def esrv_RunProc(h, cFunc, aParams):
    SendOut(h, json.dumps(("runproc", cFunc, json.dumps(aParams))))

    return None

def Parse(h, arr):
    cCommand = arr[0].lower()
    c = cCommand[0]
    arrp = []
    lErr = False
    cRes = ""
    cFunc = ""
    xRes = ""

    gWritelog(h, "Command: {}".format(cCommand))

    if c == "s":
        if cCommand == "setvar":
            lErr = len(arr) < 3
            if not lErr:
                SendIn(h, '"Ok"')
                cRes = arr[2].upper()
                if cRes not in globals():
                    globals()[cRes] = None
                globals()[cRes] = arr[3]
    elif c == "g":
        if cCommand == "getver":
            lErr = len(arr) < 2
            if not lErr:
                SendIn(h, json.dumps(gVersion(arr[1])))
        elif cCommand == "getvar":
            lErr = len(arr) < 2
            if not lErr:
                cRes = arr[1].upper()
                if cRes in globals():
                    cRes = globals()[cRes]
                else:
                    cRes = None
                SendIn(h, json.dumps(cRes))
    elif c == "e":
        if cCommand == "exit" or cCommand == "endapp":
            h["end"] = True
            SendIn(h, '"Ok"')
    elif c == "r":
        if cCommand == "runproc":
            cFunc = arr[1].lower()
            SendIn(h, '"Ok"')
            try:
                getattr( mparent, cFunc )(json.loads(arr[2]) )
            except:
                gWritelog(h, traceback.format_exc())
        elif cCommand == "runfunc":
            cFunc = arr[1].lower()
            try:
                #xRes = eval(f"{mparent}.{cFunc}(json.loads(arr[2]))")
                xRes = getattr( mparent, cFunc )(json.loads(arr[2]) )
                SendIn(h, json.dumps(xRes))
            except:
                gWritelog(h, traceback.format_exc())
                lErr = True
    else:
        lErr = True

    if lErr:
        SendIn(h, '"Err"')

    return not lErr

def MainHandler(h):
    cBuffer = conn_GetRecvBuffer(h)
    gWritelog(h, cBuffer)

    arr = json.loads(cBuffer)
    if not isinstance(arr, list) or not arr:
        SendIn(h, '"Wrong"')
        return None

    if not Parse(h, arr):
        gWritelog(h, "Parsing error")

    return None

def SendOut(h, s):
    gWritelog(h, " " + s)
    cRes = conn_Send2SocketOut(h, "+" + s + cn)
    return cRes if cRes else ""

def SendIn(h, s):
    conn_Send2SocketIn(h, "+" + s + cn)
    return None

def gVersion(n):
    return GUIS_VERSION if n == 0 else "hbExtServer {}".format(GUIS_VERSION)

def gWritelog(h, s):
    if h["log"] > 0:
        cFile = h["dir"] + cLogFile
        with open(cFile, "a") as f:
            f.write(s + cn)
    return None

# ---------------------------
def conn_SetCallBack(h, b):
    h["cb"] = b
    return None

def conn_SetVersion(s):
    global cVersion
    cVersion = s
    return None

def conn_Read(h, lOut):
    n = 0
    nPos = 0
    s = ""
    han = h["hout"] if lOut else h["hin"]
    #cBuffer = " " * BUFFLEN

    while True:
        cBuffer = os.read(han, BUFFLEN)
        if len(cBuffer) == 0:
            break
        #cBuffer = cBuffer[:n]
        if (nPos := cBuffer.find( 10 ))!= -1:
            s += cBuffer[:nPos].decode("utf-8")
            break
        s += cBuffer.decode("utf-8")

    h["bufres"] = s

    return len(s)

def conn_GetRecvBuffer(h):
    return h["bufres"][1:]

def conn_Send(h, lOut, cLine):
    han = h["hout"] if lOut else h["hin"]

    if h["active"]:
        os.lseek(han, 1, os.SEEK_SET)
        os.write(han, cLine.encode())
        os.lseek(han, 0, os.SEEK_SET)
        os.write(han, chr(nServerId).encode())

    return None

def conn_Send2SocketIn(h, s):
    if h["active"]:
        conn_Send(h, False, s)
    return None

def conn_Send2SocketOut(h, s, lNoWait=False):
    cAns = None

    if h["active"]:
        conn_Send(h, True, s)
        if not lNoWait:
            while h["active"]:
                conn_CheckIn(h)
                if (cAns := conn_CheckOut(h)):
                    return cAns
                if h["cb"]:
                    h["cb"]()
                time.sleep(nInterval  / 1000)

    return None

def conn_CheckIn(h):
    hIn = h["hin"]
    bufIn = " " * 10

    if h["active"]:
        os.lseek(hIn, 0, os.SEEK_SET)
        if os.read(hIn, 1) == chr(nClientId).encode():
            gWritelog(h, "Checkin")
            if conn_Read(h, False) > 0:
                MainHandler(h)
            return True
    return False

def conn_CheckOut(h):
    hOut = h["hout"]
    bufOut = " " * 10

    if h["active"]:
        os.lseek(hOut, 0, os.SEEK_SET)
        if os.read(hOut, 1) == chr(nClientId).encode():
            conn_Read(h, True)
            gWritelog(h, "Checkout: " + conn_GetRecvBuffer(h))
            return conn_GetRecvBuffer(h)
    return None

def conn_Server_Connect(h, cFile):

    handlOut = os.open(cFile + ".gs1", os.O_RDWR | os.O_CREAT)
    gWritelog(h, "Open out {}.gs1 {}".format(cFile,handlOut))

    handlIn = os.open(cFile + ".gs2", os.O_RDWR | os.O_CREAT)
    gWritelog(h, "Open in {}.gs2 {}".format(cFile,handlIn))

    h["active"] = handlIn >= 0 and handlOut >= 0
    h["hin"] = handlIn
    h["hout"] = handlOut

    if h["active"] and conn_Read(h, True) > 0:
        return conn_GetRecvBuffer(h)
    return None

def conn_Exit(h):
    h["active"] = False
    os.close(h["hin"])
    os.close(h["hout"])