
import os
import sys
sys.path.append('./')
#sys.path.append(os.path.abspath(os.path.join(os.path.dirname(sys.argv[0]), os.pardir)))
from extsrv import esrv_Init, esrv_Wait, gWritelog
cv = None
hExt = None
lCv2 = True
aExt = (".jpg",".png",".bmp",".tiff",".gif")

def fu1( aparams ):
    fname, fext = os.path.splitext( aparams[1] )
    if fext in aExt:
        img = cv.imread(cv.samples.findFile(aparams[0]+aparams[1]))
        if img is None:
            return "Could not read the image"

        cv.imshow("Display window", img)
        k = cv.waitKey(0)
    else:
        return "Format is not supported"

    return ":end;"

def initinfo( aparams ):
    if lCv2:
        return ":get(fu1) dir file"
    else:
        return "Can not import cv2"

def main( aparams ):

    global hExt, cv, lCv2

    hExt = esrv_Init(aparams)
    if hExt == None:
        return None

    gWritelog(hExt, "Init-Ok")
    try:
        import cv2 as cv
    except:
        gWritelog(hExt, "Can not import cv2")
        lCv2 = False
    #if lOk:
    esrv_Wait(hExt)

    return None

if __name__ == '__main__':
    main( sys.argv[1:] )