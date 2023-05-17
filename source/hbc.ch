/*
 *
 */

#ifdef __PLATFORM__UNIX
   #define  LASTROW  23
#else
   #define  LASTROW  24
#endif

#define SHIFT_PRESSED 0x010000
#define CTRL_PRESSED  0x020000
#define ALT_PRESSED   0x040000
#define KP_PRESSED    0x080000

#define HB_ZIP_OPEN_ADDINZIP   2

#define ADIR_POS      6
#define AZF_POS       6

#ifdef _USE_SSH2
#xtranslate hb_vfOpen([<n,...>])          => hbc_vfOpen(<n>)
#xtranslate hb_vfSize([<n,...>])          => hbc_vfSize(<n>)
#xtranslate hb_vfClose([<n,...>])         => hbc_vfClose(<n>)
#xtranslate hb_vfSeek([<n,...>])          => hbc_vfSeek(<n>)
#xtranslate hb_vfReadLen([<n,...>])       => hbc_vfReadLen(<n>)
#xtranslate hb_vfWrite([<n,...>])         => hbc_vfWrite(<n>)
#xtranslate hb_vfLoad([<n,...>])          => hbc_vfLoad(<n>)
#xtranslate hb_vfDirectory([<n,...>])     => hbc_vfDirectory(<n>)
#xtranslate hb_vfTimeGet([<n,...>])       => hbc_vfTimeGet(<n>)
#xtranslate hb_vfCopyFile([<n,...>])      => hbc_vfCopyFile(<n>)
#xtranslate hb_vfDirExists([<n,...>])     => hbc_vfDirExists(<n>)
#xtranslate hb_vfExists([<n,...>])        => hbc_vfExists(<n>)
#xtranslate hb_vfRename([<n,...>])        => hbc_vfRename(<n>)
#xtranslate hb_vfErase([<n,...>])         => hbc_vfErase(<n>)
#xtranslate hb_vfDirRemove([<n,...>])     => hbc_vfDirRemove(<n>)
#xtranslate hb_vfDirMake([<n,...>])       => hbc_vfDirMake(<n>)
#xtranslate hb_vfAttrGet([<n,...>])       => hbc_vfAttrGet(<n>)
#endif
