$VERSIONINFO:CompanyName=Zachary Spriggs
$VERSIONINFO:FILEVERSION#=1,0,0,0
$VERSIONINFO:FileDescription=Music Player v1.000
$VERSIONINFO:LegalCopyright=2020 Zachary Spriggs
$VERSIONINFO:Comments=Plays mp3 files and displays album art as well as embedded ID3v1 tags
$VERSIONINFO:ProductName=Music Player v1.000
$EXEICON:'music_note.ico'
': This program uses
': InForm - GUI library for QB64 - v1.1
': Fellippe Heitor, 2016-2019 - fellippe@qb64.org - @fellippeheitor
': https://github.com/FellippeHeitor/InForm
'-----------------------------------------------------------
' Dialog flag constants (use + or OR to use more than 1 flag value)
CONST OFN_ALLOWMULTISELECT = &H200& '  Allows the user to select more than one file, not recommended!
CONST OFN_CREATEPROMPT = &H2000& '     Prompts if a file not found should be created(GetOpenFileName only).
CONST OFN_EXTENSIONDIFFERENT = &H400& 'Allows user to specify file extension other than default extension.
CONST OFN_FILEMUSTEXIST = &H1000& '    Chechs File name exists(GetOpenFileName only).
CONST OFN_HIDEREADONLY = &H4& '        Hides read-only checkbox(GetOpenFileName only)
CONST OFN_NOCHANGEDIR = &H8& '         Restores the current directory to original value if user changed
CONST OFN_NODEREFERENCELINKS = &H100000& 'Returns path and file name of selected shortcut(.LNK) file instead of file referenced.
CONST OFN_NONETWORKBUTTON = &H20000& ' Hides and disables the Network button.
CONST OFN_NOREADONLYRETURN = &H8000& ' Prevents selection of read-only files, or files in read-only subdirectory.
CONST OFN_NOVALIDATE = &H100& '        Allows invalid file name characters.
CONST OFN_OVERWRITEPROMPT = &H2& '     Prompts if file already exists(GetSaveFileName only)
CONST OFN_PATHMUSTEXIST = &H800& '     Checks Path name exists (set with OFN_FILEMUSTEXIST).
CONST OFN_READONLY = &H1& '            Checks read-only checkbox. Returns if checkbox is checked
CONST OFN_SHAREAWARE = &H4000& '       Ignores sharing violations in networking
CONST OFN_SHOWHELP = &H10& '           Shows the help button (useless!)
'--------------------------------------------------------------------------------------------

DEFINT A-Z
TYPE FILEDIALOGTYPE
    lStructSize AS LONG '        For the DLL call
    hwndOwner AS LONG '          Dialog will hide behind window when not set correctly
    hInstance AS LONG '          Handle to a module that contains a dialog box template.
    lpstrFilter AS _OFFSET '     Pointer of the string of file filters
    lpstrCustFilter AS _OFFSET
    nMaxCustFilter AS LONG
    nFilterIndex AS LONG '       One based starting filter index to use when dialog is called
    lpstrFile AS _OFFSET '       String full of 0's for the selected file name
    nMaxFile AS LONG '           Maximum length of the string stuffed with 0's minus 1
    lpstrFileTitle AS _OFFSET '  Same as lpstrFile
    nMaxFileTitle AS LONG '      Same as nMaxFile
    lpstrInitialDir AS _OFFSET ' Starting directory
    lpstrTitle AS _OFFSET '      Dialog title
    flags AS LONG '              Dialog flags
    nFileOffset AS INTEGER '     Zero-based offset from path beginning to file name string pointed to by lpstrFile
    nFileExtension AS INTEGER '  Zero-based offset from path beginning to file extension string pointed to by lpstrFile.
    lpstrDefExt AS _OFFSET '     Default/selected file extension
    lCustData AS LONG
    lpfnHook AS LONG
    lpTemplateName AS _OFFSET
END TYPE

DECLARE DYNAMIC LIBRARY "user32"
    FUNCTION LoadIconA%& (BYVAL hInstance%&, BYVAL lpIconName%&)
    FUNCTION SetLayeredWindowAttributes& (BYVAL hwnd AS LONG, BYVAL crKey AS LONG, BYVAL bAlpha AS _UNSIGNED _BYTE, BYVAL dwFlags AS LONG)
    FUNCTION GetWindowLong& ALIAS "GetWindowLongA" (BYVAL hwnd AS LONG, BYVAL nIndex AS LONG)
    FUNCTION SetWindowLong& ALIAS "SetWindowLongA" (BYVAL hwnd AS LONG, BYVAL nIndex AS LONG, BYVAL dwNewLong AS LONG)
    FUNCTION GetWindowLongA& (BYVAL hwnd AS LONG, BYVAL nIndex AS LONG)
    FUNCTION SetWindowLongA& (BYVAL hwnd AS LONG, BYVAL nIndex AS LONG, BYVAL dwNewLong AS LONG)
    FUNCTION SetWindowPos& (BYVAL hwnd AS LONG, BYVAL hWndInsertAfter AS LONG, BYVAL x AS LONG, BYVAL y AS LONG, BYVAL cx AS LONG, BYVAL cy AS LONG, BYVAL wFlags AS LONG)
END DECLARE

DIM MyHwnd AS LONG
DIM SHARED WindowVal
DIM SHARED opacity

DECLARE DYNAMIC LIBRARY "comdlg32" ' Library declarations using _OFFSET types
    FUNCTION GetOpenFileNameA& (DIALOGPARAMS AS FILEDIALOGTYPE) ' The Open file dialog
    FUNCTION GetSaveFileNameA& (DIALOGPARAMS AS FILEDIALOGTYPE) ' The Save file dialog
END DECLARE
DECLARE DYNAMIC LIBRARY "playmidi32"
    FUNCTION PlayMIDI& (filename AS STRING)
END DECLARE
'DECLARE DYNAMIC LIBRARY "WINMM"
'    FUNCTION mciSendStringA% (lpstrCommand AS STRING, lpstrReturnString AS STRING, BYVAL uReturnLength AS INTEGER, BYVAL hwndCallback AS INTEGER)
' mciSendStringA function plays media files and returns the following:
' 0 = command sucessful
' - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
' lpstrCommand is the MCI command string (and optional flags) to send.
' lpstrReturnString is a string that holds any return information.
' uReturnLength is the length of the lpstrReturnString string passed.
' NOTE: If lpstrCommand given doesn't retun a value then lpstrReturnString
'       can be empty and uReturnLength can be set to 0.
' hwndCallback contains a callback window handle (only if the Notify flag used in lpstrCommand)
'====================================================================
'FUNCTION mciGetErrorStringA% (BYVAL dwError AS INTEGER, lpstrBuffer AS STRING, BYVAL uLength AS INTEGER)
' mciGetErrorStringA returns error info if the mciSendStringA failed.
' dwError is the return value from the mciSendString function.
' lpstrBuffer string holds the error information returned by the function.
' uLength is the length of the lpstrBuffer string buffer.
'====================================================================
'END DECLARE

': Controls' IDs: ------------------------------------------------------------------
DIM SHARED MusicPlayer AS LONG
DIM SHARED PlayBT AS LONG
DIM SHARED PauseBT AS LONG
DIM SHARED StopBT AS LONG
DIM SHARED LB AS LONG
DIM SHARED SongFileTB AS LONG
DIM SHARED OpenBT AS LONG
DIM SHARED position1 AS LONG
DIM SHARED LB2 AS LONG
DIM SHARED BackBT AS LONG
DIM SHARED ForwardBT AS LONG
DIM SHARED AlbumArt AS LONG
DIM SHARED RepeatBT AS LONG
DIM SHARED TitleLB AS LONG
DIM SHARED ArtistLB AS LONG
DIM SHARED YearLB AS LONG
DIM SHARED AlbumLB AS LONG
DIM SHARED s&
DIM SHARED Window$
DIM SHARED SongTitle$
DIM SHARED Artist$
'$INCLUDE:'InForm\InForm.ui'
'$INCLUDE:'InForm\xp.uitheme'
'$INCLUDE:'Music Player GUI.frm'

': Event procedures: ---------------------------------------------------------------
SUB __UI_BeforeInit
END SUB

SUB __UI_OnLoad
    IF COMMAND$ <> "" THEN
        IF INSTR(COMMAND$, ".mp3") OR INSTR(COMMAND$, ".wav") THEN
            Text(SongFileTB) = COMMAND$
            IF _FILEEXISTS(GetAlbumArt$(COMMAND$)) THEN
                LoadImage Control(AlbumArt), GetAlbumArt$(COMMAND$)
            ELSE
                LoadImage Control(AlbumArt), "Big_Note.png"
            END IF
            GetSongTags (COMMAND$)
            s& = _SNDOPEN(COMMAND$)
            _SNDPLAY (s&)
            Control(PlayBT).Disabled = True
            Control(PauseBT).Disabled = False
            Control(StopBT).Disabled = False
            Control(position1).Max = _SNDLEN(s&)
            lenseconds = _SNDLEN(s&) MOD 60
            lenminutes = (_SNDLEN(s&) \ 60) MOD 60
            IF lenminutes < 10 THEN
                IF lenseconds < 10 THEN
                    Caption(LB2) = "0" + LTRIM$(STR$(lenminutes)) + ":" + "0" + LTRIM$(STR$(lenseconds))
                ELSE
                    Caption(LB2) = "0" + LTRIM$(STR$(lenminutes)) + ":" + LTRIM$(STR$(lenseconds))
                END IF
            ELSE
                IF lenseconds < 10 THEN
                    Caption(LB2) = LTRIM$(STR$(lenminutes)) + ":" + "0" + LTRIM$(STR$(lenseconds))
                ELSE
                    Caption(LB2) = LTRIM$(STR$(lenminutes)) + ":" + LTRIM$(STR$(lenseconds))
                END IF
            END IF
        END IF
    END IF
    SetFrameRate 30
END SUB

SUB __UI_BeforeUpdateDisplay
    'This event occurs at approximately 30 frames per second.
    'You can change the update frequency by calling SetFrameRate DesiredRate%
    'CONST ERROR_ALREADY_EXISTS = &HB7

    'DECLARE DYNAMIC LIBRARY "kernel32"
    '    FUNCTION CreateMutexA%& (BYVAL lpMutexAttributes%&, BYVAL bInitialOwner&, BYVAL lpName%&)
    '    FUNCTION GetLastError~& ()
    'END DECLARE
    'Window$ = "Music Player v1.000"
    '_TITLE Window$
    'DIM t AS STRING
    't = "Global\" + Window$ + CHR$(0)
    'IF 0 = CreateMutexA(0, 0, _OFFSET(t)) THEN
    'END IF
    'see: http://msdn.microsoft.com/en-us/library/ms682411%28v=vs.85%29
    'IF ERROR_ALREADY_EXISTS = GetLastError THEN
    '    SYSTEM
    'END IF
    MyHwnd = _WINDOWHANDLE 'FindWindow(0, Window$ + CHR$(0))
    hWnd& = MyHwnd
    IF MyHwnd AND _WINDOWHASFOCUS THEN
        SetWindowOpacity MyHwnd, 255
    ELSEIF _WINDOWHASFOCUS = 0 THEN
        SetWindowOpacity MyHwnd, 200
    END IF
    _ACCEPTFILEDROP
    IF _TOTALDROPPEDFILES THEN
        IF _SNDPLAYING(s&) THEN
            _SNDSTOP s&
            _SNDCLOSE s&
            '_TITLE "Music Player v1.000"
        END IF
        OFile$ = _DROPPEDFILE
        IF INSTR(OFile$, ".mp3") OR INSTR(OFile$, ".wav") THEN
            Text(SongFileTB) = OFile$
            IF _FILEEXISTS(GetAlbumArt$(OFile$)) THEN
                LoadImage Control(AlbumArt), GetAlbumArt$(OFile$)
            ELSE
                LoadImage Control(AlbumArt), "Big_Note.png"
            END IF
            GetSongTags (OFile$)
            s& = _SNDOPEN(Text(SongFileTB))
            _SNDPLAY (s&)
            Control(position1).Max = _SNDLEN(s&)
            Control(PlayBT).Disabled = True
            Control(PauseBT).Disabled = False
            Control(StopBT).Disabled = False
            lenseconds = _SNDLEN(s&) MOD 60
            lenminutes = (_SNDLEN(s&) \ 60) MOD 60
            IF lenminutes < 10 THEN
                IF lenseconds < 10 THEN
                    Caption(LB2) = "0" + LTRIM$(STR$(lenminutes)) + ":" + "0" + LTRIM$(STR$(lenseconds))
                ELSE
                    Caption(LB2) = "0" + LTRIM$(STR$(lenminutes)) + ":" + LTRIM$(STR$(lenseconds))
                END IF
            ELSE
                IF lenseconds < 10 THEN
                    Caption(LB2) = LTRIM$(STR$(lenminutes)) + ":" + "0" + LTRIM$(STR$(lenseconds))
                ELSE
                    Caption(LB2) = LTRIM$(STR$(lenminutes)) + ":" + LTRIM$(STR$(lenseconds))
                END IF
            END IF
        END IF
    END IF
    seconds = _SNDGETPOS(s&) MOD 60
    minutes = (_SNDGETPOS(s&) \ 60) MOD 60
    IF minutes < 10 THEN
        IF seconds < 10 THEN
            Caption(LB) = "0" + LTRIM$(STR$(minutes)) + ":" + "0" + LTRIM$(STR$(seconds))
        ELSE
            Caption(LB) = "0" + LTRIM$(STR$(minutes)) + ":" + LTRIM$(STR$(seconds))
        END IF
    ELSE
        IF seconds < 10 THEN
            Caption(LB) = LTRIM$(STR$(minutes)) + ":" + "0" + LTRIM$(STR$(seconds))
        ELSE
            Caption(LB) = LTRIM$(STR$(minutes)) + ":" + LTRIM$(STR$(seconds))
        END IF
        Caption(LB) = LTRIM$(STR$(minutes)) + ":" + LTRIM$(STR$(seconds))
    END IF
    Control(position1).Value = _SNDGETPOS(s&)
    IF Control(position1).Value = _SNDLEN(s&) - 1 THEN
        Control(PlayBT).Disabled = False
        Control(RepeatBT).Disabled = False
    END IF
END SUB

SUB __UI_BeforeUnload
    'If you set __UI_UnloadSignal = False here you can
    'cancel the user's request to close.

END SUB

SUB __UI_Click (id AS LONG)
    SELECT CASE id
        CASE MusicPlayer

        CASE PlayBT
            Control(PlayBT).Disabled = True
            'IF Text(SongFileTB) <> "Select a file..." AND Caption(SongFileTB) <> "Select a file..." THEN
            IF INSTR(Text(SongFileTB), ".mp3") OR INSTR(Text(SongFileTB), ".wav") THEN
                _SNDPLAY s&
            END IF
            'SongTitle$ = Text(SongFileTB)
            'SongTitle$ = StripDirectory$(SongTitle$)
            'strip$ = StripDirectory$(Text(SongFileTB))
            Window$ = "Playing - " + SongTitle$ 'strip$
            _TITLE Window$
            Control(PauseBT).Disabled = False
            Control(StopBT).Disabled = False
            Control(RepeatBT).Disabled = False
        CASE PauseBT
            _SNDPAUSE (s&)
            Window$ = "Paused - " + SongTitle$ 'Text(SongFileTB)
            _TITLE Window$
            Control(PauseBT).Disabled = True
            Control(PlayBT).Disabled = False
        CASE StopBT
            Window$ = "Music Player v1.000"
            _TITLE Window$
            _SNDSTOP s&
            _SNDCLOSE (s&)
            Text(SongFileTB) = "Select a file..."
            Control(PlayBT).Disabled = True
            Control(PauseBT).Disabled = True
            Control(RepeatBT).Disabled = True
            Caption(TitleLB) = "Title: "
            Caption(ArtistLB) = "Artist: "
            Caption(AlbumLB) = "Album: "
            Caption(YearLB) = "Year: "
            LoadImage Control(AlbumArt), ""
            Caption(LB) = "00:00"
            Caption(LB2) = "00:00"
        CASE LB

        CASE SongFileTB

        CASE OpenBT
            IF _SNDPLAYING(s&) THEN
                _SNDSTOP s&
                _SNDCLOSE s&
                Control(PauseBT).Disabled = False
                Caption(TitleLB) = "Title: "
                Caption(ArtistLB) = "Artist: "
                Caption(AlbumLB) = "Album: "
                Caption(YearLB) = "Year: "
            END IF
            Control(PlayBT).Disabled = False
            Filter$ = "MP3 Files (.mp3)|*.MP3|WAV Files (.wav)|*.WAV" + CHR$(0)
            Flags& = OFN_FILEMUSTEXIST + OFN_NOCHANGEDIR + OFN_READONLY '    add flag constants here
            OFile$ = GetOpenFileName$(Title$ + CHR$(0), ".\", Filter$, 1, Flags&, hWnd&)
            IF OFile$ <> "" THEN
                Text(SongFileTB) = OFile$
                GetSongTags (OFile$)
                IF _FILEEXISTS(GetAlbumArt$(OFile$)) THEN
                    LoadImage Control(AlbumArt), GetAlbumArt$(OFile$)
                ELSE
                    LoadImage Control(AlbumArt), "Big_Note.png"
                END IF
            END IF
            s& = _SNDOPEN(Text(SongFileTB))
            _SNDPLAY (s&)
            'SongTitle$ = Text(SongFileTB)
            'SongTitle$ = StripDirectory$(SongTitle$)
            'strip$ = StripDirectory$(Text(SongFileTB))
            Window$ = "Playing - " + SongTitle$ 'strip$
            _TITLE Window$
            Control(PlayBT).Disabled = True
            Control(PauseBT).Disabled = False
            Control(StopBT).Disabled = False
            Control(position1).Max = _SNDLEN(s&)
            lenseconds = _SNDLEN(s&) MOD 60
            lenminutes = (_SNDLEN(s&) \ 60) MOD 60
            IF lenminutes < 10 THEN
                IF lenseconds < 10 THEN
                    Caption(LB2) = "0" + LTRIM$(STR$(lenminutes)) + ":" + "0" + LTRIM$(STR$(lenseconds))
                ELSE
                    Caption(LB2) = "0" + LTRIM$(STR$(lenminutes)) + ":" + LTRIM$(STR$(lenseconds))
                END IF
            ELSE
                IF lenseconds < 10 THEN
                    Caption(LB2) = LTRIM$(STR$(lenminutes)) + ":" + "0" + LTRIM$(STR$(lenseconds))
                ELSE
                    Caption(LB2) = LTRIM$(STR$(lenminutes)) + ":" + LTRIM$(STR$(lenseconds))
                END IF
                Caption(LB2) = LTRIM$(STR$(lenminutes)) + ":" + LTRIM$(STR$(lenseconds))
            END IF
        CASE position1
            _SNDSETPOS s&, Control(position1).Value
        CASE ForwardBT
            _SNDSETPOS s&, _SNDGETPOS(s&) + 30
        CASE BackBT
            _SNDSETPOS s&, _SNDGETPOS(s&) - 10
        CASE RepeatBT
            currentPos = _SNDGETPOS(s&)
            _SNDLOOP s&
            _SNDSETPOS s&, currentPos
    END SELECT
END SUB

SUB __UI_MouseEnter (id AS LONG)
    SELECT CASE id
        CASE MusicPlayer

        CASE PlayBT

        CASE PauseBT

        CASE StopBT

        CASE LB

        CASE SongFileTB

        CASE OpenBT

        CASE EqualizerGif

    END SELECT
END SUB

SUB __UI_MouseLeave (id AS LONG)
    SELECT CASE id
        CASE MusicPlayer

        CASE PlayBT

        CASE PauseBT

        CASE StopBT

        CASE LB

        CASE SongFileTB

        CASE OpenBT

        CASE EqualizerGif

    END SELECT
END SUB

SUB __UI_FocusIn (id AS LONG)
    SELECT CASE id
        CASE PlayBT

        CASE PauseBT

        CASE StopBT

        CASE SongFileTB

        CASE OpenBT

    END SELECT
END SUB

SUB __UI_FocusOut (id AS LONG)
    'This event occurs right before a control loses focus.
    'To prevent a control from losing focus, set __UI_KeepFocus = True below.
    SELECT CASE id
        CASE PlayBT

        CASE PauseBT

        CASE StopBT

        CASE SongFileTB

        CASE OpenBT

    END SELECT
END SUB

SUB __UI_MouseDown (id AS LONG)
    SELECT CASE id
        CASE MusicPlayer

        CASE PlayBT

        CASE PauseBT

        CASE StopBT

        CASE LB

        CASE SongFileTB

        CASE OpenBT

        CASE EqualizerGif

    END SELECT
END SUB

SUB __UI_MouseUp (id AS LONG)
    SELECT CASE id
        CASE MusicPlayer

        CASE PlayBT

        CASE PauseBT

        CASE StopBT

        CASE LB

        CASE SongFileTB

        CASE OpenBT

        CASE EqualizerGif

    END SELECT
END SUB

SUB __UI_KeyPress (id AS LONG)
    'When this event is fired, __UI_KeyHit will contain the code of the key hit.
    'You can change it and even cancel it by making it = 0
    SELECT CASE id
        CASE PlayBT

        CASE PauseBT

        CASE StopBT

        CASE SongFileTB

        CASE OpenBT

    END SELECT
END SUB

SUB __UI_TextChanged (id AS LONG)
    SELECT CASE id
        CASE SongFileTB

    END SELECT
END SUB

SUB __UI_ValueChanged (id AS LONG)
    SELECT CASE id
    END SELECT
END SUB

SUB __UI_FormResized

END SUB
FUNCTION GetOpenFileName$ (Title$, InitialDir$, Filter$, FilterIndex, Flags&, hWnd&)
    '  Title$      - The dialog title.
    '  InitialDir$ - If this left blank, it will use the directory where the last opened file is
    '  located. Specify ".\" if you want to always use the current directory.
    '  Filter$     - File filters separated by pipes (|) in the same format as using VB6 common dialogs.
    '  FilterIndex - The initial file filter to use. Will be altered by user during the call.
    '  Flags&      - Dialog flags. Will be altered by the user during the call.
    '  hWnd&       - Your program's window handle that should be aquired by the FindWindow function.
    '
    ' Returns: Blank when cancel is clicked otherwise, the file name selected by the user.
    ' FilterIndex and Flags& will be changed depending on the user's selections.
    Title$ = "Select a file" + CHR$(0)
    DIM OpenCall AS FILEDIALOGTYPE ' Needed for dialog call

    fFilter$ = Filter$
    FOR R = 1 TO LEN(fFilter$) ' Replace the pipes with character zero
        IF MID$(fFilter$, R, 1) = "|" THEN MID$(fFilter$, R, 1) = CHR$(0)
    NEXT R
    fFilter$ = fFilter$ + CHR$(0)

    lpstrFile$ = STRING$(2048, 0) ' For the returned file name
    lpstrDefExt$ = STRING$(10, 0) ' Extension will not be added when this is not specified
    OpenCall.lStructSize = LEN(OpenCall)
    OpenCall.hwndOwner = hWnd&
    OpenCall.lpstrFilter = _OFFSET(fFilter$)
    OpenCall.nFilterIndex = FilterIndex
    OpenCall.lpstrFile = _OFFSET(lpstrFile$)
    OpenCall.nMaxFile = LEN(lpstrFile$) - 1
    OpenCall.lpstrFileTitle = OpenCall.lpstrFile
    OpenCall.nMaxFileTitle = OpenCall.nMaxFile
    OpenCall.lpstrInitialDir = _OFFSET(InitialDir$)
    OpenCall.lpstrTitle = _OFFSET(Title$)
    OpenCall.lpstrDefExt = _OFFSET(lpstrDefExt$)
    OpenCall.flags = Flags&

    Result = GetOpenFileNameA&(OpenCall) '            Do Open File dialog call!

    IF Result THEN ' Trim the remaining zeros
        GetOpenFileName$ = LEFT$(lpstrFile$, INSTR(lpstrFile$, CHR$(0)) - 1)
        Flags& = OpenCall.flags
        FilterIndex = OpenCall.nFilterIndex
    END IF
END FUNCTION
'FUNCTION ReplaceStringItem$ (text$, old$, new$)
'    DO
'        find = INSTR(start + 1, text$, old$) 'find location of a word in text
'        IF find THEN
'            first$ = LEFT$(text$, find - 1) 'text before word including spaces
'            last$ = RIGHT$(text$, LEN(text$) - (find + LEN(old$) - 1)) 'text after word
'            text$ = first$ + new$ + last$
'        END IF
'        start = find
'    LOOP WHILE find
'    ReplaceStringItem$ = text$
'END FUNCTION
FUNCTION GetAlbumArt$ (OFile$)
    DO
        find = INSTR(find + 1, OFile$, "\")
        IF find THEN
            AlbumArtDir$ = LEFT$(OFile$, find)
        END IF
    LOOP WHILE find
    'SHELL _HIDE "dir " + CHR$(34) + AlbumArtDir$ + "\AlbumArt*Large.jpg" + CHR$(34) + " /a /b > albumartlarge.txt"
    'OPEN "albumartlarge.txt" FOR BINARY AS #1
    'LINE INPUT #1, albumart$
    'CLOSE #1
    GetAlbumArt$ = AlbumArtDir$ + "Folder.jpg" 'albumart$ '"AlbumArtSmall.jpg"
END FUNCTION

FUNCTION StripDirectory$ (OFile$)
    DO
        OFile$ = RIGHT$(OFile$, LEN(OFile$) - INSTR(OFile$, "\"))
    LOOP WHILE INSTR(OFile$, "\")
    StripDirectory$ = OFile$
END FUNCTION

SUB SetWindowOpacity (hWnd AS LONG, Level)
    DIM Msg AS LONG
    CONST G = -20
    CONST LWA_ALPHA = &H2
    CONST WS_EX_LAYERED = &H80000

    Msg = GetWindowLong(hWnd, G)
    Msg = Msg OR WS_EX_LAYERED
    Crap = SetWindowLong(hWnd, G, Msg)
    Crap = SetLayeredWindowAttributes(hWnd, 0, Level, LWA_ALPHA)
END SUB

SUB GetSongTags (OFile$)
    g% = FREEFILE
    OPEN OFile$ FOR BINARY AS #g%

    DIM Songname AS STRING * 30
    DIM Artist AS STRING * 30
    DIM Album AS STRING * 30
    DIM Year AS STRING * 4
    DIM position AS SINGLE

    position = LOF(g%) - 124
    GET #g%, position, Songname
    position = LOF(g%) - 94
    GET #g%, position, Artist
    position = LOF(g%) - 64
    GET #g%, position, Album
    position = LOF(g%) - 34
    GET #g%, position, Year
    CLOSE #g%
    SongTitle$ = Songname
    'Artist$ = Artist
    Caption(TitleLB) = "Title: " + (Songname)
    Caption(ArtistLB) = "Artist: " + (Artist)
    Caption(AlbumLB) = "Album: " + (Album)
    Caption(YearLB) = "Year: " + (Year)
    'a = _SHELLHIDE("songTags.exe " + CHR$(34) + OFile$ + CHR$(34))
    'IF a = 1 THEN
    'f% = FREEFILE
    'OPEN "songinfo.txt" FOR BINARY AS #f%
    'LINE INPUT #f%, Songname$
    'Caption(TitleLB) = "Title: " + Songname$
    'LINE INPUT #f%, Artist$
    'Caption(ArtistLB) = "Artist: " + Artist$
    'LINE INPUT #f%, Album$
    'Caption(AlbumLB) = "Album: " + Album$
    'LINE INPUT #f%, Year$
    'Caption(YearLB) = "Year: " + Year$
    'CLOSE #f%
    'END IF
END SUB
