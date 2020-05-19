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
'$INCLUDE:'Open-SaveFile.BI'
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

DECLARE DYNAMIC LIBRARY "playmidi32"
    FUNCTION PlayMIDI& (filename AS STRING)
END DECLARE

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
    Control(PlayBT).HelperCanvas = __playflat&
    Control(PauseBT).HelperCanvas = __pause&
    Control(StopBT).HelperCanvas = __StopNormalRed
    Control(OpenBT).HelperCanvas = __openfolder
    Control(BackBT).HelperCanvas = __rewind
    Control(ForwardBT).HelperCanvas = __fastforward
    Control(RepeatBT).HelperCanvas = __loop
    IF COMMAND$ <> "" THEN
        IF INSTR(COMMAND$, ".mp3") OR INSTR(COMMAND$, ".wav") THEN
            Text(SongFileTB) = COMMAND$
            IF _FILEEXISTS(GetAlbumArt$(COMMAND$)) THEN
                LoadImage Control(AlbumArt), GetAlbumArt$(COMMAND$)
            ELSE
                'LoadImage Control(AlbumArt), "Big_Note.png"
                Control(AlbumArt).HelperCanvas = __BigNote
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
                'LoadImage Control(AlbumArt), "Big_Note.png"
                Control(AlbumArt).HelperCanvas = __BigNote
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
            IF INSTR(Text(SongFileTB), ".mp3") OR INSTR(Text(SongFileTB), ".wav") THEN
                _SNDPLAY s&
            END IF
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
                    'LoadImage Control(AlbumArt), "Big_Note.png"
                    Control(AlbumArt).HelperCanvas = __BigNote
                END IF
            END IF
            s& = _SNDOPEN(Text(SongFileTB))
            _SNDPLAY (s&)
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
    Caption(TitleLB) = "Title: " + (Songname)
    Caption(ArtistLB) = "Artist: " + (Artist)
    Caption(AlbumLB) = "Album: " + (Album)
    Caption(YearLB) = "Year: " + (Year)
END SUB
'$INCLUDE:'OpenFile.BM'
'$INCLUDE:'Replace.BM'
'$INCLUDE:'Big_Note.png.MEM'
'$INCLUDE:'Stop1NormalRed.png.MEM'
'$INCLUDE:'play-flat.png.MEM'
'$INCLUDE:'pause.png.MEM'
'$INCLUDE:'rewind.png.MEM'
'$INCLUDE:'fast-forward.png.MEM'
'$INCLUDE:'loop.png.MEM'
'$INCLUDE:'open-folder.png.MEM'
