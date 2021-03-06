': This form was generated by
': InForm - GUI library for QB64 - v1.1
': Fellippe Heitor, 2016-2019 - fellippe@qb64.org - @fellippeheitor
': https://github.com/FellippeHeitor/InForm
'-----------------------------------------------------------
SUB __UI_LoadForm

    DIM __UI_NewID AS LONG, __UI_RegisterResult AS LONG

    __UI_NewID = __UI_NewControl(__UI_Type_Form, "MusicPlayer", 501, 380, 0, 0, 0)
    __UI_RegisterResult = 0
    SetCaption __UI_NewID, "Music Player v1.000"
    Control(__UI_NewID).Font = SetFont("segoeui.ttf", 12)
    Control(__UI_NewID).CenteredWindow = True
    Control(__UI_NewID).Encoding = 437

    __UI_NewID = __UI_NewControl(__UI_Type_Button, "PlayBT", 50, 37, 141, 115, 0)
    __UI_RegisterResult = 0
    Control(__UI_NewID).CanHaveFocus = True

    __UI_NewID = __UI_NewControl(__UI_Type_Button, "PauseBT", 50, 37, 198, 115, 0)
    __UI_RegisterResult = 0
    Control(__UI_NewID).CanHaveFocus = True
    Control(__UI_NewID).Disabled = True

    __UI_NewID = __UI_NewControl(__UI_Type_Button, "StopBT", 50, 37, 255, 115, 0)
    __UI_RegisterResult = 0
    Control(__UI_NewID).CanHaveFocus = True
    Control(__UI_NewID).Disabled = True

    __UI_NewID = __UI_NewControl(__UI_Type_Label, "LB", 27, 21, 111, 65, 0)
    __UI_RegisterResult = 0
    SetCaption __UI_NewID, "00:00"
    Control(__UI_NewID).VAlign = __UI_Middle
    Control(__UI_NewID).AutoSize = True

    __UI_NewID = __UI_NewControl(__UI_Type_TextBox, "SongFileTB", 485, 23, 8, 14, 0)
    __UI_RegisterResult = 0
    SetCaption __UI_NewID, "Select a file..."
    Control(__UI_NewID).HasBorder = True
    Control(__UI_NewID).CanHaveFocus = True
    Control(__UI_NewID).Disabled = True
    Control(__UI_NewID).BorderSize = 1

    __UI_NewID = __UI_NewControl(__UI_Type_Button, "OpenBT", 80, 23, 210, 49, 0)
    __UI_RegisterResult = 0
    SetCaption __UI_NewID, "Open"
    Control(__UI_NewID).CanHaveFocus = True
    __UI_RegisterResult = RegisterKeyCombo("Ctrl+O", __UI_NewID)

    __UI_NewID = __UI_NewControl(__UI_Type_Label, "LB2", 27, 21, 364, 65, 0)
    __UI_RegisterResult = 0
    SetCaption __UI_NewID, "00:00"
    Control(__UI_NewID).VAlign = __UI_Middle
    Control(__UI_NewID).AutoSize = True

    __UI_NewID = __UI_NewControl(__UI_Type_TrackBar, "position1", 208, 40, 147, 76, 0)
    __UI_RegisterResult = 0
    Control(__UI_NewID).Max = 10
    Control(__UI_NewID).CanHaveFocus = True
    Control(__UI_NewID).Interval = 1

    __UI_NewID = __UI_NewControl(__UI_Type_Button, "BackBT", 50, 37, 84, 115, 0)
    __UI_RegisterResult = 0
    Control(__UI_NewID).CanHaveFocus = True

    __UI_NewID = __UI_NewControl(__UI_Type_Button, "ForwardBT",  50, 37, 369, 115, 0)
    __UI_RegisterResult = 0
    Control(__UI_NewID).CanHaveFocus = True

    __UI_NewID = __UI_NewControl(__UI_Type_PictureBox, "AlbumArt", 200, 200, 24, 170, 0)
    __UI_RegisterResult = 0
    Control(__UI_NewID).Stretch = True
    Control(__UI_NewID).Align = __UI_Center
    Control(__UI_NewID).VAlign = __UI_Middle
    Control(__UI_NewID).BorderSize = 1

    __UI_NewID = __UI_NewControl(__UI_Type_Button, "RepeatBT",  50, 37, 312, 115, 0)
    __UI_RegisterResult = 0
    Control(__UI_NewID).CanHaveFocus = True

    __UI_NewID = __UI_NewControl(__UI_Type_Label, "TitleLB", 25, 21, 238, 170, 0)
    __UI_RegisterResult = 0
    SetCaption __UI_NewID, "Title:"
    Control(__UI_NewID).VAlign = __UI_Middle
    Control(__UI_NewID).AutoSize = True

    __UI_NewID = __UI_NewControl(__UI_Type_Label, "ArtistLB", 31, 21, 238, 198, 0)
    __UI_RegisterResult = 0
    SetCaption __UI_NewID, "Artist:"
    Control(__UI_NewID).VAlign = __UI_Middle
    Control(__UI_NewID).AutoSize = True

    __UI_NewID = __UI_NewControl(__UI_Type_Label, "AlbumLB", 39, 21, 238, 226, 0)
    __UI_RegisterResult = 0
    SetCaption __UI_NewID, "Album:"
    Control(__UI_NewID).VAlign = __UI_Middle
    Control(__UI_NewID).AutoSize = True

    __UI_NewID = __UI_NewControl(__UI_Type_Label, "YearLB", 26, 21, 238, 258, 0)
    __UI_RegisterResult = 0
    SetCaption __UI_NewID, "Year:"
    Control(__UI_NewID).VAlign = __UI_Middle
    Control(__UI_NewID).AutoSize = True

END SUB

SUB __UI_AssignIDs
    MusicPlayer = __UI_GetID("MusicPlayer")
    PlayBT = __UI_GetID("PlayBT")
    PauseBT = __UI_GetID("PauseBT")
    StopBT = __UI_GetID("StopBT")
    LB = __UI_GetID("LB")
    SongFileTB = __UI_GetID("SongFileTB")
    OpenBT = __UI_GetID("OpenBT")
    LB2 = __UI_GetID("LB2")
    position1 = __UI_GetID("position1")
    BackBT = __UI_GetID("BackBT")
    ForwardBT = __UI_GetID("ForwardBT")
    AlbumArt = __UI_GetID("AlbumArt")
    RepeatBT = __UI_GetID("RepeatBT")
    TitleLB = __UI_GetID("TitleLB")
    ArtistLB = __UI_GetID("ArtistLB")
    AlbumLB = __UI_GetID("AlbumLB")
    YearLB = __UI_GetID("YearLB")
END SUB
