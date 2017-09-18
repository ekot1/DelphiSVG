object ImageListEditor: TImageListEditor
  Left = 392
  Top = 450
  ActiveControl = OkButton
  Caption = 'ImageListEditor'
  ClientHeight = 316
  ClientWidth = 547
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  DesignSize = (
    547
    316)
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox1: TGroupBox
    Left = 8
    Top = 8
    Width = 435
    Height = 129
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Graphic'
    TabOrder = 0
    DesignSize = (
      435
      129)
    object Label1: TLabel
      Left = 128
      Top = 24
      Width = 31
      Height = 13
      Caption = 'Name:'
    end
    object ImagePanel: TPanel
      Left = 8
      Top = 16
      Width = 105
      Height = 105
      BevelOuter = bvNone
      BorderStyle = bsSingle
      Color = clWhite
      ParentBackground = False
      TabOrder = 0
      object ActImage: TImage
        Left = 7
        Top = 7
        Width = 89
        Height = 89
        Center = True
        Proportional = True
        Stretch = True
        Transparent = True
      end
    end
    object ImageName: TEdit
      Left = 128
      Top = 40
      Width = 291
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 1
      OnExit = ImageNameExit
    end
  end
  object GroupBox2: TGroupBox
    Left = 8
    Top = 144
    Width = 531
    Height = 129
    Anchors = [akLeft, akTop, akRight, akBottom]
    Caption = 'Pictures list'
    TabOrder = 1
    DesignSize = (
      531
      129)
    object ListView1: TListView
      Left = 8
      Top = 16
      Width = 515
      Height = 105
      Anchors = [akLeft, akTop, akRight, akBottom]
      Columns = <>
      DragMode = dmAutomatic
      FullDrag = True
      HideSelection = False
      IconOptions.Arrangement = iaLeft
      IconOptions.AutoArrange = True
      LargeImages = SVGImageList1
      MultiSelect = True
      ReadOnly = True
      ShowWorkAreas = True
      TabOrder = 0
      OnDragDrop = ListView1DragDrop
      OnDragOver = ListView1DragOver
      OnSelectItem = ListView1SelectItem
    end
  end
  object OkButton: TButton
    Left = 450
    Top = 16
    Width = 89
    Height = 25
    Anchors = [akTop, akRight]
    Caption = '&Ok'
    Default = True
    ModalResult = 1
    TabOrder = 2
    OnClick = OkButtonClick
  end
  object CancelButton: TButton
    Left = 450
    Top = 48
    Width = 89
    Height = 25
    Anchors = [akTop, akRight]
    Cancel = True
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 3
  end
  object ApplyButton: TButton
    Left = 450
    Top = 80
    Width = 89
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'A&pply'
    TabOrder = 4
    OnClick = ApplyButtonClick
  end
  object AddButton: TButton
    Left = 20
    Top = 279
    Width = 81
    Height = 25
    Anchors = [akBottom]
    Caption = '&Add...'
    TabOrder = 5
    OnClick = AddButtonClick
  end
  object DeleteButton: TButton
    Left = 231
    Top = 279
    Width = 81
    Height = 25
    Anchors = [akBottom]
    Caption = '&Delete'
    TabOrder = 6
    OnClick = DeleteButtonClick
  end
  object ClearButton: TButton
    Left = 336
    Top = 279
    Width = 81
    Height = 25
    Anchors = [akBottom]
    Caption = 'Empty'
    TabOrder = 7
    OnClick = ClearButtonClick
  end
  object ExportButton: TButton
    Left = 441
    Top = 279
    Width = 81
    Height = 25
    Anchors = [akBottom]
    Caption = '&Export...'
    TabOrder = 8
    OnClick = ExportButtonClick
  end
  object ReplaceButton: TButton
    Left = 125
    Top = 279
    Width = 81
    Height = 25
    Anchors = [akBottom]
    Caption = '&Replace...'
    TabOrder = 9
    OnClick = ReplaceButtonClick
  end
  object OpenPictureDialog1: TOpenPictureDialog
    Filter = 'Scalable Vector Graphics (*.svg)|*.svg'
    Options = [ofHideReadOnly, ofAllowMultiSelect, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 24
    Top = 168
  end
  object SavePictureDialog1: TSavePictureDialog
    DefaultExt = 'svg'
    Filter = 'Scalable Vector Graphics (*.svg)|*.svg'
    Options = [ofHideReadOnly, ofPathMustExist, ofEnableSizing]
    Left = 56
    Top = 168
  end
  object SVGImageList1: TSVGImageList
    Opacity = 255
    Width = 32
    Height = 32
    Left = 96
    Top = 168
    Images = {00000000}
  end
end
