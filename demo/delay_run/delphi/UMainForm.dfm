object MainForm: TMainForm
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Delay Run Demo'
  ClientHeight = 294
  ClientWidth = 674
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox1: TGroupBox
    Left = 32
    Top = 24
    Width = 609
    Height = 137
    Caption = 'Main Thread'
    TabOrder = 0
    object Button1: TButton
      Left = 32
      Top = 32
      Width = 264
      Height = 25
      Caption = 'Delay (1000ms) Show Message'
      TabOrder = 0
      OnClick = Button1Click
    end
    object Button4: TButton
      Left = 315
      Top = 32
      Width = 264
      Height = 25
      Caption = 'Delay (1000ms) Show Message'
      TabOrder = 1
      OnClick = Button4Click
    end
    object Button2: TButton
      Left = 32
      Top = 65
      Width = 264
      Height = 25
      Caption = 'Delay (2000ms) Show Message'
      TabOrder = 2
      OnClick = Button2Click
    end
    object Button5: TButton
      Left = 315
      Top = 65
      Width = 264
      Height = 25
      Caption = 'Delay (2000ms) Show Message'
      TabOrder = 3
      OnClick = Button5Click
    end
    object Button6: TButton
      Left = 315
      Top = 96
      Width = 83
      Height = 25
      Caption = 'Cancel'
      TabOrder = 4
      OnClick = Button6Click
    end
    object Button3: TButton
      Left = 32
      Top = 96
      Width = 83
      Height = 25
      Caption = 'Cancel'
      TabOrder = 5
      OnClick = Button3Click
    end
  end
  object GroupBox2: TGroupBox
    Left = 32
    Top = 184
    Width = 609
    Height = 81
    Caption = 'Threading'
    TabOrder = 1
    object Button7: TButton
      Left = 32
      Top = 32
      Width = 264
      Height = 25
      Caption = 'Delay (1000ms) Show Message'
      TabOrder = 0
      OnClick = Button7Click
    end
    object Button8: TButton
      Left = 315
      Top = 32
      Width = 264
      Height = 25
      Caption = 'Delay (1000ms) Show Message'
      TabOrder = 1
      OnClick = Button8Click
    end
  end
end
