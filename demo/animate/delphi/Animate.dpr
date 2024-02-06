////////////////////////////////////////////////////////////////////////////////
//
//  FileName    :   Animate.dpr
//  Creator     :   Shen Min
//  Date        :   2024-02-07
//  Comment     :
//
//  Copyright (c) 2024 Shen Min (https://nicrosoft.net)
//
////////////////////////////////////////////////////////////////////////////////

program Animate;

uses
  Vcl.Forms,
  UMainForm in 'UMainForm.pas' {Form1},
  uanimate in '..\..\..\src\uanimate.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
