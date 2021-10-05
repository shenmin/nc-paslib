////////////////////////////////////////////////////////////////////////////////
//
//  FileName    :   UMainForm.pas
//  Creator     :   Shen Min
//  Date        :   2021-10-05
//  Comment     :
//
//  Copyright (c) 2021 Shen Min (https://nicrosoft.net)
//
////////////////////////////////////////////////////////////////////////////////

unit UMainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TMainForm = class(TForm)
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    Button1: TButton;
    Button4: TButton;
    Button2: TButton;
    Button5: TButton;
    Button6: TButton;
    Button3: TButton;
    Button7: TButton;
    Button8: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
  private
    procedure do_show_msg();

  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

uses UDelayRun, UThreads;

procedure do_show_msg_proc();
begin
    ShowMessage('Hello from Procedure.');
end;

{$R *.dfm}

procedure TMainForm.Button1Click(Sender: TObject);
begin
    shared_delay().run(do_show_msg, 1000);
end;

procedure TMainForm.Button2Click(Sender: TObject);
begin
    shared_delay().run(do_show_msg, 2000);
end;

procedure TMainForm.Button3Click(Sender: TObject);
begin
    shared_delay().cancel(do_show_msg);
end;

procedure TMainForm.Button4Click(Sender: TObject);
begin
    shared_delay().run(do_show_msg_proc, 1000);
end;

procedure TMainForm.Button5Click(Sender: TObject);
begin
    shared_delay().run(do_show_msg_proc, 2000);
end;

procedure TMainForm.Button6Click(Sender: TObject);
begin
    shared_delay().cancel(do_show_msg_proc);
end;

procedure TMainForm.Button7Click(Sender: TObject);
begin
    TncThread.execute_in_thread(
    procedure
    begin
        shared_delay().cancel();
        shared_delay().run(do_show_msg, 1000);
    end
    );
end;

procedure TMainForm.Button8Click(Sender: TObject);
begin
    TncThread.execute_in_thread(
    procedure
    begin
        shared_delay().cancel();
        shared_delay().run(do_show_msg_proc, 1000);
    end
    );
end;

procedure TMainForm.do_show_msg;
begin
    ShowMessage('Hello from Method.');
end;

end.
