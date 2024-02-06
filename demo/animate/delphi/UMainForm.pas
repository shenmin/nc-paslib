////////////////////////////////////////////////////////////////////////////////
//
//  FileName    :   UMainForm.pas
//  Creator     :   Shen Min
//  Date        :   2024-02-07
//  Comment     :
//
//  Copyright (c) 2024 Shen Min (https://nicrosoft.net)
//
////////////////////////////////////////////////////////////////////////////////

unit UMainForm;

interface

uses Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
     Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Imaging.pngimage, Vcl.ExtCtrls, Vcl.StdCtrls, Math;

type
    TForm1 = class(TForm)
        Button1: TButton;
        Panel1: TPanel;
        Label1: TLabel;
        GroupBox1: TGroupBox;
        Image1: TImage;
        Button2: TButton;
        Label3: TLabel;
        Button3: TButton;
        StaticText1: TStaticText;
        procedure Button1Click(Sender: TObject);
        procedure Button2Click(Sender: TObject);
        procedure Button3Click(Sender: TObject);

    private
        procedure acceleration_animation(const Sender : TObject; const animate_tag : Integer; var progress : real);
        procedure deceleration_animation(const Sender : TObject; const animate_tag : Integer; var progress : real);

  end;

var
    Form1: TForm1;

implementation

uses UAnimate;

{$R *.dfm}

procedure TForm1.acceleration_animation(const Sender: TObject;  const animate_tag: Integer; var progress: real);
begin
    progress := power(progress, 3); // make an acceleration effect
end;

procedure TForm1.Button1Click(Sender: TObject);
var
    animate_group : TncAnimateGroup;
begin
    Button1.Enabled := false;
    animate_group := TncAnimateGroup.Create(Panel1);
    animate_group.add_size_animate(Button1.Left, Panel1.Height, false, 500); // Expand the Panel1 to Button1's left border within 500ms.
    animate_group.add_size_animate(100, Panel1.Height, false, 500); // Animate the width of Panel1 to 100 within 500ms.
    animate_group.run();
    animate_group.Free();
    Button1.Enabled := true;
end;

procedure TForm1.Button2Click(Sender: TObject);
var
    animate_group : TncAnimateAsyncGroup;
begin
    animate_group := TncAnimateAsyncGroup.Create();
    animate_group.add_move_animate(StaticText1, GroupBox1.Width - StaticText1.Width - 10, StaticText1.Top, 1000);
    animate_group.add_change_animate(Image1, GroupBox1.Width - 144 - 10, Image1.Top, 144, 144, 1000);
    animate_group.run(acceleration_animation);
    animate_group.Free();

    animate_group := TncAnimateAsyncGroup.Create();
    animate_group.add_move_animate(StaticText1, 32, StaticText1.Top, 1000);
    animate_group.add_change_animate(Image1, 56, Image1.Top, 72, 72, 1000);
    animate_group.run(deceleration_animation);
    animate_group.Free();
end;

procedure TForm1.Button3Click(Sender: TObject);
var
    animate_group : TncAnimateAsyncGroup;
begin
    animate_group := TncAnimateAsyncGroup.Create();
    animate_group.add_alpha_animate(self, 0, 1000);
    animate_group.run();
    animate_group.Free();

    animate_group := TncAnimateAsyncGroup.Create();
    animate_group.add_alpha_animate(self, 255, 1000);
    animate_group.run();
    animate_group.Free();
end;

procedure TForm1.deceleration_animation(const Sender: TObject; const animate_tag: Integer; var progress: real);
begin
    progress := power(progress, 1/3); // make a deceleration effect
end;

end.
