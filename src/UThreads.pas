////////////////////////////////////////////////////////////////////////////////
//
//  FileName    :   UThreads.pas
//  Creator     :   Shen Min
//  Date        :   2021-10-03
//  Comment     :
//
//  Copyright (c) 2021 Shen Min (https://nicrosoft.net)
//
////////////////////////////////////////////////////////////////////////////////

unit UThreads;

{$IFDEF FPC}
{$mode delphi}{$H+}
{$ENDIF}

interface

uses
    Classes, SysUtils;

type
    {$IFDEF FPC}
    TProc = TProcedure;
    {$ENDIF}

    { TncThread }

    TncThread = class(TThread)
    public
        class procedure dispatch_to_main_thread(const ThreadProc: TProcedure); overload;
        class procedure dispatch_to_main_thread(const ThreadProc: TThreadMethod); overload;
        class procedure ensure_in_main_thread(const ThreadProc: TProcedure); overload;
        class procedure ensure_in_main_thread(const ThreadProc: TThreadMethod); overload;

        class function execute_in_thread(const ThreadProc: TProc; const AOnTerminate : TNotifyEvent = nil) : TThread;

        class function is_in_main_thread() : Boolean;

    end;

implementation

type

    { TncInternalRunInMainThread }

    TncInternalRunInMainThread = class(TThread)
    private
        m_proc : TProcedure;

        procedure do_in_main_thread();

    protected
        procedure Execute; override;

    public
        constructor Create(const ThreadProc: TProcedure);

    end;

{ TncInternalRunInMainThread }

procedure TncInternalRunInMainThread.do_in_main_thread();
begin
    if Assigned(m_proc) then
    begin
        m_proc();
    end;
end;

procedure TncInternalRunInMainThread.Execute;
begin
    Synchronize(do_in_main_thread);
end;

constructor TncInternalRunInMainThread.Create(const ThreadProc : TProcedure);
begin
    m_proc := ThreadProc;
    FreeOnTerminate := true;

    inherited Create(false);
end;

{ TncThread }

class procedure TncThread.dispatch_to_main_thread(const ThreadProc : TProcedure);
begin
    TncInternalRunInMainThread.Create(ThreadProc);
end;

class procedure TncThread.dispatch_to_main_thread(const ThreadProc : TThreadMethod);
begin
    ForceQueue(nil, ThreadProc);
end;

class procedure TncThread.ensure_in_main_thread(const ThreadProc : TProcedure);
begin
    if is_in_main_thread() then
    begin
        ThreadProc();
    end
    else
    begin
        dispatch_to_main_thread(ThreadProc);
    end;
end;

class procedure TncThread.ensure_in_main_thread(const ThreadProc : TThreadMethod);
begin
    if is_in_main_thread() then
    begin
        ThreadProc();
    end
    else
    begin
        Synchronize(nil, ThreadProc);
    end;
end;

class function TncThread.execute_in_thread(const ThreadProc: TProc; const AOnTerminate: TNotifyEvent): TThread;
begin
    Result := TThread.CreateAnonymousThread(ThreadProc);
    Result.OnTerminate := AOnTerminate;
    Result.Start();
end;

class function TncThread.is_in_main_thread() : Boolean;
begin
    if CurrentThread.ThreadID = MainThreadID then
    begin
        Result := true;
    end
    else
    begin
        Result := false;
    end;
end;

end.

