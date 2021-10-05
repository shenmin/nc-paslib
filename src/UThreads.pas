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
        class procedure dispatch_to_main_thread(const proc: TProcedure); overload;
        class procedure dispatch_to_main_thread(const proc_method: TThreadMethod); overload;
        class procedure ensure_in_main_thread(const proc: TProcedure); overload;
        class procedure ensure_in_main_thread(const proc_method: TThreadMethod); overload;

        class function execute_in_thread(const proc: TProc; const AOnTerminate : TNotifyEvent = nil) : TThread;

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

class procedure TncThread.dispatch_to_main_thread(const proc : TProcedure);
begin
    TncInternalRunInMainThread.Create(proc);
end;

class procedure TncThread.dispatch_to_main_thread(const proc_method : TThreadMethod);
begin
    ForceQueue(nil, proc_method);
end;

class procedure TncThread.ensure_in_main_thread(const proc : TProcedure);
begin
    if is_in_main_thread() then
    begin
        proc();
    end
    else
    begin
        dispatch_to_main_thread(proc);
    end;
end;

class procedure TncThread.ensure_in_main_thread(const proc_method : TThreadMethod);
begin
    if is_in_main_thread() then
    begin
        proc_method();
    end
    else
    begin
        Synchronize(nil, proc_method);
    end;
end;

class function TncThread.execute_in_thread(const proc: TProc; const AOnTerminate: TNotifyEvent): TThread;
begin
    Result := TThread.CreateAnonymousThread(proc);
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

