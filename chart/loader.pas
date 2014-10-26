unit loader;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  Symbols,
  fphttpclient;

type
  TOnFeedBack = procedure (AFeedBack: string) of object;

  { TGetDataThread }

  TGetDataThread = class(TThread)
  private
    FSymbol: TSymbol;
    FOnFeedBack: TOnFeedBack;
    FOnGetData: TNotifyEvent;
    FFeedBackStr: string;
    FData: string;
    procedure OnData;
    procedure WriteFeedBack;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Execute; override;
    property Data: string read FData;
    property OnGetData: TNotifyEvent read FOnGetData write FOnGetData;
    property Symbol: TSymbol read FSymbol write FSymbol;
    property OnFeedBack: TOnFeedBack read FOnFeedBack write FOnFeedBack;
  end;

var
  gThreadList: TThreadList;

implementation

{ TGetDataThread }

procedure TGetDataThread.OnData;
begin
  // cargamos los datos dentro
  // de este Synchronize
  FSymbol.PrepareArray(FData);
end;

procedure TGetDataThread.WriteFeedBack;
begin
  if Assigned(FOnFeedBack) then
    FOnFeedBack(FFeedBackStr);
end;

constructor TGetDataThread.Create;
begin
  inherited Create(True);
  gThreadList.Add(Self);
  Priority:= tpLower;
  FreeOnTerminate := True;
end;

destructor TGetDataThread.Destroy;
begin
  gThreadList.Remove(Self);
  inherited Destroy;
end;

procedure TGetDataThread.Execute;
var
  lUrl: string;
  lHttpClient: TFPHTTPClient;
begin
  FFeedBackStr:= 'Getting ' + FSymbol.Name;
  Synchronize(@WriteFeedBack);
  lHttpClient := TFPHTTPClient.Create(nil);
  try
    try
      lUrl := FSymbol.FilePath;
      FData := lHttpClient.Get(lUrl);
      if lHttpClient.ResponseStatusCode = 200 then
      begin
        FFeedBackStr := FSymbol.Name + ' loading done.';
        Synchronize(@WriteFeedBack);
        Synchronize(@OnData);
      end
      else
        raise Exception.Create('Error loading ' + FSymbol.Name + ' (' + lHttpClient.ResponseStatusText + ')');
    except
      on E: Exception do
      begin
        FFeedBackStr:= E.Message;
        Synchronize(@WriteFeedBack);
      end;
    end;
  finally
    lHttpClient.Free;
    Terminate;
  end;
end;


end.

