program rttodaily;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp, StrUtils, DateUtils
  { you can add units after this };

type

  { TRTToDaily }

  TRTToDaily = class(TCustomApplication)
  private
    procedure Parse(AStr: TStringList);
    procedure AddToDaily(ADate, ASym, AOpen, AHigh, ALow, AClose: string);
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

{ TRTToDaily }

procedure TRTToDaily.Parse(AStr: TStringList);
var
  lDate: TDateTime;
  lDateStr: string;
  lSym: string;
  lOpen: string;
  lHigh: string;
  lLow: string;
  lClose: string;
  I: Integer;
  lLine: TStringList;
begin
  // 1er linea: Actualizacion:2014-09-23 17:59:22
  DefaultFormatSettings.DateSeparator:='-';
  DefaultFormatSettings.ShortDateFormat:='YYYY-MM-DD';

  lDate := StrToDate(Copy(AStr[0], 15, 10));

  DefaultFormatSettings.DateSeparator:='-';
  DefaultFormatSettings.ShortMonthNames[1] := 'Jan';
  DefaultFormatSettings.ShortMonthNames[2] := 'Feb';
  DefaultFormatSettings.ShortMonthNames[3] := 'Mar';
  DefaultFormatSettings.ShortMonthNames[4] := 'Apr';
  DefaultFormatSettings.ShortMonthNames[5] := 'May';
  DefaultFormatSettings.ShortMonthNames[6] := 'Jun';
  DefaultFormatSettings.ShortMonthNames[7] := 'Jul';
  DefaultFormatSettings.ShortMonthNames[8] := 'Aug';
  DefaultFormatSettings.ShortMonthNames[9] := 'Sep';
  DefaultFormatSettings.ShortMonthNames[10] := 'Oct';
  DefaultFormatSettings.ShortMonthNames[11] := 'Nov';
  DefaultFormatSettings.ShortMonthNames[12] := 'Dec';
  lDateStr := FormatDateTime('D-MMM-Y', lDate);
  lLine := TStringList.Create;
  lLine.Delimiter:=';';
  try
    for I := 1 to AStr.Count - 1 do
    begin
      lLine.DelimitedText:= AStr[I];
      lSym := AnsiReplaceStr(lLine[0], '"', '');
      lOpen := lLine[1];
      lHigh := lLine[2];
      lLow := lLine[3];
      lClose := lLine[4];
      AddToDaily(lDateStr, lSym, lOpen, lHigh, lLow, lClose);
    end;
  finally
    lLine.Free;
  end;
end;

procedure TRTToDaily.AddToDaily(ADate, ASym, AOpen, AHigh, ALow, AClose: string
  );
var
  searchResult : TSearchRec;
  lStr: TStringList;
  lLine: TStringList;
  lIndex: Integer;
  I: Integer;
  lData: string;
  lExt: string;

const
  //cDir = '/home/martin/devel/mercados/chart/';
  cDir = './';

begin
  // Try to find regular files matching Unit1.d* in the current dir
  if FindFirst(cDir + '*.csv', faAnyFile, searchResult) = 0 then
  begin
    repeat
      // excepciones
      if UpperCase(ASym) = 'YPFD' then ASym := 'YPF';
      if pos('.ADR', UpperCase(ASym)) = 0 then
        lExt := '.BA.CSV'
      else
        lExt := '.CSV';

      if UpperCase(ASym) + lExt = UpperCase(searchResult.Name) then
      begin
        Writeln(searchResult.Name);
        lStr := TStringList.Create;
        lLine := TStringList.Create;;
        lLine.Delimiter := ',';
        lData := Format('%s,%s,%s,%s,%s,0', [ADate, AOpen, AHigh, ALow, AClose]);
        try
          lStr.LoadFromFile(cDir + searchResult.Name);
          lIndex := -1;
          for I := 1 to lStr.Count - 1 do
          begin
            lLine.DelimitedText := lStr[I];
            if lLine[0] = ADate then
            begin
              lIndex := I;
              break;
            end;
          end;
          if lIndex = -1 then
          begin
            lStr.Insert(1, lData);
          end
          else
            lStr[lIndex] := lData;
          lStr.SaveToFile(searchResult.Name);
        finally
          lLine.Free;
          lStr.Free;
        end;
      end;
    until FindNext(searchResult) <> 0;

    // Must free up resources used by these successful finds
    FindClose(searchResult);
  end;
end;

procedure TRTToDaily.DoRun;
var
  lStr: TStringList;
  lIn: string;
begin
  lStr := TStringList.Create;
  While not EOF do
  begin
    ReadLn(lIn);
    lStr.Text := lStr.Text + lIn;
  end;
  writeln;
  try
    Parse(lStr);
  finally
    lStr.Free;
  end;

  Terminate;
end;

constructor TRTToDaily.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TRTToDaily.Destroy;
begin
  inherited Destroy;
end;

var
  Application: TRTToDaily;
begin
  Application:=TRTToDaily.Create(nil);
  Application.Title:='RTToDaily';
  Application.Run;
  Application.Free;
end.

