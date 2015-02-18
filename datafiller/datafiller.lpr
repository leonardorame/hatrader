program datafiller;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp,
  Process, sqldb, pqconnection,
  fpjson, jsonparser, dateutils
  { you can add units after this };

type

  { TDataFiller }
  TProcEvent = procedure of object;

  TDataFiller = class(TCustomApplication)
  private
    FDataStream: TStringStream;
    FPQConnection: TPQConnection;
    FTransaction: TSQLTransaction;
    FSrvOn: Boolean;
    FDate: TDate;
    procedure GetData(AParam: string);
    procedure FillEspeciesAcciones;
    procedure FillEspeciesOpciones;
    procedure FillAcciones;
    procedure FillOpciones;
    procedure FillBadlar;
    procedure CheckFechaYMercado;
    procedure Process(AScript: string; ACallBack: TProcEvent);
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

{ TDataFiller }

procedure TDataFiller.GetData(AParam: string);
var
  lOut: TMemoryStream;
  lProcess: TProcess;
  lBuf: array[0..511] of byte;
  lReadCount: Integer;
begin
  lOut := TMemoryStream.Create;
  lProcess := TProcess.Create(nil);
  try
    lProcess.Executable := 'casper/bin/casperjs';
    lProcess.Parameters.Add(AParam);  // el primer - es lectura por stdin
    lProcess.Options := [poUsePipes];
    lProcess.Execute;

    while lProcess.Running or (lProcess.Output.NumBytesAvailable > 0) do
    begin
      // stdout
      while lProcess.Output.NumBytesAvailable > 0 do
      begin
        lReadCount := lProcess.Output.Read(lBuf, SizeOf(lBuf));
        if lReadCount > 0 then
          lOut.Write(lBuf, lReadCount);
      end;

      // stderr
      while lProcess.StdErr.NumBytesAvailable > 0 do
      begin
        lReadCount := lProcess.StdErr.Read(lBuf, SizeOf(lBuf));
        if lReadCount > 0 then
        begin
          with TFileStream.Create('datafiller.log', fmCreate or fmOpenWrite) do
          begin
            Write(lBuf, lReadCount);
            Free;
          end;
          Break;
        end;
      end;
    end;

    lOut.Position := 0;
    lOut.SaveToStream(FDataStream);
    FDataStream.Position:= 0;
    if Pos('Unable to open file ''cookies.txt''', FDataStream.DataString) > 0 then
      raise Exception.Create('Please read cookies and try again.')
    else
    if Pos('There was an error processing the request.', FDataStream.DataString) > 0 then
      raise Exception.Create('Please read cookies and try again.');
  finally
    lProcess.Free;
    lOut.Free;
  end;
end;

procedure TDataFiller.FillEspeciesAcciones;
var
  lQuery: TSQLQuery;
  lJson: TJSONObject;
  lArray: TJSONArray;
  lParser: TJSONParser;
  I: Integer;
begin
  lParser := TJSONParser.Create(FDataStream);
  lQuery := TSQLQuery.Create(nil);
  try
    lQuery.DataBase := FPQConnection;
    lArray := lJson.Arrays['d'];
    for I := 0 to lArray.Count - 1 do
    begin
      lJson := TJsonObject(lArray[I]);
      lQuery.SQL.Text:='insert into especies_acciones(simbolo, descripcion) ' +
        'values(:simbolo, :descripcion)';
      lQuery.ParamByName('simbolo').AsString:=lJson.Strings['Simbolo'];
      lQuery.ParamByName('descripcion').AsString:=lJson.Strings['Descripcion'];
      try
        FTransaction.StartTransaction;
        lQuery.ExecSQL;
        FTransaction.Commit;
      except
        on E: Exception do
        begin
          FTransaction.Rollback;
        end;
      end;
    end;
  finally
    lQuery.Free;
    lParser.Free;
  end;
end;

procedure TDataFiller.FillEspeciesOpciones;
var
  lQuery: TSQLQuery;
  lJson: TJSONObject;
  lArray: TJSONArray;
  lParser: TJSONParser;
  I: Integer;
  lSimbolo: string;
begin
  lParser := TJSONParser.Create(FDataStream);
  lQuery := TSQLQuery.Create(nil);
  try
    lQuery.DataBase := FPQConnection;
    lJson := TJsonObject(lParser.Parse);
    lArray := lJson.Arrays['d'];
    for I := 0 to lArray.Count - 1 do
    begin
      lJson := TJsonObject(lArray[I]);
      if lJson.Nulls['SimboloSubyacente'] then
        continue;
      lSimbolo := lJson.Strings['Simbolo'];
      lQuery.SQL.Text:='select fill_especies_opciones(:simbolo, :nombre, :subyacente)';
      lQuery.ParamByName('simbolo').AsString:=lJson.Strings['Simbolo'];
      lQuery.ParamByName('nombre').AsString:=lJson.Strings['Nombre'];
      lQuery.ParamByName('subyacente').AsString:=lJson.Strings['SimboloSubyacente'];
      try
        FTransaction.StartTransaction;
        lQuery.ExecSQL;
        FTransaction.Commit;
      except
        on E: Exception do
        begin
          FTransaction.RollBack;
        end;
      end;
    end;
  finally
    lQuery.Free;
    lParser.Free;
  end;
end;

procedure TDataFiller.FillAcciones;
var
  lQuery: TSQLQuery;
  lJson: TJSONObject;
  lArray: TJSONArray;
  lParser: TJSONParser;
  I: Integer;
  lSimbolo: string;
  y,m,d,h,n,s,ms: word;
begin
  lParser := TJSONParser.Create(FDataStream);
  lQuery := TSQLQuery.Create(nil);
  try
    lQuery.DataBase := FPQConnection;
    lJson := TJsonObject(lParser.Parse);
    lArray := lJson.Arrays['d'];
    for I := 0 to lArray.Count - 1 do
    begin
      if TJsonObject(lArray[I]).Strings['TablaNombre'] = 'tbAcciones' then
      begin
        lArray := TJSONObject(lArray[I]).Arrays['aTabla'];
        Break;
      end;
    end;

    for I := 0 to lArray.Count - 1 do
    begin
      lJson := TJsonObject(lArray[I]);
      lSimbolo := lJson.Strings['Simbolo'];
      lQuery.SQL.Text:='select fill_lideres_rt(:simbolo, :bid_qty, :bid, :ask_qty, :ask, :open, :high, :low, :last, :prev, :vol, :fechahora, :msgnro)';
      lQuery.ParamByName('simbolo').AsString:=lJson.Strings['Simbolo'];
      lQuery.ParamByName('bid_qty').AsInteger:=lJson.Integers['CantidadNominalCompra'];
      lQuery.ParamByName('bid').AsFloat:=lJson.Floats['PrecioCompra'];
      lQuery.ParamByName('ask').AsFloat:=lJson.Floats['PrecioVenta'];
      lQuery.ParamByName('open').AsFloat:=lJson.Floats['PrecioApertura'];
      lQuery.ParamByName('high').AsFloat:=lJson.Floats['PrecioMaximo'];
      lQuery.ParamByName('low').AsFloat:=lJson.Floats['PrecioMinimo'];
      lQuery.ParamByName('last').AsFloat:=lJson.Floats['PrecioUltimo'];
      lQuery.ParamByName('prev').AsFloat:=lJson.Floats['PrecioCierreAnterior'];
      lQuery.ParamByName('vol').AsInteger:=lJson.Integers['VolumenNominal'];
      lQuery.ParamByName('ask_qty').AsInteger:=lJson.Integers['CantidadNominalVenta'];
      lQuery.ParamByName('msgnro').AsInteger:=lJson.Integers['MensajeNro'];
      // valores por defecto h,m,s
      h := 0;
      n := 0;
      s := 0;
      ms := 0;
      DecodeDate(FDate, y, m, d);
      if lJson.Strings['HoraCotizacion'] <> '' then
        DecodeTime(StrToTime(lJson.Strings['HoraCotizacion']), h, n, s, ms);
      lQuery.ParamByName('fechahora').AsDateTime := EncodeDateTime(y, m, d, h, n, s, ms);
      try
        FTransaction.StartTransaction;
        lQuery.ExecSQL;
        FTransaction.Commit;
      except
        on E: Exception do
        begin
          if E is EPQDatabaseError then
          begin
            Writeln('Simbolo:' + lJson.Strings['Simbolo'] + ' - Sql State: ' + EPQDatabaseError(E).SQLSTATE);
          end;
          FTransaction.Rollback;
        end;
      end;
    end;
  finally
    lQuery.Free;
    lParser.Free;
  end;
end;

procedure TDataFiller.FillOpciones;
var
  lQuery: TSQLQuery;
  lJson: TJSONObject;
  lArray: TJSONArray;
  lParser: TJSONParser;
  I: Integer;
  lSimbolo: string;
  y,m,d,h,n,s,ms: word;
begin
  lParser := TJSONParser.Create(FDataStream);
  lQuery := TSQLQuery.Create(nil);
  try
    lQuery.DataBase := FPQConnection;
    lJson := TJsonObject(lParser.Parse);
    lArray := lJson.Arrays['d'];
    for I := 0 to lArray.Count - 1 do
    begin
      if TJsonObject(lArray[I]).Strings['TablaNombre'] = 'tbOpciones' then
      begin
        lArray := TJSONObject(lArray[I]).Arrays['aTabla'];
        Break;
      end;
    end;

    for I := 0 to lArray.Count - 1 do
    begin
      lJson := TJsonObject(lArray[I]);
      lSimbolo := lJson.Strings['Simbolo'];
      lQuery.SQL.Text:='select fill_opciones_rt(:simbolo, :bid_qty, :bid, :ask_qty, :ask, :last, :fechahora, :mensajenro)';
      lQuery.ParamByName('simbolo').AsString:=lJson.Strings['Simbolo'];
      lQuery.ParamByName('bid_qty').AsInteger:=lJson.Integers['CantidadNominalCompra'];
      lQuery.ParamByName('bid').AsFloat:=lJson.Floats['PrecioCompra'];
      lQuery.ParamByName('ask').AsFloat:=lJson.Floats['PrecioVenta'];
      lQuery.ParamByName('last').AsFloat:=lJson.Floats['PrecioUltimo'];
      lQuery.ParamByName('ask_qty').AsInteger:=lJson.Integers['CantidadNominalVenta'];
      lQuery.ParamByName('mensajenro').AsInteger:=lJson.Integers['MensajeNro'];
      DecodeDate(FDate, y, m, d);
      // valores por defecto h,m,s
      h := 0;
      n := 0;
      s := 0;
      ms := 0;
      DecodeDate(FDate, y, m, d);
      if lJson.Strings['HoraCotizacion'] <> '' then
        DecodeTime(StrToTime(lJson.Strings['HoraCotizacion']), h, n, s, ms);
      lQuery.ParamByName('fechahora').AsDateTime := EncodeDateTime(y, m, d, h, n, s, ms);
      try
        FTransaction.StartTransaction;
        lQuery.ExecSQL;
        FTransaction.Commit;
      except
        on E: Exception do
        begin
          if E is EPQDatabaseError then
          begin
            Writeln('Simbolo:' + lJson.Strings['Simbolo'] + ' - Sql State: ' + EPQDatabaseError(E).SQLSTATE);
          end;
          FTransaction.Rollback;
        end;
      end;
    end;
  finally
    lQuery.Free;
    lParser.Free;
  end;
end;

procedure TDataFiller.FillBadlar;
var
  lStr: string;
  lParser: TJSONParser;
  lArray: TJSONArray;
  lJson: TJSONObject;
  I: Integer;
  lFecha: string;
  lValor: double;
  d,m,y: word;
  lQuery: TSQLQuery;
begin
  // Eliminamos el HTML
  lStr := Copy(FDataStream.DataString, Pos('[', FDataStream.DataString), Length(FDataStream.DataString));
  lStr := Copy(lStr, 0, Pos('</body', lStr) - 1);
  lParser := TJSONParser.Create(lStr);
  lQuery := TSQLQuery.Create(nil);
  try
    lArray := TJSONArray(lParser.Parse);
    for I := 0 to lArray.Count - 1 do
    begin
      lJson := TJsonObject(lArray[I]);
      lFecha := TJsonObject(lJson.Items[0]).AsString;
      y := StrToInt(Copy(lFecha, 0, 4));
      m := StrToInt(Copy(lFecha, 6,2));
      d := StrToInt(Copy(lFecha, 9, 2));
      lValor := TJsonObject(lJson.Items[1]).AsFloat;
      lQuery.DataBase := FPQConnection;
      lQuery.SQL.Text:='insert into badlar(fecha, valor) values(:fecha, :valor)';
      lQuery.ParamByName('fecha').AsDate:=EncodeDate(y,m,d);
      lQuery.ParamByName('valor').AsFloat:=lValor;
      try
        FTransaction.StartTransaction;
        lQuery.ExecSQL;
        FTransaction.Commit;
      except
        on E: Exception do
        begin
          Writeln(E.Message);
          FTransaction.Rollback;
        end;
      end;
    end;
  finally
    lQuery.Free;
    lParser.Free;
  end;
end;

procedure TDataFiller.CheckFechaYMercado;
var
  lJson: TJSONObject;
  lParser: TJSONParser;
  lStr: TFileStream;
  d,m,y: word;
  lFechaUltimaRueda: TDate;

begin
  lStr := TFileStream.Create('fechaymercado.json', fmOpenRead);
  lStr.Position:= 0;
  lParser := TJSONParser.Create(lStr);
  try
    try
      lJSon := TJSONObject(lParser.Parse);
      lJson := lJson.Objects['d'];
      FSrvOn:= lJson.Integers['SrvOn'] = 1;
      d := StrToInt(Copy(lJson.Strings['Fecha'], 1, 2));
      m := StrToInt(Copy(lJson.Strings['Fecha'], 4, 2));
      y := StrToInt(Copy(lJson.Strings['Fecha'], 7, 4));
      FDate := EncodeDate(y,m,d);
      m := StrToInt(Copy(lJson.Strings['FechaUltimaRueda'], 1, 2));
      d := StrToInt(Copy(lJson.Strings['FechaUltimaRueda'], 4, 2));
      y := StrToInt(Copy(lJson.Strings['FechaUltimaRueda'], 7, 4));
      lFechaUltimaRueda := EncodeDate(y,m,d);
      if FDate <> lFechaUltimaRueda then
        raise Exception.Create('Please read fechaymercado again.');
    except
      on E: Exception do
      begin
        // este error es porque falta leer cookies
        if Pos('Invalid character at ', E.Message) > 0 then
          raise Exception.Create('Please read cookies and try again.')
        else
          raise;
      end;
    end;
  finally
    lParser.Free;
    lStr.Free;
  end;
end;

procedure TDataFiller.Process(AScript: string; ACallBack: TProcEvent);
begin
  try
    GetData(AScript);
    if ACallBack <> nil then
      ACallBack;
  except
    on E: Exception do
    begin
      Writeln(E.message);
    end;
  end;
end;

procedure TDataFiller.DoRun;
var
  ErrorMsg: String;
begin
  // parse parameters
  if HasOption('h','help') then begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  if HasOption('especies-lideres') then
  begin
    Process('especies-lideres.js', @FillEspeciesAcciones);
    Terminate;
    Exit;
  end;

  if HasOption('especies-opciones') then
  begin
    Process('especies-opciones.js', @FillEspeciesOpciones);
    Terminate;
    Exit;
  end;

  if HasOption('badlar') then
  begin
    Process('badlar.js', @FillBadlar);
    Terminate;
    Exit;
  end;

  if HasOption('fechaymercado') then
  begin
    Process('fechaymercado.js', nil);
    Terminate;
    Exit;
  end;

  try
    CheckFechaYMercado;
  except
    on E: exception do
    begin
      Writeln(e.message);
      Terminate;
      exit;
    end;
  end;

  if not FSrvOn then
  begin
    Write('Servidor inactivo.');
    Terminate;
    exit;
  end;

  if HasOption('lideres') then
  begin
    Process('lideres.js', @FillAcciones);
    Terminate;
    Exit;
  end;

  if HasOption('opciones') then
  begin
    Process('opciones.js', @FillOpciones);
    Terminate;
    Exit;
  end;


  // stop program loop
  Terminate;
end;

constructor TDataFiller.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
  FDataStream := TStringStream.Create('');
  FPQConnection := TPQConnection.Create(nil);
  FPQConnection.DatabaseName:= 'trading';
  FPQConnection.HostName:= '127.0.0.1';
  FPQConnection.UserName:= 'postgres';
  FPQConnection.Password:= 'postgres';
  FTransaction := TSQLTransaction.Create(FPQConnection);
  FTransaction.DataBase := FPQConnection;
  FPQConnection.Connected:= True;
end;

destructor TDataFiller.Destroy;
begin
  FDataStream.Free;
  FPQConnection.Free;
  inherited Destroy;
end;

procedure TDataFiller.WriteHelp;
begin
  { add your help code here }
  writeln('Usage: ',ExeName,' -h');
end;

var
  Application: TDataFiller;
begin
  Application:=TDataFiller.Create(nil);
  Application.Run;
  Application.Free;
end.

