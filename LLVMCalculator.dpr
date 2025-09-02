program LLVMCalculator;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  System.Rtti,
  System.Character,
  Dlluminator in 'LLVMSupport\Dlluminator.pas',
  libLLVM.API in 'LLVMSupport\libLLVM.API.pas',
  libLLVM.LLD in 'LLVMSupport\libLLVM.LLD.pas',
  libLLVM in 'LLVMSupport\libLLVM.pas',
  libLLVM.Utils in 'LLVMSupport\libLLVM.Utils.pas';

type
  // Token types for expression parsing
  TTokenType = (ttNumber, ttOperator, ttLeftParen, ttRightParen, ttEnd);

  TToken = record
    TokenType: TTokenType;
    Value: string;
    NumValue: Double;
  end;

  // Simple expression parser and LLVM code generator
  TExpressionCalculator = class
  private
    FLLVM: TLLVM;
    FTokens: TArray<TToken>;
    FCurrentToken: Integer;
    FModuleId: string;

    function Tokenize(const Expression: string): TArray<TToken>;
    function GetCurrentToken: TToken;
    function ConsumeToken: TToken;
    function PeekToken: TToken;

    // Recursive descent parser methods
    function ParseExpression: TValue;
    function ParseTerm: TValue;
    function ParseFactor: TValue;

  public
    constructor Create;
    destructor Destroy; override;

    function Calculate(const Expression: string): Double;
  end;

constructor TExpressionCalculator.Create;
begin
  inherited Create;
  FLLVM := TLLVM.Create;
  FModuleId := 'calc_module';
end;

destructor TExpressionCalculator.Destroy;
begin
  FLLVM.Free;
  inherited Destroy;
end;


function TExpressionCalculator.Tokenize(const Expression: string): TArray<TToken>;
var
  Tokens: TList<TToken>;
  I: Integer;
  Ch: Char;
  NumberStr: string;
  Token: TToken;
begin
  Tokens := TList<TToken>.Create;
  try
    I := 1;
    while I <= Length(Expression) do
    begin
      Ch := Expression[I];

      // Skip whitespace
      if TCharacter.IsWhiteSpace (Ch) then
      begin
        Inc(I);
        Continue;
      end;

      // Parse numbers (including decimals)
      if Ch.IsDigit or (Ch = '.') then
      begin
        NumberStr := '';
        while (I <= Length(Expression)) and
              (IsDigit (Expression[I]) or (Expression[I] = '.')) do
        begin
          NumberStr := NumberStr + Expression[I];
          Inc(I);
        end;

        Token.TokenType := ttNumber;
        Token.Value := NumberStr;
        Token.NumValue := StrToFloat(NumberStr);
        Tokens.Add(Token);
        Continue;
      end;

      // Parse operators
      case Ch of
        '+', '-', '*', '/':
          begin
            Token.TokenType := ttOperator;
            Token.Value := Ch;
            Token.NumValue := 0;
            Tokens.Add(Token);
          end;
        '(':
          begin
            Token.TokenType := ttLeftParen;
            Token.Value := Ch;
            Token.NumValue := 0;
            Tokens.Add(Token);
          end;
        ')':
          begin
            Token.TokenType := ttRightParen;
            Token.Value := Ch;
            Token.NumValue := 0;
            Tokens.Add(Token);
          end;
      else
        raise Exception.CreateFmt('Invalid character: %s', [Ch]);
      end;

      Inc(I);
    end;

    // Add end token
    Token.TokenType := ttEnd;
    Token.Value := '';
    Token.NumValue := 0;
    Tokens.Add(Token);

    Result := Tokens.ToArray;
  finally
    Tokens.Free;
  end;
end;

function TExpressionCalculator.GetCurrentToken: TToken;
begin
  if FCurrentToken < Length(FTokens) then
    Result := FTokens[FCurrentToken]
  else
  begin
    Result.TokenType := ttEnd;
    Result.Value := '';
    Result.NumValue := 0;
  end;
end;

function TExpressionCalculator.ConsumeToken: TToken;
begin
  Result := GetCurrentToken;
  if FCurrentToken < Length(FTokens) - 1 then
    Inc(FCurrentToken);
end;

function TExpressionCalculator.PeekToken: TToken;
begin
  Result := GetCurrentToken;
end;

// Parse expression: term (('+' | '-') term)*
function TExpressionCalculator.ParseExpression: TValue;
var
  Left, Right: TValue;
  Op: string;
begin
  Left := ParseTerm;

  while (PeekToken.TokenType = ttOperator) and
        ((PeekToken.Value = '+') or (PeekToken.Value = '-')) do
  begin
    Op := ConsumeToken.Value;
    Right := ParseTerm;

    if Op = '+' then
      Left := FLLVM.FAdd(FModuleId, Left, Right)
    else
      Left := FLLVM.FSub(FModuleId, Left, Right);
  end;

  Result := Left;
end;

// Parse term: factor (('*' | '/') factor)*
function TExpressionCalculator.ParseTerm: TValue;
var
  Left, Right: TValue;
  Op: string;
begin
  Left := ParseFactor;

  while (PeekToken.TokenType = ttOperator) and
        ((PeekToken.Value = '*') or (PeekToken.Value = '/')) do
  begin
    Op := ConsumeToken.Value;
    Right := ParseFactor;

    if Op = '*' then
      Left := FLLVM.FMul(FModuleId, Left, Right)
    else
      Left := FLLVM.FDiv(FModuleId, Left, Right);
  end;

  Result := Left;
end;

// Parse factor: number | '(' expression ')'
function TExpressionCalculator.ParseFactor: TValue;
var
  Token: TToken;
  ExprResult: TValue;
begin
  Token := PeekToken;

  case Token.TokenType of
    ttNumber:
      begin
        ConsumeToken; // consume the number
        Result := FLLVM.FloatValue(FModuleId, Token.NumValue, dtFloat64);
      end;

    ttLeftParen:
      begin
        ConsumeToken; // consume '('
        ExprResult := ParseExpression;
        if PeekToken.TokenType = ttRightParen then
          ConsumeToken // consume ')'
        else
          raise Exception.Create('Expected closing parenthesis');
        Result := ExprResult;
      end;

  else
    raise Exception.Create('Expected number or opening parenthesis');
  end;
end;

function TExpressionCalculator.Calculate(const Expression: string): Double;
var
  ExprResult: TValue;
  CalcResult: TValue;
  TempUInt64: UInt64;
begin
  // Create a new module for this calculation
  FLLVM.CreateModule(FModuleId);

  try
    // Begin a function that will perform the calculation
    FLLVM.BeginFunction(FModuleId, 'calculate', dtFloat64, []);
    FLLVM.BeginBlock(FModuleId, 'entry');

    // Tokenize the expression
    FTokens := Tokenize(Expression);
    FCurrentToken := 0;

    // Parse and generate LLVM IR for the expression
    ExprResult := ParseExpression;

    // Return the result
    FLLVM.ReturnValue(FModuleId, ExprResult);
    FLLVM.EndBlock(FModuleId);
    FLLVM.EndFunction(FModuleId);

    // Validate and execute
    if not FLLVM.ValidateModule(FModuleId) then
      raise Exception.Create('Invalid LLVM module generated');

    // Execute the calculation function
    CalcResult := FLLVM.ExecuteFunction(FModuleId, 'calculate', []);

    // Extract the double result
    if CalcResult.IsType<Double> then
      Result := CalcResult.AsType<Double>
    else if CalcResult.IsType<UInt64> then
    begin
      // LLVM might return as UInt64, convert back to double
      TempUInt64 := CalcResult.AsType<UInt64>;
      Result := PDouble(@TempUInt64)^;
    end
    else
      raise Exception.Create('Unexpected return type from calculation');

  finally
    FLLVM.DeleteModule(FModuleId);
  end;
end;

// Main program
procedure RunCalculator;
var
  Calculator: TExpressionCalculator;
  Expression: string;
  Result: Double;
begin
  Calculator := TExpressionCalculator.Create;
  try
    WriteLn('LLVM Expression Calculator');
    WriteLn('Enter mathematical expressions (or "quit" to exit)');
    WriteLn('Examples: 2+3, 10*5/2, (3+4)*2, 2.5+1.7');
    WriteLn;

    while True do
    begin
      Write('> ');
      ReadLn(Expression);

      Expression := Trim(Expression);
      if (Expression = '') then
        Continue;

      if (LowerCase(Expression) = 'quit') or (LowerCase(Expression) = 'exit') then
        Break;

      try
        Result := Calculator.Calculate(Expression);
        WriteLn(Format('Result: %.6g', [Result]));
      except
        on E: Exception do
          WriteLn('Error: ' + E.Message);
      end;

      WriteLn;
    end;

  finally
    Calculator.Free;
  end;
end;

begin
  try
    WriteLn('libLLVM Version: ' + TLLVM.GetVersionStr);
    WriteLn('LLVM Version: ' + TLLVM.GetLLVMVersionStr);
    WriteLn;

    RunCalculator;

  except
    on E: Exception do
    begin
      WriteLn('Fatal error: ' + E.Message);
      ReadLn;
    end;
  end;
end.
