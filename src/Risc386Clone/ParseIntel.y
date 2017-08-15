{
module Risc386Clone.ParseIntel  where

import Data.Int

import Risc386Clone.GenSym
import Risc386Clone.Frame -- Acc
import Risc386Clone.Intel
import Risc386Clone.TokenIntel hiding (Label, Temp, Reg)
import qualified Risc386Clone.TokenIntel as Tok
import Risc386Clone.LexIntel
import Risc386Clone.FrameIntel
}

%tokentype { (Token AlexPosn) }

%name parse

%token
        '['             { LBrack _ }
        ']'             { RBrack _ }
        ':'             { Colon _ }
        ','             { Comma _ }
        '+'             { Plus _ }
        '-'             { Minus _ }
        '*'             { Times _ }
        '0'             { Nat 0 _ }
        '1'             { Nat 1 _ }
        '2'             { Nat 2 _ }
        '4'             { Nat 4 _ }
        '8'             { Nat 8 _ }

        mov             { Mov _ }
        add             { Add _ }
        sub             { Sub _ }
        shl             { Shl _ }
        shr             { Shr _ }
        sal             { Sal _ }
        sar             { Sar _ }
        and             { And _ }
        or              { Or _ }
        xor             { Xor _ }
        neg             { Neg _ }
        not             { Not _ }
        inc             { Inc _ }
        dec             { Dec _ }
        pop             { Pop _ }
        push            { Push _ }
        imul            { Imul _ }
        idiv            { Idiv _ }
        jmp             { Jmp _ }
        lea             { Lea _ }
        cmp             { Cmp _ }
        cdq             { Cdq _ }
        je              { Je _  }
        jne             { Jne _ }
        jl              { Jl _ }
        jle             { Jle _ }
        jg              { Jg _ }
        jge             { Jge _ }
        call            { Call _ }
        ret             { Ret _ }
        enter           { Enter _ }
        leave           { Leave _ }
        nop             { Nop _ }

        dword           { Dword _ }
        ptr             { Ptr _ }

        nat             { Nat $$ _ }
        temp            { Tok.Temp $$ _ }
        reg             { Tok.Reg $$ _ }
        label           { Id $$ _ }

%%

-- a function starts with a label and ends with "ret"

Prg  :: { [IntelFrame] }
Prg  : {- empty -}        { [] }
     | Label IList         { [Frame $1 (mkFrameData []) $2] }
     | Label IList ret Prg { (Frame $1 (mkFrameData []) ($2 ++ [RET])) : $4 }

IList :: { [Instr] }
IList :                { [ ] }
      | Instr IList       { $1 : $2 }

Instr :: { Instr }
Instr : Label             { LABEL $1 }
      | DS Dest ',' Src   { DS $1 $2 $4 }
      | D  Dest           { D  $1 $2 }
      | lea Reg ',' dword ptr '[' Addr ']'  { LEA $2 $7 }
      | cmp Dest ',' Src  { CMP $2 $4 }
      | push Src          { PUSH $2 }
      | imul Reg ',' Src  { DS IMUL2 (Reg $2) $4 } -- Dest because no Imm
      | imul Dest         { IMUL $2 }     -- Dest because no Imm
      | idiv Dest         { IDIV $2 }     -- Dest because no Imm
      | jmp  label        { JMP  $2 }
      | CJmp label        { J $1 $2 }
      | call label        { CALL $2 }
      | enter Nat ',' '0' { ENTER $2 }
      | cdq               { CDQ }
      | leave             { LEAVE }
      | nop               { NOP }
--      | ret               { RET }

-- a Label ends in a colon

Label :: { Label }
Label : label ':'         { $1 }

-- instruction taking both destination and source

DS :: { DS }
DS    : mov               { MOV }
      | add               { ADD }
      | sub               { SUB }
      | shl               { SHL }
      | shr               { SHR }
      | sal               { SAL }
      | sar               { SAR }
      | and               { AND }
      | or                { OR  }
      | xor               { XOR }

-- instruction taking only destination

D :: { D }
D     : pop               { POP }
      | neg               { NEG }
      | not               { NOT }
      | inc               { INC }
      | dec               { DEC }

-- conditional jump

CJmp :: { Cond }
CJmp  : je                { E  }
      | jne               { NE }
      | jl                { L  }
      | jle               { LE }
      | jg                { G  }
      | jge               { GE }

-- operands

Src :: { Src }
Src   : Int               { Imm $1  }
      | Dest              { Dest $1 }

Dest :: { Dest }
Dest  : Reg               { Reg $1 }
      | dword ptr '[' Addr ']'  { ( \ (a,b,c,d) -> Mem a b c d) $4 }

Reg :: { Reg }
Reg   : reg               { Fixed $1 }
      | temp              { Flex (Temp $1) }

Addr :: { EA }
Addr  : Reg                             { (,,,) $1 Nothing Nothing 0 }

      | Reg '+' Int                     { (,,,) $1 Nothing Nothing $3 }
      | Reg '+' Reg                     { (,,,) $1 Nothing (Just $3) 0 }
      | Int '+' Reg                     { (,,,) $3 Nothing Nothing $1 }

      | Reg '+' Reg '+' Int             { (,,,) $1 Nothing (Just $3) $5 }
      | Reg '+' Int '+' Reg             { (,,,) $1 Nothing (Just $5) $3 }
      | Int '+' Reg '+' Reg             { (,,,) $3 Nothing (Just $5) $1 }

      | Reg '*' Scl                     { (,,,) $1 (Just $3) Nothing 0 }
      | Scl '*' Reg                     { (,,,) $3 (Just $1) Nothing 0 }

      | Reg '*' Scl '+' Int             { (,,,) $1 (Just $3) Nothing $5 }
      | Scl '*' Reg '+' Int             { (,,,) $3 (Just $1) Nothing $5 }
      | Reg '*' Scl '+' Reg             { (,,,) $1 (Just $3) (Just $5) 0 }
      | Scl '*' Reg '+' Reg             { (,,,) $3 (Just $1) (Just $5) 0 }

      | Int '+' Reg '*' Scl             { (,,,) $3 (Just $5) Nothing $1 }
      | Int '+' Scl '*' Reg             { (,,,) $5 (Just $3) Nothing $1 }
      | Reg '+' Reg '*' Scl             { (,,,) $3 (Just $5) (Just $1) 0 }
      | Reg '+' Scl '*' Reg             { (,,,) $5 (Just $3) (Just $1) 0 }

      | Reg '*' Scl '+' Int '+' Reg     { (,,,) $1 (Just $3) (Just $7) $5 }
      | Scl '*' Reg '+' Int '+' Reg     { (,,,) $3 (Just $1) (Just $7) $5 }
      | Reg '*' Scl '+' Reg '+' Int     { (,,,) $1 (Just $3) (Just $5) $7 }
      | Scl '*' Reg '+' Reg '+' Int     { (,,,) $3 (Just $1) (Just $5) $7 }

      | Int '+' Reg '*' Scl '+' Reg     { (,,,) $3 (Just $5) (Just $7) $1 }
      | Int '+' Scl '*' Reg '+' Reg     { (,,,) $5 (Just $3) (Just $7) $1 }
      | Reg '+' Reg '*' Scl '+' Int     { (,,,) $3 (Just $5) (Just $1) $7 }
      | Reg '+' Scl '*' Reg '+' Int     { (,,,) $5 (Just $3) (Just $1) $7 }

      | Reg '+' Int '+' Reg '*' Scl     { (,,,) $5 (Just $7) (Just $1) $3 }
      | Reg '+' Int '+' Scl '*' Reg     { (,,,) $7 (Just $5) (Just $1) $3 }
      | Int '+' Reg '+' Reg '*' Scl     { (,,,) $5 (Just $7) (Just $3) $1 }
      | Int '+' Reg '+' Scl '*' Reg     { (,,,) $7 (Just $5) (Just $3) $1 }

-- negative displacement

      | Reg '-' Int                     { (,,,) $1 Nothing Nothing (- $3) }

      | Reg '+' Reg '-' Int             { (,,,) $1 Nothing (Just $3) (- $5) }
      | Reg '-' Int '+' Reg             { (,,,) $1 Nothing (Just $5) (- $3) }

      | Reg '*' Scl '-' Int             { (,,,) $1 (Just $3) Nothing (- $5) }
      | Scl '*' Reg '-' Int             { (,,,) $3 (Just $1) Nothing (- $5) }

      | Reg '*' Scl '-' Int '+' Reg     { (,,,) $1 (Just $3) (Just $7) (- $5) }
      | Scl '*' Reg '-' Int '+' Reg     { (,,,) $3 (Just $1) (Just $7) (- $5) }
      | Reg '*' Scl '+' Reg '-' Int     { (,,,) $1 (Just $3) (Just $5) (- $7) }
      | Scl '*' Reg '+' Reg '-' Int     { (,,,) $3 (Just $1) (Just $5) (- $7) }

      | Reg '+' Reg '*' Scl '-' Int     { (,,,) $3 (Just $5) (Just $1) (- $7) }
      | Reg '+' Scl '*' Reg '-' Int     { (,,,) $5 (Just $3) (Just $1) (- $7) }

      | Reg '-' Int '+' Reg '*' Scl     { (,,,) $5 (Just $7) (Just $1) (- $3) }
      | Reg '-' Int '+' Scl '*' Reg     { (,,,) $7 (Just $5) (Just $1) (- $3) }


-- currently only support scaling by 4

Scl :: { Scale }
Scl   : '1'                         { S1 }
      | '2'                         { S2 }
      | '4'                         { S4 }
      | '8'                         { S8 }

Nat :: { Int32 }
Nat   : '0'                         { fromIntegral 0  }
      | '1'                         { fromIntegral 1  }
      | '2'                         { fromIntegral 2  }
      | '4'                         { fromIntegral 4  }
      | '8'                         { fromIntegral 8  }
      | nat                         { fromIntegral $1 }

Int :: { Int32 }
Int   : Nat                         { $1     }
      | '-' Nat                     { (- $2) }
{

happyError :: [Token AlexPosn] -> a
happyError tks = error ("Parse error at " ++ lcn ++ "\n")
        where
        lcn =   case tks of
                  [] -> "end of file"
                  (tk:_) -> "line " ++ show l ++ ", column " ++ show c ++ " (token " ++ filterPn (show tk) ++ ")"
                        where AlexPn _ l c = token_pos tk

}
