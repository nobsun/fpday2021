---
marp: true
style: |
    section {
        font-family: 'Migu 1C';
    }
    p br {
        display: none;
    }
    pre, code {
        font-family: 'HackGenNerd Console';
    }
    table {
        font-size: 16pt;
    }
math: katex
paginate: true

---
```haskell
module Toysim
    ( toysim
    , sampleCode
    ) where

import Data.Char
import Data.List
```

---
# 命令プログラムを関数プログラミングぽく書く

---
## 関数プログラミングとは

関数プログラミング
- 「プログラム＝関数」と考えてプログラミングするスタイル
- 「プログラム＝関数」を表現できる言語で思考するスタイル

---
## Haskellプログラミングは「関数プログラミング」か

- Haskellは「汎用の純粋関数型言語」（Haskell 2010 Language Report）
- Haskellは「最も美しい命令型言語」（S. Peyton Jones: Beautiful Concurrency）

---
## 関数プログラミングぽさは気分の問題

$$
$$
$$
\begin{array}{lll}
& \textbf{命令ぽい} & \textbf{関数ぽい} \\\\
\textbf{計算順序} & 出現順序 & データ依存順 \\\\
\textbf{思考方向} & ボトムアップ & トップダウン\\\\
\textbf{計算結果の伝搬} & 暗黙的、間接的 & 明示的、直接的\\\\
\textbf{データアクセス} & ランダム &シーケンシャル\\\\
\textbf{計算、入出力} & \text{Eager} & \text{Lazy}
\end{array}
$$

---
## どこまで「関数ぽく」書けるか

命令プログラミングに向いてそうなもの（個人的思い込み）
- 命令プログラムの実行シミュレーション

---
## カーニハンのトイ・マシン・シミュレーター

[Toy Machine Simulator](https://kernighan.com/toysim.html)

- アキュムレータ 1つ
- 命令とデータを格納するメモリ
- キーボード入力
- 端末表示


---
| |命令| |意味|
|-|-|-|-|
||`GET`||キーボードからアキュムレータに数値を読み込む|
||`PRINT`||アキュムレータの内容を出力欄に表示|
||`LOAD` |*Val*|アキュムレータに値（またはメモリ番地の内容）を設定|
||`STORE` |*M*|アキュムレータの内容の写しをメモリ番地*M*に格納|
||`ADD`|*Val*|アキュムレータの内容に値（またはメモリ番地の内容）を足し込む|
||`SUB`|*Val*|アキュムレータの内容から値（またはメモリ番地の内容）を引く|
||`GOTO`|*L*|ラベル*L*が付けられた命令に飛ぶ|
||`IFPOS`|*L*|もしアキュムレータの内容が$0$または正であればラベル*L*に飛ぶ|
||`IFZERO`|*L*|もしアキュムレータの内容がちょうど$0$であればラベル*L*に飛ぶ|
||`STOP`||実行を停止する|
|*M*|*Num*||プログラムを起動する前に、このメモリ位置（番地*M*）を数値*Num*に設定する|

---
トイ・マシンのプログラム例：数値入力の加算
```haskell
sampleCode :: String
sampleCode = unlines
    [ "Top GET"
    , "    IFZERO Bot"
    , "    ADD Sum"
    , "    STORE Sum"
    , "    GOTO Top"
    , "Bot LOAD Sum"
    , "    PRINT"
    , "    STOP"
    , "Sum 0"
    ]
```
---
## シミュレーターは入出力をともなうプログラム

```haskell
toysim :: IO ()
toysim = interact (wrap (toy code))

type Interaction = String -> String

wrap :: Toy -> Interaction
toy  :: String -> Toy
```
---
## トイ・マシンを実行をシミュレートする関数

トイ・マシンのシミュレートする関数`toy :: String -> [String] -> [String]`
- ソースコード
    - ソースプログラムの型 `String`
- `GET`命令と`PRINT`命令の実行をシミュレートする
    - 入力の型 `[String]`
    - 出力の型 `[String]`

```haskell
type Toy = [String] -> [String]
toy :: String -> Toy
toy code = filter (not . null) . map output
         . eval . initState (loadProg code)
    where
        output (_,_,_,out) = out
```

---
## トイ・マシンの実行
関数として考えるなら
- トイ・マシンの命令実行は、状態遷移関数(`ToyState -> ToyState`)の適用
- トイ・マシンのプログラム（命令列）の実行は、初期状態から最終状態にいたる状態遷移列の生成

```haskell
eval :: ToyState -> [ToyState]
eval state = state : followings
    where
        followings 
            | final state = []
            | otherwise   = eval (step state)
```

---
`step` は `fetch`、`decode`、`execute` の1サイクル分
```haskell
step :: ToyState -> ToyState
step state = execute (decode (fetch state)) state

fetch :: ToyState -> Code
decode :: Code -> Instruction
execute :: Instruction -> ToyState -> ToyState
```

---
プログラムローダー `loadProg :: String -> Memory` の実装
```haskell
loadProg :: String -> Memory
loadProg src = (,) . length <*> cycle . map conv $ lines $ map toUpper src
```

---
状態初期化関数 `initState`
```haskell
initState :: Memory -> [Input] -> ToyState
initState mem input = (mem, 0, input, "")
```

---
`ToyState` 実装
```haskell
type ToyState = (Memory, Accumulator, [Input], Output)

type Memory = (Int, [(Label, Content)])

type Input = String
type Accumulator = Int
type Output = String

type Instruction = ToyState -> ToyState
```

---
メモリ `[(Label, Content)]` の実装
```haskell
type Label = String

data Content
    = Code Code
    | Data Data

type Code = (Operator, Operand)

type Data = Int
```

---
`Operator` 実装
```haskell
data Operator
    = GET
    | PRINT
    | STOP
    | LOAD
    | STORE
    | ADD
    | SUB
    | GOTO
    | IFPOS
    | IFZERO
    deriving (Eq, Ord, Enum, Show, Read)
```

---
`Operand` 実装
```haskell
data Operand
    = None
    | Number Int
    | Label Label
    deriving (Eq, Show)
```

---
`fetch :: ToyState -> Code` の実装

```haskell
fetch ((_,mem),_,_,_) = case mem of
    (_,Code code):_ -> code
```

---
`execute :: Instruction -> ToyState -> ToyState`
```haskell
execute = id
```

---
`decode :: Code -> Instruction` の実装
```haskell
decode (ope, opd)
    = (case ope of
    GET    -> iGet
    PRINT  -> iPrint
    LOAD   -> iLoad
    STORE  -> iStore
    ADD    -> iAdd
    SUB    -> iSub
    GOTO   -> iGoto
    IFPOS  -> iIfpos
    IFZERO -> iIfzero
    STOP   -> iStop
    ) opd
```

---
`iGet, iPrint :: Operand -> Instruction`の実装
```haskell
iGet, iPrint :: Operand -> Instruction
iGet   _ ((sz,mem),acc,ins,_)
    = ((sz, tail mem), read (head ins), tail ins, "")

iPrint _ ((sz,mem),acc,ins,_) 
    = ((sz, tail mem), acc, ins, show acc)
```

---
`iLoad, iStore :: Operand -> Instruction`の実装
```haskell
iLoad, iStore :: Operand -> Instruction
iLoad  opd ((sz,mem),_,ins,_)
    = ((sz, tail mem), value (sz,mem) opd, ins, "")

iStore opd (m,acc,ins,_)
    = next (update opd acc m, acc, ins, "")
```

---
`iAdd, iSub :: Operand -> Instruction`の実装
```haskell
iAdd, iSub :: Operand -> Instruction
iAdd opd ((sz,mem),acc,ins,_)
    = ((sz, tail mem), acc + value (sz, mem) opd, ins, "")
    
iSub opd ((sz,mem),acc,ins,_)
    = ((sz, tail mem), acc - value (sz, mem) opd, ins, "")
```

---
`iGoto, iIfpos, iIfzero :: Operand -> Instruction`の実装
```haskell
iGoto, iIfpos, iIfzero :: Operand -> Instruction
iGoto (Label l) ((sz,mem),acc,ins,_) 
    = ((sz, mem'), acc, ins, "")
    where
        mem' = dropWhile ((l /=) . fst) mem

iIfpos lab@(Label _) st@(_,acc,_,_) = st'
    where
        st' = if acc >= 0 then iGoto lab st else next st

iIfzero lab@(Label l) st@(_,acc,_,_) = st'
    where
        st' = if acc == 0 then iGoto lab st else next st
```

---
`iStop :: Operand -> Instruction` の実装
```haskell
iStop :: Operand -> Instruction
iStop  _ ((_,mem),acc,ins,_)
    = ((0, mem), acc, ins, "\nstopped")
```
`STOP`の実行によって、メモリサイズを`0`に設定し、これを終了判定`final`で判定する
```haskell
final :: ToyState -> Bool
final st = case st of
    ((0,_),_,_,_) -> True
    _             -> False
```

---
補助関数 `next` の定義
```haskell
next :: ToyState -> ToyState
next ((sz,mem),acc,ins,_) = ((sz, tail mem), acc, ins, "")
```

---
補助関数 `update` の定義
```haskell
update :: Operand -> Data -> Memory -> Memory
update (Label m) val (sz,mem)
    = case break ((m ==) . fst) (take sz mem) of
          (xs, _:ys) -> (sz, cycle (xs ++ (m,Data val) : ys))
          _          -> error (m ++ ": not exist")
```

---
補助関数 `value` の定義
```haskell
value :: Memory -> Operand -> Data
value (sz,mem) opd
    = case opd of
          Number num -> num
          Label lab  -> case dropWhile ((lab /=) . fst) (take sz mem) of
              (_,Data d):_ -> d
              _            -> error (lab ++ ": not exist")
```

---
補助関数 `conv :: String -> (Label, Content)`
```haskell
conv :: String -> (Label, Content)
conv line = case line of
    c:cs -> if isSpace c 
        then ("", toContent (words line))
        else case words line of
            label:content -> (label, toContent content)
```

---
補助関数 `toContent :: [String] -> Content`
```haskell
toContent :: [String] -> Content
toContent ss = case ss of
    [cs] | all isDigit cs -> Data (read cs)
         | otherwise      -> Code (read cs, None)
    [ope,cs]              -> Code (read ope, opd)
        where
            opd = if all isDigit cs 
                then Number (read cs)
                else Label cs
```