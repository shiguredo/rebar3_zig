rebar3_zig
==========

[![License](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](https://opensource.org/licenses/Apache-2.0)

[Erlang] から [Zig] で書かれたコードを利用可能にするための [Rebar3] プラグインです。
内部では Erlang の [NIF] (Native Implemented Functions) という機能を利用しています。

[Erlang]: https://www.erlang.org/
[Zig]: https://ziglang.org/
[Rebar3]: https://github.com/erlang/rebar3
[NIF]: https://www.erlang.org/doc/tutorial/nif.html

## About Shiguredo's open source software

We will not respond to PRs or issues that have not been discussed on Discord. Also, Discord is only available in Japanese.

Please read https://github.com/shiguredo/oss/blob/master/README.en.md before use.

## 時雨堂のオープンソースソフトウェアについて

利用前 https://github.com/shiguredo/oss をお読みください。

## 使い方

基本的な使い方は以下の通りです:

```console
////////
// 1) 適当な rebar3 プロジェクトを作成
$ rebar3 new lib zig_sample
$ cd zig_sample/


////////
// 2) rebar3_zig をプロジェクトプラグインとして追加
$ echo "\n{project_plugins, [rebar3_zig]}." >> rebar.config


////////
// 3) rebar3 のテンプレート機能を使って Zig 用のファイル群を生成

// 自分の環境での NIF 用ヘッダのインクルードパスを指定
// （ここでは省略して後で C_INCLUDE_PATH 環境変数に指定するのでも可）
$ INCLUDE_PATH=/path/to/erlang/erts-${ ERTS_VERSION }/include/

// NIF 関数呼び出し用に生成される Erlang のモジュール名を指定
$ NIF_MODULE_NAME=sample

// ファイル生成
$ rebar3 new zig name=$NIF_MODULE_NAME include_path=$INCLUDE_PATH
===> Analyzing applications...
===> Compiling rebar3_zig
===> Writing zig_src/build.zig
===> Writing zig_src/nif.zig
===> Writing zig_src/main.zig
===> Writing src/sample.erl

$ cat zig_src/main.zig
const std = @import("std");

pub fn add(x: c_int, y: c_int) c_int {
    return x + y;
}

test "add" {
    try std.testing.expectEqual(add(1, 2), 3);
}


////////
// 4) コンパイルとテスト
$ rebar3 zig compile
===> Analyzing applications...
===> Compiling rebar3_zig
===> Verifying dependencies...
===> Analyzing applications...
===> Compiling zig_sample
===> Running zig compile...
===> Moving artifacts...
===> Moved: "zig_src/zig-out/lib/libsample.0.0.0.dylib" => "priv/libsample.so"

$ rebar3 zig test
===> Analyzing applications...
===> Compiling rebar3_zig
===> Running zig test...
All 1 tests passed.


////////
// 5) Erlang シェルからの NIF 関数呼び出し
$ rebar3 shell
> sample:add(1, 2).
3
```

また rebar.config ファイルに以下のフックを追加することで、
`$ rebar3 compile` や `$ rebar3 eunit` 実行時に自動で Zig のコードのコンパイルやテストが実行可能になります。

```erlang
{provider_hooks, [{post, [{compile, {zig, compile}},
                          {eunit, {zig, test}},
                          {clean, {zig, clean}}]}]}.
```

## ライセンス

[Apache License 2.0](https://www.apache.org/licenses/LICENSE-2.0)

```
Copyright 2022-2022, Takeru Ohta (Original Author)
Copyright 2022-2022, Shiguredo Inc.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
```
