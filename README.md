# CRDT

[![hex.pm version](https://img.shields.io/hexpm/v/shiguredo_crdt.svg)](https://hex.pm/packages/shiguredo_crdt)
[![License](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](https://opensource.org/licenses/Apache-2.0)

CRDT (conflict-free replicated data types) の Erlang 実装です。

現在は [An optimized conflict-free replicated set](https://arxiv.org/abs/1210.3368) で説明されている
"Optimized OR-Set" のみが提供されています。

## rebar.conf

```erlang
{deps, [{crdt, "2022.1.0", {pkg, shiguredo_crdt}}]}.
```
## About Shiguredo's open source software

We will not respond to PRs or issues that have not been discussed on Discord. Also, Discord is only available in Japanese.

Please read https://github.com/shiguredo/oss/blob/master/README.en.md before use.

## 時雨堂のオープンソースソフトウェアについて

利用前に https://github.com/shiguredo/oss をお読みください。

## サポートについて

### Discord

- サポートしません
- アドバイスします
- フィードバック歓迎します

最新の状況などは Discord で共有しています。質問や相談も Discord でのみ受け付けています。

https://discord.gg/shiguredo

### バグ報告

Discord へお願いします。

## ライセンス

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
