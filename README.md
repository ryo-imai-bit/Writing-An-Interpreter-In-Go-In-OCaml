# InterpreterInGoInOCaml

[InterpreterInGo](https://www.oreilly.co.jp/books/9784873118222/)のOCaml実装

## 経過

- duneでlocalのlexerのlibararyが読み込めなかった問題。プロジェクトルートにdune関係なくビルドしたlexer.cmaとかが残ってたせいだった
- parseの相互再帰的部分がうまく細かい単位に分割できないのでリファクタ考える
